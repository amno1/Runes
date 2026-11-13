;;;; src/inflate.lisp — deflate (RFC 1951) and zlib (RFC 1950) decoding.
;;;;
;;;; WOFF 1.0 compresses each font table as a zlib stream; ZLIB-DECOMPRESS with
;;;; the table's origLength as :SIZE-HINT decodes one.  INFLATE decodes raw
;;;; deflate.
;;;;
;;;; Built from the Brotli decoder's parts: deflate uses the same LSB-first bit
;;;; order and the same canonical prefix codes, so the bit reader (reader.lisp),
;;;; the decode tables (huffman.lisp) and the back-reference copy (decoder.lisp)
;;;; are shared.  Errors are signalled as DEFLATE-ERROR.

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(in-package #:decompress)

;;; ---- tables (RFC 1951 sec. 3.2.5) ----------------------------------------

;;; Length symbols 257..285 and distance symbols 0..29 -> base<<5 | extra bits.
;;; The two unused symbols of each alphabet (286, 287; 30, 31) map to 0, which
;;; the decoder rejects.

(defglobal-table +length-tab+ u32vec
  (let ((base #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
        (extra #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
        (a (make-array 31 :element-type '(unsigned-byte 32) :initial-element 0)))
    (dotimes (i 29 a)
      (setf (aref a i) (logior (ash (svref base i) 5) (svref extra i))))))

(defglobal-table +distance-tab+ u32vec
  (let ((base #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049
                3073 4097 6145 8193 12289 16385 24577))
        (extra #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
        (a (make-array 32 :element-type '(unsigned-byte 32) :initial-element 0)))
    (dotimes (i 30 a)
      (setf (aref a i) (logior (ash (svref base i) 5) (svref extra i))))))

;;; Decode table entries, as libdeflate's (u32, built by BUILD-DEFLATE-TABLE):
;;;   literal   #x80000000 | byte<<16 | code length
;;;   length    base<<16 | code length<<8 | (code length + extra bits)
;;;   distance  base<<16 | code length<<8 | (code length + extra bits)
;;;   subtable  #xC000 | offset<<16 | subtable bits<<8 | root bits
;;;   end code  #xA000 | code length
;;;   invalid   #x8000
;;; The low byte is what the entry's code (and extra bits) takes from the bit
;;; buffer, and bit 5 of it is clear, so a shift by the entry itself is a shift
;;; by that count (SHRX and BZHI only look at the low bits of their count).
;;; The extra bits are in the buffer as it was before the entry's shift, above
;;; the code (the code length is at bit 8).  The code-length code (read before
;;; the other two) keeps sym<<5 | length entries.
(defconstant +length-class+ #x4000
  "Marks a map value that has a base and an extra-bit count (a bit that
neither a base, which starts at bit 16, nor the other map values use).")

(defglobal-table +literal-map+ u32vec
  (let ((m (make-array 288 :element-type '(unsigned-byte 32) :initial-element #x8000)))
    (dotimes (i 256) (setf (aref m i) (logior #x80000000 (ash i 16))))
    (setf (aref m 256) #xA000)
    (dotimes (i 29 m)
      (let ((e (aref +length-tab+ i)))
        (setf (aref m (+ 257 i))
              (logior +length-class+ (ash (ash e -5) 16) (logand e 31)))))))

(defglobal-table +distance-map+ u32vec
  (let ((m (make-array 32 :element-type '(unsigned-byte 32) :initial-element #x8000)))
    (dotimes (i 30 m)
      (let ((e (aref +distance-tab+ i)))
        (setf (aref m i) (logior +length-class+ (ash (ash e -5) 16) (logand e 31)))))))

(declaim (inline combine-entry))
(defun combine-entry (v l)
  "Table entry for map value V, code length L."
  (declare (type (unsigned-byte 32) v) (type (integer 0 15) l))
  (if (logtest v +length-class+)
      (+ (logxor v +length-class+) (* l 257))
      (+ v l)))

;;; Order of the code length code's lengths.
(defglobal-table +deflate-cl-order+ u8vec
  (coerce #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15) 'u8vec))

;;; ---- prefix codes ----------------------------------------------------------

;;; Root size of the literal/length decode tables.  Font data has many
;;; literal codes longer than 8 bits (17-22% of symbols with an 8-bit root),
;;; and each takes the second-level path; zlib uses 10 bits for the same
;;; reason.  Distance tables keep an 8-bit root.
(defconstant +lit-root-bits+ 10)

;;; ---- code lengths compared 64 at a time (AVX-512) --------------------------
;;;
;;; Table setup needs, for each code length L, the symbols that have it.  One
;;; AVX-512 compare of 64 code lengths against L gives a 64-bit mask with bit
;;; K set when symbol C+K has length L: DEFLATE-TABLE counts the bits, and
;;; BUILD-DEFLATE-TABLE reads them in order to sort the symbols.  The loads
;;; read whole 64-byte chunks, so up to 63 bytes past the last length; the
;;; length arrays have that room (LOADS-OVERRUN-P checks), and CHUNK-MASK
;;; clears the bits past N.

#+decompress-avx512
(progn
  ;; Macros used to generate loops:  inline functions measured 1-3% slower here
  (defmacro loads-overrun-p (lens start n)
    "Whether 64-byte loads of the N lengths at LENS[START...] would run past
the end of LENS (then the scalar code is used)."
    `(> (+ ,start (* 64 (ceiling ,n 64))) (length ,lens)))

  (defmacro chunk-mask (n c)
    "Bit K set for each symbol C+K below N: all 64 bits except in the last
chunk."
    `(if (>= (- ,n ,c) 64)
         #xFFFFFFFFFFFFFFFF
         (1- (ash 1 (- ,n ,c)))))

  (defmacro length-mask (lengths ls valid)
    "Bit K set when byte K of LENGTHS (64 code lengths) equals byte K of LS
(the length L in all 64 bytes, from (U8.64 L)), and bit K of VALID is set."
    `(logand (sb-simd-avx512bw:m8.64-value
              (sb-simd-avx512bw:two-arg-u8.64-mask= ,lengths ,ls))
             ,valid))

  (defmacro do-set-bits ((index mask) &body body)
    "Run BODY with INDEX bound to the position of each bit set in the 64-bit
MASK, lowest first."
    (let ((m (gensym "M")))
      `(let ((,m ,mask))
         (declare (type (unsigned-byte 64) ,m))
         (loop until (zerop ,m)
               do (let ((,index (1- (integer-length (logand ,m (ldb (byte 64 0) (- ,m)))))))
                    ,@body)
                  ;; clear the lowest set bit
                  (setf ,m (logand ,m (ldb (byte 64 0) (1- ,m)))))))))

(defun build-deflate-table (lens start n count root-bits kind scratch)
  "Decode table, in BUILD-TABLE's format, for the complete code given by the N
lengths at LENS[START...], COUNT[l] of them of length l.  libdeflate's method:
codewords are enumerated bit-reversed in canonical order, and the root is
filled by doubling (the table is treated as 2^len entries while codes of
length len are stored, one entry each, then copied onto its upper half), so
there are no strided stores.  KIND (:LITERAL, :DISTANCE or :CODE-LENGTH) picks
the map; literal/length entries are made with COMBINE-LENGTH.  The table is
built in place in SCRATCH (+TABLE-CAPACITY+ entries; libdeflate builds into
its decompressor object the same way); returns SCRATCH and the table's
length."
  (declare (type u8vec lens) (type index start n) (type counts count)
           (type (integer 5 11) root-bits) (type u32vec scratch)
           (optimize speed (debug 0) (safety 0)))
  (let ((offs (make-array 17 :element-type '(unsigned-byte 16) :initial-element 0))
        (sorted (make-array 288 :element-type '(unsigned-byte 16)))
        (cnt 0) (len 1) (codeword 0) (end 0) (i 0))
    (declare (dynamic-extent offs sorted)
             (type fixnum cnt end i) (type (integer 0 16) len)
             (type (unsigned-byte 16) codeword))
    ;; SORTED: the used symbols by length, then symbol (canonical order)
    (flet ((counting-sort ()
             ;; each symbol reads OFFS[l] right after the previous store to
             ;; it, a chain through memory (store forwarding) per symbol
             (loop for l from 1 to 15 do (setf (aref offs (1+ l)) (+ (aref offs l) (aref count l))))
             (dotimes (s n)
               (let ((l (aref lens (+ start s))))
                 (when (plusp l)
                   (setf (aref sorted (aref offs l)) s)
                   (incf (aref offs l)))))))
      (declare (inline counting-sort))
      #-decompress-avx512 (counting-sort)
      #+decompress-avx512
      ;; for each length L in turn, the symbols of length L in order, read
      ;; from the bits of LENGTH-MASK: no chain through memory
      (if (loads-overrun-p lens start n)
          (counting-sort)
          (sb-sys:with-pinned-objects (lens)
            (loop for l of-type (integer 1 16) from 1 to 15
                  with sap = (sb-sys:sap+ (sb-sys:vector-sap lens) start)
                  unless (zerop (aref count l))
                    do (loop for c of-type fixnum from 0 below n by 64
                             with ls = (sb-simd-avx512bw:u8.64 l)
                             do (do-set-bits (k (length-mask (sb-simd-avx512bw:u8.64-sap-ref sap c)
                                                             ls (chunk-mask n c)))
                                  (setf (aref sorted i) (+ c k))
                                  (incf i))))
            (setf i 0))))
    (macrolet ((entry (l)
                 `(let* ((s (aref sorted i))
                         (l ,l))
                    (incf i)
                    (case kind
                      (:literal (combine-entry (aref +literal-map+ s) l))
                      (:distance (combine-entry (aref +distance-map+ s) l))
                      (t (logior (ash s 5) l)))))
               (next-codeword (mask)
                 ;; the next canonical codeword, bit-reversed: set the
                 ;; highest 0 bit below MASK, clear the 1 bits above it
                 `(let ((bit (ash 1 (1- (integer-length (logxor codeword ,mask))))))
                    (setf codeword (logior (logand codeword (1- bit)) bit))))
               (done () `(return-from build-deflate-table (values scratch end))))
      (loop while (zerop (setf cnt (aref count len))) do (incf len))
      (setf end (ash 1 len))
      ;; codes of up to ROOT-BITS bits
      (loop while (<= len root-bits) do
        (loop
         (setf (aref scratch codeword) (entry len))
         (when (= codeword (1- end))           ; the last codeword (all 1s)
           (loop while (< end (ash 1 root-bits))
                 do (replace scratch scratch :start1 end :end2 end)
                    (setf end (ash end 1)))
           (done))
         (next-codeword (1- end))
         (when (zerop (decf cnt)) (return)))
        (loop
         (when (<= (incf len) root-bits)
           (replace scratch scratch :start1 end :end2 end)
           (setf end (ash end 1)))
         (unless (zerop (setf cnt (aref count len))) (return))))
      ;; longer codes: subtables, each sized to be filled exactly
      (let ((rmask (1- (ash 1 root-bits))) (prefix -1) (sstart 0))
        (declare (type fixnum prefix sstart))
        (setf end (ash 1 root-bits))
        (loop
         (unless (= (logand codeword rmask) prefix)
           (let ((sbits (- len root-bits))
                 (used cnt))
             (declare (type (integer 0 15) sbits)
                      (type fixnum used))
             (loop while (< used (ash 1 sbits))
                   do (incf sbits)
                      (setf used (+ (ash used 1) (aref count (+ root-bits sbits)))))
             (setf prefix (logand codeword rmask)
                   sstart end
                   (aref scratch prefix) (logior #xC000 (ash sstart 16) (ash sbits 8) root-bits)
                   end (+ sstart (ash 1 sbits)))))
         (loop for j of-type fixnum from (+ sstart (ash codeword (- root-bits))) below end
                 by (ash 1 (- len root-bits))
               with e = (entry (- len root-bits))
               do (setf (aref scratch j) e))
         (when (= codeword (1- (ash 1 len))) (done))
         (next-codeword (1- (ash 1 len)))
         (when (zerop (decf cnt))
           (loop (setf cnt (aref count (incf len)))
                 (when (plusp cnt) (return)))))))))

(defun deflate-table (lens start n kind root-bits tab)
  "Decode table for the code given by N lengths at LENS[START...].  KIND is
:LITERAL, :DISTANCE or :CODE-LENGTH; like zlib, an incomplete code is only
accepted as a single code of length 1 (literal and distance codes), and a
distance code may be empty (an error only if a distance is ever needed).
Built in place in TAB (see BUILD-DEFLATE-TABLE); returns TAB."
  (declare (type u8vec lens) (type index start n) (type u32vec tab))
  (let ((count (make-array 16 :element-type '(unsigned-byte 16) :initial-element 0))
        (m 0) (space 0) (first 0))
    (declare (dynamic-extent count)
             (type (integer 0 1024) m)
             (type fixnum space)
             (type index first))
    (flet ((count-scalar ()
             ;; COUNT[l]++ per symbol: a chain through memory, as in the
             ;; builder's counting sort
             (dotimes (i n)
               (let ((l (aref lens (+ start i))))
                 (when (plusp l)
                   (when (zerop m) (setf first i))
                   (incf m)
                   (incf (aref count l))
                   (incf space (ash 1 (- 15 l))))))))
      (declare (inline count-scalar))
      #-decompress-avx512 (count-scalar)
      #+decompress-avx512
      ;; COUNT[l] = the number of bits set in LENGTH-MASK for L
      (if (loads-overrun-p lens start n)
          (count-scalar)
          (sb-sys:with-pinned-objects (lens)
            (loop for c of-type fixnum from 0 below n by 64
                  with sap = (sb-sys:sap+ (sb-sys:vector-sap lens) start)
                  do (loop for l of-type (integer 1 16) from 1 to 15
                           with lengths = (sb-simd-avx512bw:u8.64-sap-ref sap c)
                           with valid of-type (unsigned-byte 64) = (chunk-mask n c)
                           do (incf (aref count l)
                                    (logcount (length-mask lengths
                                                           (sb-simd-avx512bw:u8.64 l)
                                                           valid)))))
            (loop for l from 1 to 15
                  do (incf m (aref count l))
                     (incf space (ash (aref count l) (- 15 l))))
            ;; FIRST only matters for a code of one symbol
            (when (= m 1)
              (setf first (position-if #'plusp lens :start start :end (+ start n)))
              (decf first start)))))
    (cond
      ((> space 32768) (err "oversubscribed prefix code"))
      ((= space 32768)
       (values (build-deflate-table lens start n count root-bits kind tab)))
      ;; incomplete codes zlib accepts: one code of length 1 (its other bit
      ;; pattern is invalid), and an empty distance code
      ((or (and (= m 1) (= (aref lens (+ start first)) 1) (not (eq kind :code-length)))
           (and (= m 0) (eq kind :distance)))
       (progn
         (fill tab #x8000 :end (ash 1 root-bits))
         (when (= m 1)
           (let ((v (if (eq kind :literal)
                        (aref +literal-map+ first)
                        (aref +distance-map+ first))))
             (loop for j from 0 below (ash 1 root-bits) by 2
                   do (setf (aref tab j) (combine-entry v 1)))))
         tab))
      (t (err "incomplete prefix code")))))

;;; ---- reusable table storage ------------------------------------------------
;;;
;;; As libdeflate's decompressor object: the dynamic blocks' decode tables are
;;; built in place in arrays that are reused across blocks and streams, so a
;;; block allocates, zeroes and copies nothing for its tables.  A stream takes
;;; one set from a lock-free pool and returns it at the end (a stream that
;;; signals an error just drops its set), so concurrent threads never share one.

(defconstant +table-capacity+ #.(+ 2048 32768)
  "Entries a table under construction may use: a root of up to 2^11 entries
plus its subtables, which a complete code (Kraft) keeps within 2^15.")

(defstruct (inflate-tables (:constructor make-inflate-tables ()) (:conc-name tables-))
  (lit (make-array +table-capacity+ :element-type '(unsigned-byte 32)) :type u32vec)
  (dist (make-array +table-capacity+ :element-type '(unsigned-byte 32)) :type u32vec)
  ;; code-length codes are at most 7 bits, under an 8-bit root: no subtables
  (cl (make-array 256 :element-type '(unsigned-byte 32)) :type u32vec))

(sb-ext:defglobal **table-pool** nil)

(defun take-tables ()
  (loop (let ((head **table-pool**))
          (when (null head) (return (make-inflate-tables)))
          (when (eq (sb-ext:compare-and-swap (symbol-value '**table-pool**) head (cdr head)) head)
            (return (car head))))))

(defun return-tables (tables)
  (loop (let ((head **table-pool**))
          (when (eq (sb-ext:compare-and-swap (symbol-value '**table-pool**) head (cons tables head))
                    head)
            (return)))))

(sb-ext:define-load-time-global +fixed-tables+
  (let ((lens (make-array 320 :element-type '(unsigned-byte 8))))
    (fill lens 8 :start 0 :end 144)
    (fill lens 9 :start 144 :end 256)
    (fill lens 7 :start 256 :end 280)
    (fill lens 8 :start 280 :end 288)
    (fill lens 5 :start 288 :end 320)
    ;; the root is the whole table (no fixed code is longer than 9 bits)
    (flet ((build (start n kind root-bits)
             (subseq (deflate-table lens start n kind root-bits
                                    (make-array +table-capacity+ :element-type '(unsigned-byte 32)))
                     0 (ash 1 root-bits))))
      (cons (build 0 288 :literal +lit-root-bits+)
            (build 288 32 :distance 8))))
  "The fixed literal/length and distance codes of block type 1.")

(defun read-dynamic-tables (r tables)
  "Block type 2 header: returns the literal/length and distance decode tables,
built in TABLES' arrays."
  (declare (type reader r) (type inflate-tables tables))
  (let* ((hlit (+ 257 (read-bits r 5)))
         (hdist (+ 1 (read-bits r 5)))
         (hclen (+ 4 (read-bits r 4)))
         (cl-lens (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)) ; room for 64-byte loads
         (order +deflate-cl-order+))
    (declare (dynamic-extent cl-lens))
    (check-error (or (> hlit 286) (> hdist 30)) "too many length or distance codes")
    (dotimes (i hclen)
      (setf (aref cl-lens (aref order i)) (read-bits r 3)))
    (let ((cl-table (deflate-table cl-lens 0 19 :code-length 8 (tables-cl tables)))
          (lens (make-array 320 :element-type '(unsigned-byte 8) :initial-element 0))
          (total (+ hlit hdist))
          (i 0))
      (declare (dynamic-extent lens) (type (integer 0 320) total i))
      ;; the reader's state in locals, as in INFLATE-BLOCK; one refill per
      ;; code length covers its code (<= 7 bits) and repeat bits (<= 7)
      (let ((in (reader-in r)) (limit (reader-limit r))
            (val (reader-val r)) (bits (reader-bits r)) (pos (reader-pos r)))
        (declare (type u8vec in) (type index limit)
                 (type (unsigned-byte 64) val) (type sb-ext:word pos bits))
        (loop while (< i total) do
          (br-need 14)
          (let ((sym (br-sym cl-table (progn))))
            (declare (type (unsigned-byte 27) sym))
            (cond
              ((< sym 16) (setf (aref lens i) sym) (incf i))
              ((> sym 18) (err "invalid code length code"))
              (t (multiple-value-bind (value n)
                     (case sym
                       (16 (check-error (zerop i) "repeat with no previous length")
                           (values (aref lens (1- i)) (+ 3 (br-bits-no-refill 2))))
                       (17 (values 0 (+ 3 (br-bits-no-refill 3))))
                       (t (values 0 (+ 11 (br-bits-no-refill 7)))))
                   (check-error (> (+ i n) total) "code lengths repeat past the end")
                   ;; LENS starts zeroed, so runs of zeros (17, 18) are only
                   ;; skipped; a repeat (16) is 3-6 bytes, stored inline
                   (unless (zerop value)
                     (loop for j of-type (integer 0 320) from i below (+ i n)
                           do (setf (aref lens j) value)))
                   (incf i n))))))
        (setf (reader-val r) val (reader-bits r) bits (reader-pos r) pos))
      (check-error (zerop (aref lens 256)) "no end-of-block code")
      ;; the distance lengths sit after the literal ones: copy them down to a
      ;; zero-padded 32-entry area so the table code sees one alphabet
      (let ((dlens (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))) ; as CL-LENS
        (declare (dynamic-extent dlens))
        (replace dlens lens :start2 hlit :end2 total)
        (values (deflate-table lens 0 hlit :literal +lit-root-bits+ (tables-lit tables))
                (deflate-table dlens 0 32 :distance 8 (tables-dist tables)))))))

;;; ---- the block loop -------------------------------------------------------

(defmacro copy-match (sap fp dist len)
  "COPY-BACK-REFERENCE through the output's SAP alone, so the block loop
needs no register for the output vector.  Same classes and overshoot."
  `(let ((%src (- ,fp ,dist)))
     (declare (type index %src))
     (flet ((words16 ()
              (loop for %i of-type index from 0 below ,len by 16
                    do (setf (sb-sys:sap-ref-64 ,sap (+ ,fp %i)) (sb-sys:sap-ref-64 ,sap (+ %src %i))
                             (sb-sys:sap-ref-64 ,sap (+ ,fp %i 8)) (sb-sys:sap-ref-64 ,sap (+ %src %i 8))))))
       (declare (inline words16))
       (cond
         ((and (<= ,len 16) (>= ,dist ,len))
          (let ((%a (sb-sys:sap-ref-64 ,sap %src)) (%b (sb-sys:sap-ref-64 ,sap (+ %src 8))))
            (setf (sb-sys:sap-ref-64 ,sap ,fp) %a
                  (sb-sys:sap-ref-64 ,sap (+ ,fp 8)) %b)))
         ((>= ,dist ,len)
          #+decompress-avx512
          (if (>= ,dist 64)
              (loop for %i of-type index from 0 below ,len by 64
                    do (setf (sb-simd-avx512bw:u8.64-sap-ref ,sap (+ ,fp %i))
                             (sb-simd-avx512bw:u8.64-sap-ref ,sap (+ %src %i))))
              (words16))
          #-decompress-avx512
          (words16))
         ((>= ,dist 8) (words16))
         (t
          ;; as libdeflate: overlapping 8-byte stores, each advancing DIST
          ;; bytes; each store's first DIST bytes are right, the rest is
          ;; rewritten by the next (may overshoot by up to 7 bytes)
          (loop for %i of-type index from 0 below ,len by ,dist
                do (setf (sb-sys:sap-ref-64 ,sap (+ ,fp %i))
                         (sb-sys:sap-ref-64 ,sap (+ %src %i)))))))
     (incf ,fp ,len)))

(defun inflate-block (r buf fp ltab dtab)
  "Decode one compressed block's symbols into BUF from FP.  Returns the new FP
and :END at the end-of-block code, or :FULL when BUF needs to grow first (the
loop stops before a symbol whenever fewer than 258 bytes + slack are left)."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type reader r) (type u8vec buf) (type index fp) (type u32vec ltab dtab))
  (let* ((in (reader-in r))
         (limit (reader-limit r))
         (val (reader-val r))
         (bits (reader-bits r))
         (pos (reader-pos r))
         (cap (- (length buf) 258 +out-slack+))
         (status :end)
         (error nil))
    (declare (type u8vec in) (type index limit cap)
             (type (unsigned-byte 64) val) (type sb-ext:word pos bits))
    ;; No calls inside the loop: SBCL gives every value that lives across a
    ;; call site a stack home, so the error exits (never taken on valid input)
    ;; would push the loop's state to the stack. This local ERR has the same
    ;; name as the global one on purpose: the checks here and in the macros
    ;; (BR-REFILL, CHECK-END) expand to (ERR ...) and so reach this one, which
    ;; records the message and leaves the loop without a call.  The error is
    ;; signalled after the loop.
    (block decode
    (flet ((err (message)
             (setf error message)
             (return-from decode)))
      (declare (inline err))
    (sb-sys:with-pinned-objects (buf)
      (let ((bs (sb-sys:vector-sap buf)))
        (let ((e 0) (saved 0))
          (declare (type (unsigned-byte 32) e) (type (unsigned-byte 64) saved))
          ;; SAVED is VAL before the entry's shift: a length entry's shift
          ;; also drops its extra bits, which are read from SAVED
          (macrolet ((finish-symbol (entry)
                       `(progn
                          (setf e ,entry)
                          (when (logtest e #x4000)      ; a subtable link
                            (br-drop (logand e 255))
                            (setf e (aref ltab (+ (ash e -16)
                                                  (low-bits val (ldb (byte 6 8) e))))))
                          (setf saved val)
                          (br-drop (logand e 255))))
                     (next-symbol ()
                       `(finish-symbol (aref ltab (ldb (byte #.+lit-root-bits+ 0) val))))
                     (literal-or-leave ()
                       ;; store a literal, or go handle the end code / a match
                       `(if (logtest e #x80000000)
                            (progn (setf (sb-sys:sap-ref-8 bs fp) (ldb (byte 8 16) e))
                                   (incf fp))
                            (go other))))
            (tagbody
               ;; the loop as one VOP while the input has 16 bytes left; it
               ;; stops at the top of an iteration, so this loop can go on
               #+decompress-avx512
               (when (>= limit 16)
                 (multiple-value-bind (v b p f st)
                     (inflate-fast val bits pos fp in buf ltab dtab (- limit 16) cap)
                   (setf val v bits b pos p fp f)
                   (case st
                     (0 (go done))
                     (1 (setf status :full) (go done))
                     (3 (err "invalid length code"))
                     (4 (err "invalid distance code"))
                     (5 (err "distance too far back")))))
               ;; the early root lookup below needs >= 10 valid bits on entry too
               (br-refill)
             top
               (when (> fp cap) (setf status :full) (go done))
               ;; a refill leaves >= 56 bits, enough for three literal codes
               ;; (<= 15 bits each) without refilling in between, as zlib-ng's
               ;; fast loop does; the room check above allows the two extra
               ;; bytes (the output slack covers them).  The first symbol's
               ;; root entry is looked up before the refill (>= 10 bits are
               ;; always left here), so the refill's load overlaps it.
               (let ((root (aref ltab (ldb (byte #.+lit-root-bits+ 0) val))))
                 (br-refill)
                 (finish-symbol root))
               (literal-or-leave)
               (next-symbol) (literal-or-leave)
               (next-symbol) (literal-or-leave)
               (go top)
             other
               (when (logtest e #x8000)         ; the end code, or invalid
                 (if (logtest e #x2000)
                     (go done)
                     (err "invalid length code")))
               ;; a match needs up to 28 more bits (15 distance code, 13
               ;; distance extra; the length's extra bits are consumed
               ;; already), and the next iteration's early root lookup 10
               ;; after that
               (br-need 38)
               (let* ((len (+ (ash e -16)
                              (ash (low-bits saved (logand e 255))
                                   (- (ldb (byte 4 8) e)))))
                      (d (aref dtab (ldb (byte 8 0) val))))
                 (declare (type index len) (type (unsigned-byte 32) d))
                 (when (logtest d #x8000)       ; a subtable link, or invalid
                   (unless (logtest d #x4000) (err "invalid distance code"))
                   (br-drop 8)
                   (setf d (aref dtab (+ (ash d -16) (low-bits val (ldb (byte 6 8) d)))))
                   (when (logtest d #x8000) (err "invalid distance code")))
                 (let* ((dsaved val)
                        (dtotal (logand d 255))
                        (dist (+ (ash d -16)
                                 (ash (low-bits dsaved dtotal) (- (ldb (byte 4 8) d))))))
                   (declare (type index dist))
                   (br-drop dtotal)
                   (check-error (> dist fp) "distance too far back")
                   (copy-match bs fp dist len)))
               (go top)
             done)))))))
    (when error (err error))
    (setf (reader-val r) val
          (reader-bits r) bits
          (reader-pos r) pos)
    (values fp status)))

;;; ---- streams -----------------------------------------------------------------

(defun align-any (r)
  "Drop the bits up to the next byte boundary (deflate ignores their value)."
  (with-reader (r) (br-drop (logand bits 7))))

(defun inflate-into (r buf fp tables)
  "Decode deflate blocks from R into BUF at FP until the final block.  Returns
the (possibly regrown) BUF and the new FP."
  (declare (type reader r) (type u8vec buf) (type index fp))
  (loop
    (let ((final (read-bit r))
          (type (read-bits r 2)))
      (case type
        (0 (align-any r)
           (let* ((p (byte-pos r))
                  (len (progn
                         (check-end (+ p 4) (reader-limit r))
                         (logior (aref (reader-in r) p)
                                 (ash (aref (reader-in r) (+ p 1)) 8))))
                  (nlen (logior (aref (reader-in r) (+ p 2))
                                (ash (aref (reader-in r) (+ p 3)) 8))))
             (check-valid (= nlen (logxor len #xffff))
               "stored block length check failed")
             (check-end (+ p 4 len) (reader-limit r))
             (setf buf (grow buf fp len))
             (replace buf (reader-in r) :start1 fp :start2 (+ p 4) :end2 (+ p 4 len))
             (incf fp len)
             (seek r (+ p 4 len))))
        ((1 2)
         (multiple-value-bind (ltab dtab)
             (if (= type 1)
                 (values (car +fixed-tables+) (cdr +fixed-tables+))
                 (read-dynamic-tables r tables))
           (loop
             (multiple-value-bind (new-fp status) (inflate-block r buf fp ltab dtab)
               (setf fp new-fp)
               (if (eq status :end)
                   (return)
                   (setf buf (grow buf fp (max 4096 (length buf)))))))))
        (t (err "invalid block type")))
      (when (= final 1) (return (values buf fp))))))

(defun %inflate (in size-hint zlib start end)
  (let* ((*error-type* 'deflate-error)
         (in (if (typep in 'u8vec) in (coerce in 'u8vec)))
         (len (bounded-end in start end))
         (r (make-reader in :start start :end len))
         (tail (reader-tail r)))
    (sb-sys:with-pinned-objects (in tail)
      (when zlib
        (check-end (+ start 2) len)
        (let ((cmf (aref in start)) (flg (aref in (1+ start))))
          (check-valid (zerop (mod (+ (* 256 cmf) flg) 31)) "header check failed")
          (check-valid (= (logand cmf 15) 8) "unknown compression method")
          (check-error (> (ash cmf -4) 7) "invalid window size")
          (check-error (logbitp 5 flg) "preset dictionary required")
          (seek r (+ start 2))))
      (multiple-value-bind (buf fp)
          ;; + 258: the block loop wants room for a whole match before each
          ;; symbol, so an exact SIZE-HINT alone would make it regrow at the end
          (let ((tables (take-tables)))
            (multiple-value-prog1
                (inflate-into r (make-array (+ (or size-hint (max 4096 (* 4 (- len start)))) 258 +out-slack+)
                                            :element-type '(unsigned-byte 8))
                              0 tables)
              (return-tables tables)))
        (declare (type u8vec buf)
                 (type index fp))
        (check-end (- (* 8 (reader-pos r)) (reader-bits r)) (* 8 len))
        (when zlib
          (align-any r)
          (let ((p (byte-pos r)))
            (check-end (+ p 4) len)
            (check-valid (= (adler32 buf fp)
                       (logior (ash (aref in p) 24) (ash (aref in (+ p 1)) 16)
                               (ash (aref in (+ p 2)) 8) (aref in (+ p 3))))
              "Adler-32 check failed")))
        (sb-kernel:%shrink-vector buf fp)))))

(defun inflate (input &key size-hint (start 0) end)
  "Decode raw deflate data INPUT (octets, RFC 1951); return the decoded octets.
START and END bound the data within INPUT, which is read in place.
SIZE-HINT, the decoded size if known, lets the output be allocated once.
Signals DEFLATE-ERROR on malformed input."
  (%inflate input size-hint nil start end))

(defun zlib-decompress (input &key size-hint (start 0) end)
  "Decode the zlib stream INPUT (octets, RFC 1950: header, deflate data,
Adler-32); return the decoded octets.  START and END bound the stream within
INPUT, which is read in place.  For a WOFF 1.0 table pass its
origLength as SIZE-HINT.  Signals DEFLATE-ERROR on malformed input."
  (%inflate input size-hint t start end))

;;; ---- Adler-32 (RFC 1950 sec. 9) ------------------------------------------
;;;
;;; a = 1 + sum of the bytes, b = sum of the a's after each byte, both mod
;;; 65521.  With AVX-512, 64 bytes per step: VPSADBW adds the bytes into a,
;;; VPMADDUBSW with weights 64..1 (then VPMADDWD with ones) gives their
;;; position-weighted sum for b, and a third accumulator collects the a of the
;;; blocks before, each of which adds 64 times itself to b.  The sums are
;;; reduced every 64 KB, before any 32-bit lane can overflow.

#+decompress-avx512
(defglobal-table +adler-weights+ (simple-array (signed-byte 8) (64))
  (let ((w (make-array 64 :element-type '(signed-byte 8))))
    (dotimes (i 64 w) (setf (aref w i) (- 64 i)))))

#+decompress-avx2
(defglobal-table +adler-weights+ (simple-array (signed-byte 8) (32))
  (let ((w (make-array 32 :element-type '(signed-byte 8))))
    (dotimes (i 32 w) (setf (aref w i) (- 32 i)))))

(defun adler32 (buf n)
  "Adler-32 of the first N bytes of BUF."
  (declare (type u8vec buf) (type index n) (optimize (speed 3) (safety 0)))
  (let ((a 1) (b 0) (i 0))
    (declare (type (unsigned-byte 62) a b)
             (type index i))
    #+decompress-avx512
    (sb-sys:with-pinned-objects (buf)
      (let ((sap (sb-sys:vector-sap buf))
            (w (sb-simd-avx512bw:s8.64-aref +adler-weights+ 0))
            (ones (sb-simd-avx512bw:s16.32 1))
            (zero (sb-simd-avx512bw:u8.64 0)))
        (declare (type sb-simd-avx512bw:s8.64 w)
                 (type sb-simd-avx512bw:s16.32 ones)
                 (type sb-simd-avx512bw:u8.64 zero))
        (loop while (<= (+ i 64) n) do
          (let ((blocks (min 1024 (floor (- n i) 64)))
                (s1 (sb-simd-avx512f:u64.8 0))     ; byte sums
                (ps (sb-simd-avx512f:u64.8 0))     ; byte sums of the blocks before
                (s2 (sb-simd-avx512f:s32.16 0)))   ; weighted sums
            (declare (type (integer 1 1024) blocks)
                     (type sb-simd-avx512f:u64.8 s1 ps)
                     (type sb-simd-avx512f:s32.16 s2))
            (dotimes (k blocks)
              (let ((x (sb-simd-avx512bw:u8.64-sap-ref sap i)))
                (declare (type sb-simd-avx512bw:u8.64 x))
                (setf ps (sb-simd-avx512f:u64.8+ ps s1)
                      s1 (sb-simd-avx512f:u64.8+ s1 (sb-simd-avx512bw:u8.64-sad x zero))
                      ;; VPMADDWD's result is sixteen 32-bit sums; sb-simd types
                      ;; it s16.32, so it is reinterpreted (no instruction)
                      s2 (sb-simd-avx512f:s32.16+
                          s2 (sb-simd-avx512f:s32.16!
                              (sb-simd-avx512bw:s16.32-madd (sb-simd-avx512bw:s16.32-maddubs x w) ones))))
                (incf i 64)))
            (let ((sum1 (multiple-value-call #'+ (sb-simd-avx512f:u64.8-values s1)))
                  (sump (multiple-value-call #'+ (sb-simd-avx512f:u64.8-values ps)))
                  (sum2 (multiple-value-call #'+ (sb-simd-avx512f:s32.16-values s2))))
              (setf b (mod (+ b (* 64 blocks a) (* 64 sump) sum2) 65521)
                    a (mod (+ a sum1) 65521)))))))
    ;; the same with AVX2, 32 bytes per step (libdeflate's adler32_avx2)
    #+decompress-avx2
    (sb-sys:with-pinned-objects (buf)
      (let ((sap (sb-sys:vector-sap buf))
            (w (sb-simd-avx:s8.32-aref +adler-weights+ 0))
            (ones (sb-simd-avx:s16.16 1))
            (zero (sb-simd-avx:u8.32 0)))
        (declare (type sb-simd-avx:s8.32 w)
                 (type sb-simd-avx:s16.16 ones)
                 (type sb-simd-avx:u8.32 zero))
        (loop while (<= (+ i 32) n) do
          (let ((blocks (min 2048 (floor (- n i) 32)))
                (s1 (sb-simd-avx:u64.4 0))
                (ps (sb-simd-avx:u64.4 0))
                (s2 (sb-simd-avx:s32.8 0)))
            (declare (type (integer 1 2048) blocks)
                     (type sb-simd-avx:u64.4 s1 ps)
                     (type sb-simd-avx:s32.8 s2))
            (dotimes (k blocks)
              (let ((x (sb-simd-avx:u8.32-sap-ref sap i)))
                (declare (type sb-simd-avx:u8.32 x))
                (setf ps (sb-simd-avx2:u64.4+ ps s1)
                      s1 (sb-simd-avx2:u64.4+ s1 (sb-simd-avx2:u8.32-sad x zero))
                      s2 (sb-simd-avx2:s32.8+
                          s2 (sb-simd-avx:s32.8!
                              (sb-simd-avx2:s16.16-madd (sb-simd-avx2:s16.16-maddubs x w) ones))))
                (incf i 32)))
            (let ((sum1 (multiple-value-call #'+ (sb-simd-avx:u64.4-values s1)))
                  (sump (multiple-value-call #'+ (sb-simd-avx:u64.4-values ps)))
                  (sum2 (multiple-value-call #'+ (sb-simd-avx:s32.8-values s2))))
              (setf b (mod (+ b (* 32 blocks a) (* 32 sump) sum2) 65521)
                    a (mod (+ a sum1) 65521)))))))
    ;; the rest (all of it without simd), in chunks small enough that the
    ;; sums stay fixnums
    (loop while (< i n) do
      (let ((end (min n (+ i 5552))))
        (loop while (< i end) do
          (incf a (aref buf i))
          (incf b a)
          (incf i))
        (setf a (mod a 65521) b (mod b 65521))))
    (logior (ash b 16) a)))
