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

;;; What the decode tables hold per symbol (BUILD-TABLE's MAP): literals and
;;; the end code themselves; for length and distance codes +MATCH-FLAG+ | base<<4
;;; | extra bits, so a match needs no second lookup; unused symbols map to
;;; +INVALID-SYMBOL+ (below +MATCH-FLAG+, so one comparison rejects them).
(defconstant +match-flag+ #x100000)
(defconstant +invalid-symbol+ 1023
  "Symbol of decode-table entries that no valid stream can reach.")

(defglobal-table +literal-map+ u32vec
  (let ((m (make-array 288 :element-type '(unsigned-byte 32) :initial-element +invalid-symbol+)))
    (dotimes (i 257) (setf (aref m i) i))
    (dotimes (i 29 m)
      (let ((e (aref +length-tab+ i)))
        (setf (aref m (+ 257 i)) (logior +match-flag+ (ash (ash e -5) 4) (logand e 31)))))))

(defglobal-table +distance-map+ u32vec
  (let ((m (make-array 32 :element-type '(unsigned-byte 32) :initial-element +invalid-symbol+)))
    (dotimes (i 30 m)
      (let ((e (aref +distance-tab+ i)))
        (setf (aref m i) (logior +match-flag+ (ash (ash e -5) 4) (logand e 31)))))))

;;; Order of the code length code's lengths.
(defglobal-table +deflate-cl-order+ u8vec
  (coerce #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15) 'u8vec))

;;; ---- prefix codes ----------------------------------------------------------

;;; Root size of the literal/length decode tables.  Font data has many
;;; literal codes longer than 8 bits (17-22% of symbols with an 8-bit root),
;;; and each takes the second-level path; zlib uses 10 bits for the same
;;; reason.  Distance tables keep an 8-bit root.
(defconstant +lit-root-bits+ 10)

(defun deflate-table (lens start n kind &optional (root-bits 8))
  "Decode table for the code given by N lengths at LENS[START...].  KIND is
:LITERAL, :DISTANCE or :CODE-LENGTH; like zlib, an incomplete code is only
accepted as a single code of length 1 (literal and distance codes), and a
distance code may be empty (an error only if a distance is ever needed)."
  (declare (type u8vec lens) (type index start n))
  (with-code-arrays (syms lens* count n)
    (let ((m 0) (space 0))
      (declare (type (integer 0 1024) m) (type fixnum space))
      (dotimes (i n)
        (let ((l (aref lens (+ start i))))
          (when (plusp l)
            (setf (aref syms m) i (aref lens* m) l)
            (incf m)
            (incf (aref count l))
            (incf space (ash 1 (- 15 l))))))
      (cond
        ((> space 32768) (berr "oversubscribed prefix code"))
        ((= space 32768)
         (build-table syms lens* m count root-bits
                      (case kind (:literal +literal-map+) (:distance +distance-map+))))
        ;; incomplete codes zlib accepts: one code of length 1 (its other bit
        ;; pattern is invalid), and an empty distance code
        ((or (and (= m 1) (= (aref lens* 0) 1) (not (eq kind :code-length)))
             (and (= m 0) (eq kind :distance)))
         (let ((tab (make-array (ash 1 root-bits) :element-type '(unsigned-byte 32)
                                                  :initial-element (logior (ash +invalid-symbol+ 5) 1))))
           (when (= m 1)
             (let ((v (case kind
                        (:literal (aref +literal-map+ (aref syms 0)))
                        (:distance (aref +distance-map+ (aref syms 0)))
                        (t (aref syms 0)))))
               (loop for j from 0 below (ash 1 root-bits) by 2
                     do (setf (aref tab j) (logior (ash v 5) 1)))))
           tab))
        (t (berr "incomplete prefix code"))))))

(sb-ext:define-load-time-global +fixed-tables+
  (let ((lens (make-array 320 :element-type '(unsigned-byte 8))))
    (fill lens 8 :start 0 :end 144)
    (fill lens 9 :start 144 :end 256)
    (fill lens 7 :start 256 :end 280)
    (fill lens 8 :start 280 :end 288)
    (fill lens 5 :start 288 :end 320)
    (cons (deflate-table lens 0 288 :literal +lit-root-bits+)
          (deflate-table lens 288 32 :distance)))
  "The fixed literal/length and distance codes of block type 1.")

(defun read-dynamic-tables (r)
  "Block type 2 header: returns the literal/length and distance decode tables."
  (declare (type reader r))
  (let* ((hlit (+ 257 (read-bits r 5)))
         (hdist (+ 1 (read-bits r 5)))
         (hclen (+ 4 (read-bits r 4)))
         (cl-lens (make-array 19 :element-type '(unsigned-byte 8) :initial-element 0))
         (order +deflate-cl-order+))
    (declare (dynamic-extent cl-lens))
    (when (or (> hlit 286) (> hdist 30)) (berr "too many length or distance codes"))
    (dotimes (i hclen)
      (setf (aref cl-lens (aref order i)) (read-bits r 3)))
    (let ((cl-table (deflate-table cl-lens 0 19 :code-length))
          (lens (make-array 320 :element-type '(unsigned-byte 8) :initial-element 0))
          (total (+ hlit hdist))
          (i 0))
      (declare (dynamic-extent lens) (type (integer 0 320) total i))
      (loop while (< i total) do
        (let ((sym (read-sym r cl-table)))
          (cond
            ((< sym 16) (setf (aref lens i) sym) (incf i))
            ((> sym 18) (berr "invalid code length code"))
            (t (multiple-value-bind (value n)
                   (case sym
                     (16 (when (zerop i) (berr "repeat with no previous length"))
                         (values (aref lens (1- i)) (+ 3 (read-bits r 2))))
                     (17 (values 0 (+ 3 (read-bits r 3))))
                     (t (values 0 (+ 11 (read-bits r 7)))))
                 (when (> (+ i n) total) (berr "code lengths repeat past the end"))
                 (fill lens value :start i :end (+ i n))
                 (incf i n))))))
      (when (zerop (aref lens 256)) (berr "no end-of-block code"))
      ;; the distance lengths sit after the literal ones: copy them down to a
      ;; zero-padded 32-entry area so the table code sees one alphabet
      (let ((dlens (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
        (declare (dynamic-extent dlens))
        (replace dlens lens :start2 hlit :end2 total)
        (values (deflate-table lens 0 hlit :literal +lit-root-bits+)
                (deflate-table dlens 0 32 :distance))))))

;;; ---- the block loop -------------------------------------------------------

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
    ;; would push the loop's state to the stack.  This local BERR, which the
    ;; checks here and in BR-REFILL expand to, records the message and leaves
    ;; the loop instead; the error is signalled after it.
    (block decode
    (flet ((berr (message)
             (setf error message)
             (return-from decode)))
      (declare (inline berr))
    (sb-sys:with-pinned-objects (buf)
      (let ((bs (sb-sys:vector-sap buf)))
        (let ((sym 0))
          (declare (type index sym))
          (macrolet ((next-symbol ()
                       `(setf sym (br-sym ltab (progn) #.+lit-root-bits+)))
                     (literal-or-leave ()
                       ;; store a literal, or go handle the end code / a match
                       `(if (< sym 256)
                            (progn (setf (sb-sys:sap-ref-8 bs fp) sym)
                                   (incf fp))
                            (go other))))
            ;; the early root lookup below needs >= 10 valid bits on entry too
            (br-refill)
            (tagbody
             top
               (when (> fp cap) (setf status :full) (go done))
               ;; a refill leaves >= 56 bits, enough for three literal codes
               ;; (<= 15 bits each) without refilling in between, as zlib-ng's
               ;; fast loop does; the room check above allows the two extra
               ;; bytes (the output slack covers them).  The first symbol's
               ;; root entry is looked up before the refill (>= 10 bits are
               ;; always left here), so the refill's load overlaps it.
               (let ((e (aref ltab (ldb (byte #.+lit-root-bits+ 0) val))))
                 (br-refill)
                 (setf sym (br-sym-entry ltab e #.+lit-root-bits+)))
               (literal-or-leave)
               (next-symbol) (literal-or-leave)
               (next-symbol) (literal-or-leave)
               (go top)
             other
               (when (= sym 256) (go done))
               ;; a match needs up to 33 more bits (5 length extra, 15
               ;; distance code, 13 distance extra), and the next iteration's
               ;; early root lookup 10 after that
               (br-need 43)
               ;; length entries hold +MATCH-FLAG+ | base<<4 | extra bits
               (when (< sym +match-flag+) (berr "invalid length code"))
               (let ((len (+ (ldb (byte 9 4) sym) (ldb (byte (logand sym 15) 0) val))))
                 (declare (type index len))
                 (br-drop (logand sym 15))
                 (let ((d (br-sym dtab (progn))))
                   (declare (type index d))
                   (when (< d +match-flag+) (berr "invalid distance code"))
                   (let ((dist (+ (ldb (byte 15 4) d) (ldb (byte (logand d 15) 0) val))))
                   (declare (type index dist))
                   (br-drop (logand d 15))
                   (when (> dist fp) (berr "distance too far back"))
                   (copy-back-reference buf fp dist len))))
               (go top)
             done)))))))
    (when error (berr error))
    (setf (reader-val r) val (reader-bits r) bits (reader-pos r) pos)
    (values fp status)))

;;; ---- streams -----------------------------------------------------------------

(defun align-any (r)
  "Drop the bits up to the next byte boundary (deflate ignores their value)."
  (with-reader (r) (br-drop (logand bits 7))))

(defun inflate-into (r buf fp)
  "Decode deflate blocks from R into BUF at FP until the final block.  Returns
the (possibly regrown) BUF and the new FP."
  (declare (type reader r) (type u8vec buf) (type index fp))
  (loop
    (let ((final (read-bit r))
          (type (read-bits r 2)))
      (case type
        (0 (align-any r)
           (let* ((p (byte-pos r))
                  (len (progn (when (> (+ p 4) (reader-limit r)) (berr "end of input"))
                              (logior (aref (reader-in r) p) (ash (aref (reader-in r) (+ p 1)) 8))))
                  (nlen (logior (aref (reader-in r) (+ p 2)) (ash (aref (reader-in r) (+ p 3)) 8))))
             (unless (= nlen (logxor len #xffff)) (berr "stored block length check failed"))
             (when (> (+ p 4 len) (reader-limit r)) (berr "end of input"))
             (setf buf (grow buf fp len))
             (replace buf (reader-in r) :start1 fp :start2 (+ p 4) :end2 (+ p 4 len))
             (incf fp len)
             (seek r (+ p 4 len))))
        ((1 2)
         (multiple-value-bind (ltab dtab)
             (if (= type 1)
                 (values (car +fixed-tables+) (cdr +fixed-tables+))
                 (read-dynamic-tables r))
           (loop
             (multiple-value-bind (new-fp status) (inflate-block r buf fp ltab dtab)
               (setf fp new-fp)
               (if (eq status :end)
                   (return)
                   (setf buf (grow buf fp (max 4096 (length buf)))))))))
        (t (berr "invalid block type")))
      (when (= final 1) (return (values buf fp))))))

(defun %inflate (in size-hint zlib start end)
  (let* ((*error-type* 'deflate-error)
         (in (if (typep in 'u8vec) in (coerce in 'u8vec)))
         (len (bounded-end in start end))
         (r (make-reader in :start start :end len))
         (tail (reader-tail r)))
    (sb-sys:with-pinned-objects (in tail)
      (when zlib
        (when (< (- len start) 2) (berr "end of input"))
        (let ((cmf (aref in start)) (flg (aref in (1+ start))))
          (unless (zerop (mod (+ (* 256 cmf) flg) 31)) (berr "header check failed"))
          (unless (= (logand cmf 15) 8) (berr "unknown compression method"))
          (when (> (ash cmf -4) 7) (berr "invalid window size"))
          (when (logbitp 5 flg) (berr "preset dictionary required"))
          (seek r (+ start 2))))
      (multiple-value-bind (buf fp)
          ;; + 258: the block loop wants room for a whole match before each
          ;; symbol, so an exact SIZE-HINT alone would make it regrow at the end
          (inflate-into r (make-array (+ (or size-hint (max 4096 (* 4 (- len start)))) 258 +out-slack+)
                                      :element-type '(unsigned-byte 8))
                        0)
        (declare (type u8vec buf) (type index fp))
        (when (> (- (* 8 (reader-pos r)) (reader-bits r)) (* 8 len)) (berr "end of input"))
        (when zlib
          (align-any r)
          (let ((p (byte-pos r)))
            (when (> (+ p 4) len) (berr "end of input"))
            (unless (= (adler32 buf fp)
                       (logior (ash (aref in p) 24) (ash (aref in (+ p 1)) 16)
                               (ash (aref in (+ p 2)) 8) (aref in (+ p 3))))
              (berr "Adler-32 check failed"))))
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

(defun adler32 (buf n)
  "Adler-32 of the first N bytes of BUF."
  (declare (type u8vec buf) (type index n) (optimize (speed 3) (safety 0)))
  (let ((a 1) (b 0) (i 0))
    (declare (type (unsigned-byte 62) a b) (type index i))
    #+decompress-avx512
    (sb-sys:with-pinned-objects (buf)
      (let ((sap (sb-sys:vector-sap buf))
            (w (sb-simd-avx512bw:s8.64-aref +adler-weights+ 0))
            (ones (sb-simd-avx512bw:s16.32 1))
            (zero (sb-simd-avx512bw:u8.64 0)))
        (declare (type sb-simd-avx512bw:s8.64 w) (type sb-simd-avx512bw:s16.32 ones)
                 (type sb-simd-avx512bw:u8.64 zero))
        (loop while (<= (+ i 64) n) do
          (let ((blocks (min 1024 (floor (- n i) 64)))
                (s1 (sb-simd-avx512f:u64.8 0))     ; byte sums
                (ps (sb-simd-avx512f:u64.8 0))     ; byte sums of the blocks before
                (s2 (sb-simd-avx512f:s32.16 0)))   ; weighted sums
            (declare (type (integer 1 1024) blocks) (type sb-simd-avx512f:u64.8 s1 ps)
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
    ;; the rest (all of it without AVX-512), in chunks small enough that the
    ;; sums stay fixnums
    (loop while (< i n) do
      (let ((end (min n (+ i 5552))))
        (loop while (< i end) do
          (incf a (aref buf i))
          (incf b a)
          (incf i))
        (setf a (mod a 65521) b (mod b 65521))))
    (logior (ash b 16) a)))
