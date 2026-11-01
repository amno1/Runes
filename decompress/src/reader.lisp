;;;; src/reader.lisp — the bit reader.
;;;;
;;;; Brotli and deflate are read LSB-first.  VAL holds up to 63 not yet consumed
;;;; bits, BITS says how many; a refill ORs in the next 8 input bytes with one
;;;; unaligned load and advances POS by the whole bytes that fit, leaving 56-63
;;;; bits.  The input vector is
;;;; read in place: within 8 bytes of its end a refill loads from TAIL, a copy
;;;; of the last (up to) 8 bytes followed by zeros, so no load ever leaves an
;;;; array.  Running past the real end is detected at the end of the stream.
;;;;
;;;; The macros work on variables named IN LIMIT VAL BITS POS and the reader R.
;;;; Header code uses them on the struct slots (WITH-READER); the hot loop
;;;; copies the slots into locals and uses the same macros on those.

(in-package #:decompress)

(defmacro low-bits (val n)
  `(logand ,val (1- (the sb-ext:word (ash 1 (logand ,n 31))))))

(defmacro load64 (vec pos)
  `(sb-sys:sap-ref-64 (sb-sys:vector-sap ,vec) ,pos))

(defmacro store64 (vec pos word)
  `(setf (sb-sys:sap-ref-64 (sb-sys:vector-sap ,vec) ,pos) ,word))

(defconstant +tail-size+ 32)

(defstruct (reader (:constructor %make-reader (in limit tail tbase)))
  (in nil :type u8vec)
  (limit 0 :type index)                 ; input end
  (val 0 :type sb-ext:word)             ; a raw (untagged) slot
  (bits 0 :type (integer 0 64))
  (pos 0 :type index)                   ; next input byte to load
  (tail nil :type u8vec)                ; last (up to) 8 input bytes + zeros
  (tbase 0 :type index))                ; input offset of TAIL[0]

(declaim (inline make-reader))
(defun make-reader (in &key (start 0) (end (length in)))
  "A reader over the bytes of IN from START below END, read in place."
  (let* ((tbase (max start (- end 8)))
         (tail (make-array +tail-size+ :element-type '(unsigned-byte 8) :initial-element 0))
         (r (progn (replace tail in :start2 tbase :end2 end)
                   (%make-reader in end tail tbase))))
    (setf (reader-pos r) start)
    r))

(defun bounded-end (in start end)
  "END, defaulting to IN's length, after checking 0 <= START <= END <= length."
  (let ((end (or end (length in))))
    (unless (<= 0 start end (length in))
      (error "Bounds ~D..~D are not within an input of ~D bytes" start end (length in)))
    end))

(defmacro with-reader ((r) &body body)
  `(symbol-macrolet ((in (reader-in ,r)) (limit (reader-limit ,r))
                     (val (reader-val ,r)) (bits (reader-bits ,r)) (pos (reader-pos ,r)))
     ,@body))

(defmacro br-refill ()
  ;; BITS <= 63 here.  VAL is a full word (the struct slot is raw), so SBCL
  ;; keeps it untagged; BITS and the byte count use modular word arithmetic
  ;; for the same reason.  Afterwards BITS = old BITS + 8*%K = BITS | 56.
  `(let ((%k (ash (ldb (byte 64 0) (- 63 bits)) -3)))
     (setf val (logior val (ldb (byte 64 0)
                                (ash (if (> (+ pos 8) limit)
                                         (progn
                                           (when (> pos (+ limit 8)) (berr "end of input"))
                                           (let ((%tail (reader-tail r)))
                                             (load64 %tail (- pos (reader-tbase r)))))
                                         (load64 in pos))
                                     (logand bits 63))))
           pos (+ pos %k)
           bits (ldb (byte 64 0) (logior bits 56)))))

(defmacro br-need (n)
  `(when (< bits ,n) (br-refill)))

(defmacro br-drop (n)
  ;; modular, so BITS keeps a full-word type and stays untagged (BITS >= N)
  `(let ((%d ,n))
     (setf val (ash val (- %d))
           bits (ldb (byte 64 0) (- bits %d)))))

(defmacro br-bits (n)
  "Read N (<= 32) bits."
  `(let ((%n ,n))
     (br-need %n)
     (prog1 (low-bits val %n)
       (br-drop %n))))

(defmacro br-bits-no-refill (n)
  "Read N bits assuming VAL already has >= N bits."
  `(let ((%n ,n))
     (prog1 (low-bits val %n)
       (br-drop %n))))

(defmacro br-sym (table &optional (refill '(br-need 15)) (root 8))
  "Decode one symbol with a decode table (see huffman.lisp) whose root has
2^ROOT entries: entry = sym<<5 | nbits, where nbits >= 16 links to a
2^(nbits-16)-entry subtable at offset sym."
  `(let ((%tab ,table))
     (declare (type u32vec %tab))
     ,refill
     (let* ((%e (aref %tab (ldb (byte ,root 0) val)))
            (%l (logand %e 31)))
       (if (< %l 16)
           (progn (br-drop %l) (ash %e -5))
           (progn (br-drop ,root)
                  (let ((%e2 (aref %tab (+ (ash %e -5) (low-bits val (- %l 16))))))
                    (br-drop (logand %e2 31))
                    (ash %e2 -5)))))))

(defmacro br-sym-entry (table entry root)
  "Finish decoding a symbol whose root ENTRY (TABLE's entry at the low ROOT
bits of VAL) was looked up already, e.g. before a refill."
  `(let ((%tab ,table) (%e ,entry))
     (declare (type u32vec %tab) (type (unsigned-byte 32) %e))
     (let ((%l (logand %e 31)))
       (if (< %l 16)
           (progn (br-drop %l) (ash %e -5))
           (progn (br-drop ,root)
                  (let ((%e2 (aref %tab (+ (ash %e -5) (low-bits val (- %l 16))))))
                    (br-drop (logand %e2 31))
                    (ash %e2 -5)))))))

(defmacro br-sym-small (table root-bits)
  "Decode with a one-level 2^ROOT-BITS-entry table (all codes <= ROOT-BITS)."
  `(let ((%tab ,table))
     (declare (type u32vec %tab))
     (br-need ,root-bits)
     (let ((%e (aref %tab (ldb (byte ,root-bits 0) val))))
       (br-drop (logand %e 31))
       (ash %e -5))))

;;; Functional versions for header parsing.
(declaim (inline read-bits read-bit read-sym align byte-pos seek skip-bytes))
(defun read-bits (r n)
  (declare (type reader r) (type (integer 0 32) n))
  (with-reader (r) (br-bits n)))

(defun read-bit (r)
  (read-bits r 1))

(defun read-sym (r table)
  (declare (type reader r))
  (with-reader (r) (br-sym table)))

(defun align (r)
  "Drop the bits up to the next byte boundary; they must be zero (sec. 9.2)."
  (with-reader (r)
    (let ((n (logand bits 7)))
      (unless (zerop (ldb (byte n 0) val)) (berr "nonzero padding bits"))
      (br-drop n))))

(defun byte-pos (r)
  "Input offset of the next unread byte of an aligned reader."
  (with-reader (r) (- pos (ash bits -3))))

(defun seek (r p)
  (setf (reader-val r) 0 (reader-bits r) 0 (reader-pos r) p))

(defun skip-bytes (r n)
  (let ((p (+ (byte-pos r) n)))
    (when (> p (reader-limit r)) (berr "end of input"))
    (seek r p)))
