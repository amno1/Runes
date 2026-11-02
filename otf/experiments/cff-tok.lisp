;;;; cff-tok.lisp — CFF Type2 charstring tokenizing: scalar vs AVX-512 (VBMI).
;;;;
;;;; Task: find every token start (number or operator) in every charstring
;;;; of a CFF font, without following subroutines.  Both versions return
;;;; (values token-count sum-of-token-positions) so they can be checked
;;;; against each other.
;;;;
;;;; Token length depends only on the first byte, except hintmask/cntrmask
;;;; (19/20), whose mask length depends on the number of stems declared
;;;; earlier.  Stems are declared only at the start of a charstring, so the
;;;; SIMD version runs a short scalar prefix up to the first hintmask or
;;;; drawing operator, after which every token length is a pure function of
;;;; its first byte.
;;;;
;;;; The SIMD block step (64 bytes): byte -> length via two VPERMI2B lookups,
;;;; NEXT[p] = p + LEN[p], then pointer doubling J(2^k) = J(2^(k-1)) o
;;;; J(2^(k-1)) with VPERMI2B (positions >= 64 map to themselves).  Lane k of
;;;; V = J^k(0) is the k-th token start, so V is the compacted list of token
;;;; offsets in the block, and J^64(0) is where the next block starts.
;;;;
;;;; Run with the sb-simd-512 SBCL:  run-sbcl.sh --load cff-tok.lisp

(require :sb-simd)
(defpackage #:cff-tok
  (:use #:cl #:sb-simd-avx512vbmi)
  (:shadowing-import-from #:sb-simd-avx512bw #:u8.64-sad))
(in-package #:cff-tok)

;;; POPCNT without SBCL's per-call CPU check (from wordcount's swar.cl).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown fpopcount ((unsigned-byte 64)) (integer 0 64)
      (sb-c:flushable sb-c:movable)
    :overwrite-fndb-silently t))
(in-package "SB-VM")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-vop (cff-tok::fpopcount)
    (:policy :fast-safe)
    (:translate cff-tok::fpopcount)
    (:args (x :scs (unsigned-reg)))
    (:arg-types unsigned-num)
    (:results (r :scs (unsigned-reg)))
    (:result-types unsigned-num)
    (:generator 2 (inst popcnt r x))))
(in-package #:cff-tok)

(deftype u8vec () '(simple-array (unsigned-byte 8) (*)))
(deftype u32vec () '(simple-array (unsigned-byte 32) (*)))

;;; ---- extracting the CharStrings INDEX from an OTF/TTC -------------------

(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v)))
(defun be (v i n) (let ((r 0)) (dotimes (k n r) (setf r (logior (ash r 8) (aref v (+ i k)))))))

(defun read-index (v pos)
  "Absolute offsets (count+1 of them) of a CFF INDEX at POS."
  (let ((count (be v pos 2)))
    (if (zerop count) (vector (+ pos 2))
        (let* ((os (aref v (+ pos 2))) (obase (+ pos 3)) (dbase (+ obase (* (1+ count) os) -1))
               (offs (make-array (1+ count))))
          (dotimes (i (1+ count) offs)
            (setf (aref offs i) (+ dbase (be v (+ obase (* i os)) os))))))))

(defun dict-op (v s e want)
  "Last operand before operator WANT in a CFF DICT [S,E)."
  (let ((stack '()) (p s))
    (loop while (< p e) do
      (let ((b (aref v p)))
        (cond ((<= 32 b 246) (push (- b 139) stack) (incf p))
              ((<= 247 b 250) (push (+ (* (- b 247) 256) (aref v (1+ p)) 108) stack) (incf p 2))
              ((<= 251 b 254) (push (- (+ (* (- b 251) 256) (aref v (1+ p)) 108)) stack) (incf p 2))
              ((= b 28) (push (be v (1+ p) 2) stack) (incf p 3))
              ((= b 29) (push (be v (1+ p) 4) stack) (incf p 5))
              ((= b 30) (incf p) (loop until (or (= (logand (aref v p) 15) 15) (= (ash (aref v p) -4) 15))
                                       do (incf p))
               (incf p) (push 0 stack))
              (t (let ((op (if (= b 12) (+ 1200 (aref v (1+ p))) b)))
                   (when (= op want) (return-from dict-op (first stack)))
                   (setf stack '()) (incf p (if (= b 12) 2 1)))))))))

(defun load-charstrings (path)
  "Returns (values data offsets): all charstrings of the first font in PATH,
concatenated, with 128 bytes of zero padding."
  (let* ((v (slurp path))
         (font (if (= (be v 0 4) #x74746366) (be v 12 4) 0))   ; 'ttcf'
         (cff (loop for i below (be v (+ font 4) 2)
                    for rec = (+ font 12 (* 16 i))
                    when (= (be v rec 4) #x43464620) return (be v (+ rec 8) 4)))  ; 'CFF '
         (names (read-index v (+ cff (aref v (+ cff 2)))))
         (tops (read-index v (aref names (1- (length names)))))
         (cs-off (+ cff (dict-op v (aref tops 0) (aref tops 1) 17)))
         (offs (read-index v cs-off))
         (n (1- (length offs)))
         (start (aref offs 0)) (size (- (aref offs n) start))
         (data (make-array (+ size 128) :element-type '(unsigned-byte 8) :initial-element 0))
         (o (make-array (1+ n) :element-type '(unsigned-byte 32))))
    (replace data v :start2 start :end2 (+ start size))
    (dotimes (i (1+ n)) (setf (aref o i) (- (aref offs i) start)))
    (values data o)))

;;; ---- scalar ---------------------------------------------------------------

(defparameter +len+
  (let ((a (make-array 256 :element-type '(unsigned-byte 8) :initial-element 1)))
    (loop for b from 247 to 254 do (setf (aref a b) 2))
    (setf (aref a 255) 5 (aref a 28) 3 (aref a 12) 2)
    a))

(defmacro scalar-step (cs p count sum nstems nargs len &key stop-at-draw)
  "One token at P.  With STOP-AT-DRAW, return from the enclosing block NIL
instead of consuming a non-stem operator, and return T after a hintmask."
  `(let ((b (aref ,cs ,p)))
     (cond ((or (>= b 32) (= b 28))
            (incf ,count) (incf ,sum ,p) (incf ,nargs) (incf ,p (aref ,len b)))
           ((or (= b 1) (= b 3) (= b 18) (= b 23))
            (incf ,count) (incf ,sum ,p) (incf ,nstems (ash ,nargs -1)) (setf ,nargs 0) (incf ,p))
           ((or (= b 19) (= b 20))
            (incf ,count) (incf ,sum ,p) (incf ,nstems (ash ,nargs -1)) (setf ,nargs 0)
            (incf ,p (+ 1 (ash (+ ,nstems 7) -3)))
            ,@(when stop-at-draw '((when (= b 19) (return t)))))   ; cntrmask may precede more stems
           (t ,@(when stop-at-draw '((unless (or (= b 10) (= b 29)) (return nil))))   ; subr calls may precede stems
              (incf ,count) (incf ,sum ,p) (setf ,nargs 0) (incf ,p (if (= b 12) 2 1))))))

(defun scalar-tokens (cs offs)
  (declare (optimize speed (safety 0)) (type u8vec cs) (type u32vec offs))
  (let ((count 0) (sum 0) (len +len+))
    (declare (type fixnum count sum) (type u8vec len))
    (dotimes (g (1- (length offs)))
      (let ((p (aref offs g)) (e (aref offs (1+ g))) (nstems 0) (nargs 0))
        (declare (type fixnum p e nstems nargs))
        ;; stems may only be declared before the first hintmask / drawing
        ;; operator (Type2 spec), so the count is frozen after that
        (block prefix
          (loop while (< p e) do (scalar-step cs p count sum nstems nargs len :stop-at-draw t)))
        (let ((extra (ash (+ nstems 7) -3)))
          (declare (type fixnum extra))
          (loop while (< p e) do
            (let ((b (aref cs p)))
              (incf count) (incf sum p)
              (incf p (if (or (= b 19) (= b 20)) (1+ extra) (aref len b))))))))
    (values count sum)))

;;; ---- AVX-512 --------------------------------------------------------------

(defun bytes (f)
  (let ((a (make-array 64 :element-type '(unsigned-byte 8))))
    (dotimes (i 64) (setf (aref a i) (funcall f i)))
    (u8.64-aref a 0)))

(defun simd-tokens (cs offs)
  (declare (optimize speed (safety 0)) (type u8vec cs) (type u32vec offs))
  (let* ((count 0) (sum 0) (len +len+)
         (iota (bytes #'identity))
         (idhi (bytes (lambda (i) (+ 64 i))))                 ; positions >= 64 are fixed points
         (t0 (bytes (lambda (i) (aref +len+ i))))             ; LEN for bytes 0..63 (19/20 patched)
         (t1 (bytes (lambda (i) (aref +len+ (+ 64 i)))))
         (t2 (bytes (lambda (i) (aref +len+ (+ 128 i)))))
         (t3 (bytes (lambda (i) (aref +len+ (+ 192 i)))))
         (hm (bytes (lambda (i) (if (or (= i 19) (= i 20)) 255 0))))
         (bit0 (bytes (lambda (i) (if (logbitp 0 i) 255 0)))) (bit1 (bytes (lambda (i) (if (logbitp 1 i) 255 0))))
         (bit2 (bytes (lambda (i) (if (logbitp 2 i) 255 0)))) (bit3 (bytes (lambda (i) (if (logbitp 3 i) 255 0))))
         (bit4 (bytes (lambda (i) (if (logbitp 4 i) 255 0)))) (bit5 (bytes (lambda (i) (if (logbitp 5 i) 255 0))))
         (zero (u8.64 0)) (b128 (u8.64 128))
         (acc (u8.64-sad zero zero))
         (tmp (make-array 64 :element-type '(unsigned-byte 8))))
    (declare (type fixnum count sum) (type u8vec len tmp)
             (type u8.64 iota idhi t0 t1 t2 t3 hm bit0 bit1 bit2 bit3 bit4 bit5 zero b128)
             (type sb-simd-avx512f:u64.8 acc))
    (sb-sys:with-pinned-objects (cs tmp)
      (let ((sap (sb-sys:vector-sap cs)))
        (dotimes (g (1- (length offs)))
          (let ((p (aref offs g)) (e (aref offs (1+ g))) (nstems 0) (nargs 0))
            (declare (type fixnum p e nstems nargs))
            ;; scalar prefix: stem hints and the first hintmask
            (block prefix
              (loop while (< p e) do (scalar-step cs p count sum nstems nargs len :stop-at-draw t)))
            (when (< p e)
              (let* ((h (min 63 (ash (+ nstems 7) -3)))       ; extra hintmask bytes
                     (t0h (u8.64+ t0 (u8.64-and hm (u8.64 h))))
                     (base p))
                (declare (type fixnum base))
                (loop while (< base e) do
                  (let* ((x (u8.64-sap-ref sap base))
                         (l (u8.64-if (u8.64>= x b128) (u8.64-permi2 x t2 t3) (u8.64-permi2 x t0h t1)))
                         (j1 (u8.64+ iota l))
                         (j2 (u8.64-permi2 j1 j1 idhi))
                         (j4 (u8.64-permi2 j2 j2 idhi))
                         (j8 (u8.64-permi2 j4 j4 idhi))
                         (j16 (u8.64-permi2 j8 j8 idhi))
                         (j32 (u8.64-permi2 j16 j16 idhi))
                         (j64 (u8.64-permi2 j32 j32 idhi))
                         ;; lane k <- J^k(0): apply J(2^b) where bit b of k is set
                         (v (u8.64-if bit0 (u8.64-permi2 zero j1 idhi) zero))
                         (v (u8.64-if bit1 (u8.64-permi2 v j2 idhi) v))
                         (v (u8.64-if bit2 (u8.64-permi2 v j4 idhi) v))
                         (v (u8.64-if bit3 (u8.64-permi2 v j8 idhi) v))
                         (v (u8.64-if bit4 (u8.64-permi2 v j16 idhi) v))
                         (v (u8.64-if bit5 (u8.64-permi2 v j32 idhi) v))
                         (lim (u8.64 (min 64 (- e base))))
                         (n (fpopcount (m8.64-value (u8.64-mask< v lim)))))
                    (declare (type fixnum n))
                    (setf (u8.64-aref tmp 0) j64)
                    (incf count n)
                    (incf sum (* n base))
                    (setf acc (sb-simd-avx512f:u64.8+ acc (u8.64-sad (u8.64-if (u8.64< v lim) v zero) zero)))
                    (incf base (aref tmp 0))))))))))
    (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7) (sb-simd-avx512f:u64.8-values acc)
      (values count (+ sum a0 a1 a2 a3 a4 a5 a6 a7)))))

;;; Two glyphs in flight: each 64-byte block is a chain of ~12 dependent
;;; VPERMI2Bs plus a store/reload for the next block start, so running two
;;; independent chains per iteration lets them overlap.  When a slot's glyph
;;; ends, the next glyph (after its scalar prefix) is loaded into that slot.
(defun simd-tokens-2 (cs offs)
  (declare (optimize speed (safety 0)) (type u8vec cs) (type u32vec offs))
  (let* ((count 0) (sum 0) (len +len+)
         (iota (bytes #'identity))
         (idhi (bytes (lambda (i) (+ 64 i))))
         (t0 (bytes (lambda (i) (aref +len+ i))))
         (t1 (bytes (lambda (i) (aref +len+ (+ 64 i)))))
         (t2 (bytes (lambda (i) (aref +len+ (+ 128 i)))))
         (t3 (bytes (lambda (i) (aref +len+ (+ 192 i)))))
         (hm (bytes (lambda (i) (if (or (= i 19) (= i 20)) 255 0))))
         (bit0 (bytes (lambda (i) (if (logbitp 0 i) 255 0)))) (bit1 (bytes (lambda (i) (if (logbitp 1 i) 255 0))))
         (bit2 (bytes (lambda (i) (if (logbitp 2 i) 255 0)))) (bit3 (bytes (lambda (i) (if (logbitp 3 i) 255 0))))
         (bit4 (bytes (lambda (i) (if (logbitp 4 i) 255 0)))) (bit5 (bytes (lambda (i) (if (logbitp 5 i) 255 0))))
         (zero (u8.64 0)) (b128 (u8.64 128))
         (acc (u8.64-sad zero zero))
         (tmp (make-array 128 :element-type '(unsigned-byte 8)))
         (g 0) (ng (1- (length offs)))
         (ba 0) (ea 0) (tha t0) (bb 0) (eb 0) (thb t0))
    (declare (type fixnum count sum g ng ba ea bb eb) (type u8vec len tmp)
             (type u8.64 iota idhi t0 t1 t2 t3 hm bit0 bit1 bit2 bit3 bit4 bit5 zero b128 tha thb)
             (type sb-simd-avx512f:u64.8 acc))
    (sb-sys:with-pinned-objects (cs tmp)
      (let ((sap (sb-sys:vector-sap cs)))
        (macrolet ((start (b e th)
                     ;; next glyph with SIMD work into this slot, or an idle slot (b = e = 0)
                     `(loop
                        (when (>= g ng) (setf ,b 0 ,e 0) (return))
                        (let ((p (aref offs g)) (e (aref offs (1+ g))) (nstems 0) (nargs 0))
                          (declare (type fixnum p e nstems nargs))
                          (incf g)
                          (block prefix
                            (loop while (< p e) do (scalar-step cs p count sum nstems nargs len :stop-at-draw t)))
                          (when (< p e)
                            (setf ,b p ,e e
                                  ,th (u8.64+ t0 (u8.64-and hm (u8.64 (min 63 (ash (+ nstems 7) -3))))))
                            (return)))))
                   (blk-step (b e th slot)
                     `(let* ((x (u8.64-sap-ref sap ,b))
                             (l (u8.64-if (u8.64>= x b128) (u8.64-permi2 x t2 t3) (u8.64-permi2 x ,th t1)))
                             (j1 (u8.64+ iota l))
                             (j2 (u8.64-permi2 j1 j1 idhi))
                             (j4 (u8.64-permi2 j2 j2 idhi))
                             (j8 (u8.64-permi2 j4 j4 idhi))
                             (j16 (u8.64-permi2 j8 j8 idhi))
                             (j32 (u8.64-permi2 j16 j16 idhi))
                             (j64 (u8.64-permi2 j32 j32 idhi))
                             (v (u8.64-if bit0 (u8.64-permi2 zero j1 idhi) zero))
                             (v (u8.64-if bit1 (u8.64-permi2 v j2 idhi) v))
                             (v (u8.64-if bit2 (u8.64-permi2 v j4 idhi) v))
                             (v (u8.64-if bit3 (u8.64-permi2 v j8 idhi) v))
                             (v (u8.64-if bit4 (u8.64-permi2 v j16 idhi) v))
                             (v (u8.64-if bit5 (u8.64-permi2 v j32 idhi) v))
                             (lim (u8.64 (max 0 (min 64 (- ,e ,b)))))
                             (n (fpopcount (m8.64-value (u8.64-mask< v lim)))))
                        (setf (u8.64-aref tmp ,slot) j64)
                        (incf count n)
                        (incf sum (* n ,b))
                        (setf acc (sb-simd-avx512f:u64.8+ acc (u8.64-sad (u8.64-if (u8.64< v lim) v zero) zero)))
                        (incf ,b (aref tmp ,slot)))))
          (start ba ea tha)
          (start bb eb thb)
          (loop while (or (< ba ea) (< bb eb)) do
            (blk-step ba ea tha 0)
            (blk-step bb eb thb 64)
            (when (>= ba ea) (start ba ea tha))
            (when (>= bb eb) (start bb eb thb))))))
    (multiple-value-bind (a0 a1 a2 a3 a4 a5 a6 a7) (sb-simd-avx512f:u64.8-values acc)
      (values count (+ sum a0 a1 a2 a3 a4 a5 a6 a7)))))

;;; ---- benchmark --------------------------------------------------------------

(defun now () (multiple-value-bind (s us) (sb-unix::get-time-of-day) (+ s (/ us 1d6))))

(defparameter *variants*
  '(("scalar" scalar-tokens) ("avx512 x1" simd-tokens) ("avx512 x2" simd-tokens-2)))

(defun bench (path &key (reps 10))
  (multiple-value-bind (cs offs) (load-charstrings path)
    (let* ((n (1- (length offs))) (bytes (aref offs n)))
      (format t "~&~a~%  ~d charstrings, ~,1f MB, avg ~,1f bytes~%"
              (file-namestring path) n (/ bytes 1d6) (/ bytes n))
      (multiple-value-bind (c1 s1) (scalar-tokens cs offs)
        (loop for (label f) in *variants* do
          (multiple-value-bind (c s) (funcall f cs offs)
            (let ((best 0d0))
              (dotimes (k 5)
                (let ((t0 (now)))
                  (dotimes (i reps) (funcall f cs offs))
                  (setf best (max best (/ (* reps bytes) 1d6 (- (now) t0))))))
              (format t "  ~11a ~6,0f MB/s  ~5,2f ns/token  ~:[RESULT DIFFERS~;ok~]~%"
                      label best (/ (* 1d3 bytes) (* best c1)) (and (= c c1) (= s s1))))))))))
