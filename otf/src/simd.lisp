;;;; simd.lisp — AVX-512 for the loops over many points, where the Lisp has it
;;;; Copyright (c) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

;;; The SBCL with the AVX-512 instructions (sb-simd-512) has SB-SIMD-AVX512F;
;;; the others do not, and neither do machines without the instructions.  So
;;; the code that uses it is there only when the compiler has the package
;;; (feature :ZPB-AVX512), and is used only when the processor has the
;;; instructions; everything has a scalar version that gives the same values.

(in-package #:zpb-ttf)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The fork is told by a symbol of its own; no point loading SB-SIMD in the others.
  (when (and (find-package "SB-VM") (find-symbol "MOVE-TO-AVX512" "SB-VM")
             (not (find-package "SB-SIMD-AVX512F")))
    (ignore-errors (require :sb-simd)))
  ;; An SBCL can have the packages and lack what is used from them (the stock
  ;; SB-SIMD has some AVX-512), so each function is looked for.  A name is
  ;; (PACKAGE SYMBOL); with a :INTERNAL after it, any symbol will do.
  (flet ((has-all (names)
           (every (lambda (entry)
                    (destructuring-bind (package name &optional internal) entry
                      (let ((package (find-package package)))
                        (and package
                             (multiple-value-bind (symbol status) (find-symbol name package)
                               (and symbol (fboundp symbol)
                                    (or internal (eq status :external))))))))
                  names)))
    (when (has-all '(("SB-SIMD-AVX512F" "S32.16-BROADCAST") ("SB-SIMD-AVX512F" "S32.16-AREF")
                     ("SB-SIMD-AVX512F" "S32.16+") ("SB-SIMD-AVX512F" "S32.16-")
                     ("SB-SIMD-AVX512F" "S32.16*") ("SB-SIMD-AVX512F" "S32.16-SHIFTL")
                     ("SB-SIMD-AVX512F" "S64.8-BROADCAST") ("SB-SIMD-AVX512F" "S64.8-SHIFTA")
                     ("SB-SIMD-AVX512F" "S64.8-SHIFTL") ("SB-SIMD-AVX512F" "S64.8+")
                     ("SB-SIMD-AVX512F" "S64.8-AND") ("SB-SIMD-AVX512F" "S64.8-OR")
                     ("SB-SIMD-AVX512F" "S64.8!-FROM-P512" :internal)
                     ("SB-SIMD-AVX512F" "S32.16!-FROM-P512" :internal)
                     ("SB-SIMD-AVX512DQ" "S64.8-MUL")
                     ("SB-SIMD-INTERNALS" "AVX512F-SUPPORTED-P")
                     ("SB-SIMD-INTERNALS" "AVX512DQ-SUPPORTED-P")))
      (pushnew :zpb-avx512 *features*)
      ;; The glyph decoder also needs the byte permute and the widening of
      ;; bytes to 32-bit lanes, which the fork has since it exposes them.
      (when (has-all '(("SB-SIMD-AVX512F" "S32.16-FROM-U8.16") ("SB-SIMD-AVX512F" "S32.16-ALIGNR")
                       ("SB-SIMD-AVX512F" "S32.16-AND") ("SB-SIMD-AVX512F" "S32.16-OR")
                       ("SB-SIMD-AVX512F" "S32.16-XOR") ("SB-SIMD-AVX512F" "S32.16-SHIFTR")
                       ("SB-SIMD-AVX512F" "S32.16-SHIFTA")
                       ("SB-SIMD-AVX512BW" "U8.64-AREF")
                       ("SB-SIMD-AVX512BW" "U8.64!-FROM-P512" :internal)
                       ("SB-SIMD-AVX512VBMI" "U8.64-PERMUTE")
                       ("SB-SIMD-AVX" "U8.16-AREF") ("SB-SIMD-AVX" "U8.16-AND")
                       ("SB-SIMD-AVX" "U8.16-BROADCAST")
                       ("SB-SIMD-INTERNALS" "AVX512BW-SUPPORTED-P")
                       ("SB-SIMD-INTERNALS" "AVX512VBMI-SUPPORTED-P")))
        (pushnew :zpb-avx512-decode *features*)))))

(sb-ext:defglobal **avx512** nil
  "True if the processor running this has the AVX-512 instructions used here.")

(sb-ext:defglobal **avx512-decode** nil
  "True if the processor has the instructions the glyph decoder uses: AVX-512
F, DQ, BW and VBMI.")

#+zpb-avx512
(setf **avx512** (and (sb-simd-internals:avx512f-supported-p)
                      (sb-simd-internals:avx512dq-supported-p) t))

#+zpb-avx512-decode
(setf **avx512-decode** (and **avx512**
                             (sb-simd-internals:avx512bw-supported-p)
                             (sb-simd-internals:avx512vbmi-supported-p)
                             t))

(deftype i32-array () '(simple-array (signed-byte 32) (*)))

(declaim (inline add-products))
(defun add-products (dest source factor n)
  "DEST[i] += SOURCE[i] * FACTOR for i below N, as 32-bit wrapping arithmetic.
Sixteen at a time with AVX-512, if the processor has it."
  (declare (type i32-array dest source) (type (signed-byte 32) factor) (fixnum n)
           (optimize (speed 3) (safety 0)))
  (let ((i 0))
    (declare (fixnum i))
    #+zpb-avx512
    (when (and **avx512** (>= n 16))
      (let ((f (sb-simd-avx512f:s32.16-broadcast factor)))
        (loop while (<= (+ i 16) n)
              do (setf (sb-simd-avx512f:s32.16-aref dest i)
                       (sb-simd-avx512f:s32.16+ (sb-simd-avx512f:s32.16-aref dest i)
                                                (sb-simd-avx512f:s32.16*
                                                 (sb-simd-avx512f:s32.16-aref source i) f)))
                 (incf i 16))))
    (loop while (< i n)
          do (setf (aref dest i)
                   (sb-c::mask-signed-field 32 (+ (aref dest i) (* (aref source i) factor))))
             (incf i))
    nil))

(declaim (inline add-differences shifted-left-16))
(defun add-differences (dest a b n)
  "DEST[i] += A[i] - B[i] for i below N, as 32-bit wrapping arithmetic."
  (declare (type i32-array dest a b) (fixnum n) (optimize (speed 3) (safety 0)))
  (let ((i 0))
    (declare (fixnum i))
    #+zpb-avx512
    (when (and **avx512** (>= n 16))
      (loop while (<= (+ i 16) n)
            do (setf (sb-simd-avx512f:s32.16-aref dest i)
                     (sb-simd-avx512f:s32.16+
                      (sb-simd-avx512f:s32.16-aref dest i)
                      (sb-simd-avx512f:s32.16- (sb-simd-avx512f:s32.16-aref a i)
                                               (sb-simd-avx512f:s32.16-aref b i))))
               (incf i 16)))
    (loop while (< i n)
          do (setf (aref dest i)
                   (sb-c::mask-signed-field 32 (+ (aref dest i) (- (aref a i) (aref b i)))))
             (incf i))
    nil))

(defun shifted-left-16 (dest source n &optional (src-start 0))
  "DEST[i] = SOURCE[src-start + i] shifted left 16 bits, wrapping in 32 bits
(the low 16 bits of the source as the high ones: FIXED-FROM-INT), for i below N."
  (declare (type i32-array dest source) (fixnum n src-start) (optimize (speed 3) (safety 0)))
  (let ((i 0))
    (declare (fixnum i))
    #+zpb-avx512
    (when (and **avx512** (>= n 16))
      (loop while (<= (+ i 16) n)
            do (setf (sb-simd-avx512f:s32.16-aref dest i)
                     (sb-simd-avx512f:s32.16-shiftl
                      (sb-simd-avx512f:s32.16-aref source (+ src-start i)) 16))
               (incf i 16)))
    (loop while (< i n)
          do (setf (aref dest i)
                   (sb-c::mask-signed-field 32 (ash (aref source (+ src-start i)) 16)))
             (incf i))
    nil))


(declaim (inline mul-fix-1))
(defun mul-fix-1 (a b)
  "A times B rounded half away from zero as FreeType's FT_MulFix, wrapped to 32 bits."
  (declare (type (signed-byte 32) a b) (optimize (speed 3) (safety 0)))
  (let ((ab (* a b)))
    (sb-c::mask-signed-field 32 (ash (+ ab #x8000 (ash ab -63)) -16))))

(defun mul-fix-into (out1 out2 source factor n &optional (start 0))
  "OUT1[start + i] = OUT2[start + i] = SOURCE[start + i] times FACTOR as
FreeType's FT_MulFix, for i below N.  With AVX-512 the 32x32 to 64 bit products
of eight points are made by one instruction, and the sixteen results put
together again."
  (declare (type i32-array out1 out2 source) (type (signed-byte 32) factor) (fixnum n start)
           (optimize (speed 3) (safety 0)))
  (let ((i 0))
    (declare (fixnum i))
    #+zpb-avx512
    (when (and **avx512** (>= n 16))
      (let ((f (sb-simd-avx512f:s64.8-broadcast factor))
            (half (sb-simd-avx512f:s64.8-broadcast #x8000))
            (low (sb-simd-avx512f:s64.8-broadcast #xFFFFFFFF)))
        (flet ((rounded (p)
                 ;; (p + 0x8000 + (p >> 63)) >> 16
                 (sb-simd-avx512f:s64.8-shifta
                  (sb-simd-avx512f:s64.8+ (sb-simd-avx512f:s64.8+ p half)
                                          (sb-simd-avx512f:s64.8-shifta p 63))
                  16)))
          (declare (inline rounded))
          (loop while (<= (+ i 16) n)
                do (let* ((at (+ start i))
                          (u (sb-simd-avx512f::s64.8!-from-p512
                              (sb-simd-avx512f:s32.16-aref source at)))
                          ;; The even-numbered points are the low halves of the
                          ;; 64-bit lanes, the odd ones the high halves.
                          (even (rounded (sb-simd-avx512dq:s64.8-mul u f)))
                          (odd (rounded (sb-simd-avx512dq:s64.8-mul
                                         (sb-simd-avx512f:s64.8-shifta u 32) f)))
                          (both (sb-simd-avx512f::s32.16!-from-p512
                                 (sb-simd-avx512f:s64.8-or
                                  (sb-simd-avx512f:s64.8-and even low)
                                  (sb-simd-avx512f:s64.8-shiftl odd 32)))))
                     (setf (sb-simd-avx512f:s32.16-aref out1 at) both
                           (sb-simd-avx512f:s32.16-aref out2 at) both))
                   (incf i 16)))))
    (loop while (< i n)
          do (let* ((at (+ start i))
                    (v (mul-fix-1 (aref source at) factor)))
               (setf (aref out1 at) v
                     (aref out2 at) v))
             (incf i))
    nil))

#+zpb-avx512-decode
(progn
  (defmacro %v+ (a b) `(sb-simd-avx512f:s32.16+ ,a ,b))
  (defmacro %v- (a b) `(sb-simd-avx512f:s32.16- ,a ,b))
  (defmacro %vand (a b) `(sb-simd-avx512f:s32.16-and ,a ,b))
  (defmacro %vor (a b) `(sb-simd-avx512f:s32.16-or ,a ,b))
  (defmacro %vxor (a b) `(sb-simd-avx512f:s32.16-xor ,a ,b))
  (defmacro %vshl (a n) `(sb-simd-avx512f:s32.16-shiftl ,a ,n))
  (defmacro %vshr (a n) `(sb-simd-avx512f:s32.16-shiftr ,a ,n))
  (defmacro %vsar (a n) `(sb-simd-avx512f:s32.16-shifta ,a ,n))
  (defmacro %bc (n) `(sb-simd-avx512f:s32.16-broadcast ,n))

  (defmacro %prefix-sum (x zero)
    "The inclusive running sums of the sixteen lanes of X: shifted copies
added, by 1, 2, 4 and 8 lanes (VALIGND with zeros brought in)."
    `(let* ((x ,x)
            (x (%v+ x (sb-simd-avx512f:s32.16-alignr x ,zero 15)))
            (x (%v+ x (sb-simd-avx512f:s32.16-alignr x ,zero 14)))
            (x (%v+ x (sb-simd-avx512f:s32.16-alignr x ,zero 12)))
            (x (%v+ x (sb-simd-avx512f:s32.16-alignr x ,zero 8))))
       x))

  (defmacro decode-axis-block (bytes q flags j out carry cell short-bit same-bit mask-flags)
    "Decode sixteen coordinates of one axis of a simple glyph: the flags at
FLAGS from J, the coordinate bytes of BYTES from Q, where each is a byte
(SHORT-BIT; its sign from SAME-BIT), the previous value (SAME-BIT without
SHORT-BIT) or a big-endian word.  The running values, from CARRY, go to OUT from
J; CELL (sixteen 32-bit integers) gets the byte offsets after each point, so
its last element is the number of bytes used.  With MASK-FLAGS the flags are cut
to their low bit.  Q must have 64 bytes in BYTES.

The length of each point's coordinate is 1 for a short one, 0 for a repeat,
else 2; their running sums are the offsets, VPERMB puts each point's two bytes
in its lane, and the sum of the values, again running, plus CARRY gives the
coordinates."
    `(let* ((zero (%bc 0)) (one (%bc 1))
            (fbytes (sb-simd-avx:u8.16-aref ,flags ,j))
            (f (sb-simd-avx512f:s32.16-from-u8.16 fbytes))
            (s (%vand (%vshr f ,short-bit) one))
            (m (%vand (%vshr f ,same-bit) one))
            ;; 1 where the coordinate is a word: neither short nor repeated
            (tw (%vand (%vxor (%vor s m) one) one))
            (len (%v+ s (%vshl tw 1)))
            (incl (%prefix-sum len zero))
            (off (%v- incl len))
            (idx (%vor off (%vshl (%v+ off one) 8)))
            (src (sb-simd-avx512bw:u8.64-aref ,bytes ,q))
            (got (sb-simd-avx512f::s32.16!-from-p512
                  (sb-simd-avx512vbmi:u8.64-permute
                   (sb-simd-avx512bw::u8.64!-from-p512 idx) src)))
            (b0 (%vand got (%bc 255)))
            (b1 (%vand (%vshr got 8) (%bc 255)))
            (negate (%v- m one))
            (bytev (%v- (%vxor b0 negate) negate))
            (wordv (%vsar (%vor (%vshl b0 24) (%vshl b1 16)) 16))
            (value (%vor (%vand bytev (%v- zero s)) (%vand wordv (%v- zero tw))))
            (sum (%v+ (%prefix-sum value zero) (%bc ,carry))))
       (setf (sb-simd-avx512f:s32.16-aref ,out ,j) sum
             (sb-simd-avx512f:s32.16-aref ,cell 0) incl)
       ,@(when mask-flags
           `((setf (sb-simd-avx:u8.16-aref ,flags ,j)
                   (sb-simd-avx:u8.16-and fbytes (sb-simd-avx:u8.16-broadcast 1)))))
       nil)))

(defun copy-points-out (xs ys on cx cy zf n x-shift)
  "XS[i] = CX[i] - X-SHIFT (wrapping in 32 bits), YS[i] = CY[i] and ON[i] = the
low bit of ZF[i], for i below N: the points of a hinted glyph out of the zone
into an outline.  Sixteen at a time with AVX-512, if the processor has it."
  (declare (type i32-array xs ys cx cy)
           (type (simple-array (unsigned-byte 8) (*)) on zf)
           (type (signed-byte 32) x-shift) (fixnum n)
           (optimize (speed 3) (safety 0)))
  (let ((i 0))
    (declare (fixnum i))
    #+zpb-avx512-decode
    (when (and **avx512-decode** (>= n 16))
      (let ((shift (sb-simd-avx512f:s32.16-broadcast x-shift))
            (one (sb-simd-avx:u8.16-broadcast 1)))
        (loop while (<= (+ i 16) n)
              do (setf (sb-simd-avx512f:s32.16-aref xs i)
                       (sb-simd-avx512f:s32.16- (sb-simd-avx512f:s32.16-aref cx i) shift)
                       (sb-simd-avx512f:s32.16-aref ys i)
                       (sb-simd-avx512f:s32.16-aref cy i)
                       (sb-simd-avx:u8.16-aref on i)
                       (sb-simd-avx:u8.16-and (sb-simd-avx:u8.16-aref zf i) one))
                 (incf i 16))))
    (loop while (< i n)
          do (setf (aref xs i) (sb-c::mask-signed-field 32 (- (aref cx i) x-shift))
                   (aref ys i) (aref cy i)
                   (aref on i) (logand (aref zf i) 1))
             (incf i))
    nil))
