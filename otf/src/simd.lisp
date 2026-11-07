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
  (when (find-package "SB-SIMD-AVX512F")
    (pushnew :zpb-avx512 *features*)))

(sb-ext:defglobal **avx512** nil
  "True if the processor running this has the AVX-512 instructions used here.")

#+zpb-avx512
(setf **avx512** (and (sb-simd-internals:avx512f-supported-p) t))

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

(defun shifted-left-16 (dest source n)
  "DEST[i] = SOURCE[i] shifted left 16 bits, wrapping in 32 bits (the low 16
bits of the source as the high ones: FIXED-FROM-INT), for i below N."
  (declare (type i32-array dest source) (fixnum n) (optimize (speed 3) (safety 0)))
  (let ((i 0))
    (declare (fixnum i))
    #+zpb-avx512
    (when (and **avx512** (>= n 16))
      (loop while (<= (+ i 16) n)
            do (setf (sb-simd-avx512f:s32.16-aref dest i)
                     (sb-simd-avx512f:s32.16-shiftl (sb-simd-avx512f:s32.16-aref source i) 16))
               (incf i 16)))
    (loop while (< i n)
          do (setf (aref dest i) (sb-c::mask-signed-field 32 (ash (aref source i) 16)))
             (incf i))
    nil))
