;;;; src/package.lisp
(defpackage #:decompress
  (:use #:cl)
  (:export #:decompress #:brotli-error
           #:inflate #:zlib-decompress #:deflate-error
           #:decompression-error))

(in-package #:decompress)

;;; The bit reader loads 8 input bytes as one native 64-bit word, which has
;;; the bits in stream order only on a little-endian CPU.  Elsewhere every
;;; stream would decode to garbage, so refuse to build instead.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (and (member :little-endian *features*) (member :64-bit *features*))
    (error "decompress needs a 64-bit little-endian SBCL.")))

(deftype u8vec () '(simple-array (unsigned-byte 8) (*)))
(deftype u32vec () '(simple-array (unsigned-byte 32) (*)))
(deftype index () '(integer 0 #.(ash most-positive-fixnum -4)))

(define-condition decompression-error (error)
  ((message :initarg :message :reader decompression-error-message))
  (:report (lambda (c s) (format s "~a: ~a" (type-of c) (decompression-error-message c)))))

(define-condition brotli-error (decompression-error) ()
  (:documentation "Malformed Brotli input."))

(define-condition deflate-error (decompression-error) ()
  (:documentation "Malformed deflate or zlib input."))

;;; The bit reader and the prefix-code builder are shared by both decoders;
;;; each decoder binds the condition its errors are signalled as.  Read only
;;; when an error is signalled, so it costs nothing on valid input.
(defvar *error-type* 'brotli-error)

;;; Out of line on purpose: every check in the decoders calls it, and only on
;;; corrupt input.
(defun err (fmt &rest args)
  (error *error-type* :message (apply #'format nil fmt args)))

;;; Signals the error FMT/ARGS when TEST is true.  A macro, so inside
;;; INFLATE-BLOCK the ERR it expands to is that function's local one.
(defmacro check-error (test fmt &rest args)
  `(when ,test (err ,fmt ,@args)))

;;; Signals the error FMT/ARGS unless TEST is true.
(defmacro check-valid (test fmt &rest args)
  `(unless ,test (err ,fmt ,@args)))

;;; Signals "end of input" when reading up to END would go past LIMIT.
(defmacro check-end (end limit)
  `(check-error (> ,end ,limit) "end of input"))

;;; Long back-reference copies use 64-byte AVX-512 moves when SBCL has sb-simd
;;; with AVX512BW (the sb-simd-512 branch) *and* the compiling CPU supports
;;; it.  Decided at compile time so the hot loop has no run-time test: a fasl
;;; built this way must only be loaded on AVX-512 CPUs.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require :sb-simd))
  ;; Stock sb-simd may have the AVX512BW package without the operations only
  ;; the sb-simd-512 branch adds; check for those too.
  (when (and (find-package "SB-SIMD-AVX512BW")
             (every (lambda (name) (find-symbol name "SB-SIMD-AVX512BW"))
                    '("U8.64-SAD" "S16.32-MADDUBS" "U8.64-SAP-REF"))
             (ignore-errors
              (funcall (find-symbol "INSTRUCTION-SET-AVAILABLE-P" "SB-SIMD-INTERNALS")
                       (funcall (find-symbol "FIND-INSTRUCTION-SET" "SB-SIMD-INTERNALS")
                                :avx512bw))))
    (pushnew :decompress-avx512 *features*))
  ;; Otherwise stock sb-simd's AVX2 (32 bytes per step) for Adler-32.
  (when (and (not (member :decompress-avx512 *features*))
             (find-package "SB-SIMD-AVX2")
             (ignore-errors
              (funcall (find-symbol "INSTRUCTION-SET-AVAILABLE-P" "SB-SIMD-INTERNALS")
                       (funcall (find-symbol "FIND-INSTRUCTION-SET" "SB-SIMD-INTERNALS")
                                :avx2))))
    (pushnew :decompress-avx2 *features*)))
