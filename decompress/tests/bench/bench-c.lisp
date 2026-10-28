;;;; tests/bench/bench-c.lisp — decode speed of libbrotli and libz (C), for comparison.
;;;;
;;;; Same files and method as bench.lisp: every stream in data/, ~100 MB of
;;;; output per round, best of 5 rounds, MB/s of output.  Brotli streams go to
;;;; libbrotlidec's one-shot BrotliDecoderDecompress, zlib streams and WOFF 1.0
;;;; tables to libz's uncompress (or, with ZLIB=libdeflate, to libdeflate's
;;;; libdeflate_zlib_decompress), through SB-ALIEN.  Like the Lisp decoders
;;;; with :SIZE-HINT, each call gets the exact output size and a fresh output
;;;; vector.  (The library itself uses no FFI; only this benchmark does.)
;;;;
;;;;   sbcl --script bench-c.lisp          (run from tests/bench)
;;;;   ONLY=.z sbcl --script bench-c.lisp  (only files whose name contains .z)
;;;;   ZLIB=libdeflate sbcl --script bench-c.lisp   (zlib streams to libdeflate)
;;;;
;;;; Needs libbrotlidec and libz (and libdeflate for ZLIB=libdeflate);
;;;; LIBBROTLIDEC / LIBZ / LIBDEFLATE override their paths.  (On distributions
;;;; that ship zlib-ng's compatibility build as libz.so, libz is zlib-ng.)

(sb-alien:load-shared-object (or (sb-ext:posix-getenv "LIBBROTLIDEC") "libbrotlidec.so"))
(sb-alien:load-shared-object (or (sb-ext:posix-getenv "LIBZ") "libz.so"))

;;; BrotliDecoderResult BrotliDecoderDecompress(size_t encoded_size,
;;;     const uint8_t *encoded, size_t *decoded_size, uint8_t *decoded);
(sb-alien:define-alien-routine ("BrotliDecoderDecompress" brotli-decoder-decompress) sb-alien:int
  (encoded-size sb-alien:unsigned-long)
  (encoded sb-sys:system-area-pointer)
  (decoded-size (* sb-alien:unsigned-long))
  (decoded sb-sys:system-area-pointer))

;;; int uncompress(uint8_t *dest, unsigned long *destLen, const uint8_t *source,
;;;     unsigned long sourceLen);
(sb-alien:define-alien-routine ("uncompress" z-uncompress) sb-alien:int
  (dest sb-sys:system-area-pointer)
  (dest-len (* sb-alien:unsigned-long))
  (source sb-sys:system-area-pointer)
  (source-len sb-alien:unsigned-long))

(defvar *libdeflate*
  (when (equal (sb-ext:posix-getenv "ZLIB") "libdeflate")
    (sb-alien:load-shared-object (or (sb-ext:posix-getenv "LIBDEFLATE") "libdeflate.so"))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "libdeflate_alloc_decompressor" (function sb-sys:system-area-pointer))))
  "A libdeflate decompressor, or NIL to use libz.")

(defun libdeflate-zlib (in out)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "libdeflate_zlib_decompress"
                          (function sb-alien:int sb-sys:system-area-pointer
                                    sb-sys:system-area-pointer sb-alien:unsigned-long
                                    sb-sys:system-area-pointer sb-alien:unsigned-long
                                    sb-sys:system-area-pointer))
   *libdeflate* (sb-sys:vector-sap in) (length in)
   (sb-sys:vector-sap out) (length out) (sb-sys:int-sap 0)))

(defun c-decompress (in n zlib)
  "Decode the stream IN into a new octet vector of exactly N bytes."
  (declare (type (simple-array (unsigned-byte 8) (*)) in) (type fixnum n))
  (let ((out (make-array n :element-type '(unsigned-byte 8))))
    (sb-alien:with-alien ((size sb-alien:unsigned-long n))
      (sb-sys:with-pinned-objects (in out)
        (unless (if zlib
                    (if *libdeflate*
                        (zerop (libdeflate-zlib in out))
                        (zerop (z-uncompress (sb-sys:vector-sap out) (sb-alien:addr size)
                                         (sb-sys:vector-sap in) (length in))))
                    (= 1 (brotli-decoder-decompress (length in) (sb-sys:vector-sap in)
                                                    (sb-alien:addr size) (sb-sys:vector-sap out))))
          (error "failed to decode"))
        (unless (= size n)
          (error "produced ~d bytes, expected ~d" size n))))
    out))

(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun now ()
  "Wall-clock seconds, microsecond resolution."
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ sec (/ usec 1d6))))

(defun be (v i n)
  (let ((r 0))
    (dotimes (k n r)
      (setf r (logior (ash r 8) (aref v (+ i k)))))))

(defun woff-tables (v)
  "The zlib-compressed tables of the WOFF 1.0 font V: a list of (stream . size)."
  (loop for i below (be v 12 2)
        for e = (+ 44 (* 20 i))
        for offset = (be v (+ e 4) 4)
        for comp = (be v (+ e 8) 4)
        for orig = (be v (+ e 12) 4)
        when (< comp orig)
          collect (cons (subseq v offset (+ offset comp)) orig)))

(defun job (path)
  (let ((v (slurp path)))
    (flet ((size ()
             (with-open-file (s (make-pathname :type "size" :defaults path))
               (parse-integer (read-line s)))))
      (cond
        ((string= (pathname-type path) "woff")
         (let ((tables (woff-tables v)))
           (values (lambda ()
                     (loop for (z . n) in tables
                           do (c-decompress z n t)))
                   (reduce #'+ tables :key #'cdr))))
        ((string= (pathname-type path) "z")
         (let ((n (size)))
           (values (lambda () (c-decompress v n t)) n)))
        (t
         (let ((n (size)))
           (values (lambda () (c-decompress v n nil)) n)))))))

(defun bench (path &key (rounds 5) (target 100000000))
  (multiple-value-bind (fn n) (job path)
    (let ((reps (max 10 (floor target n)))
          (best 0d0))
      (funcall fn)                                      ; warm-up and check
      (dotimes (k rounds (values best n))
        (let ((t0 (now)))
          (dotimes (i reps)
            (funcall fn))
          (setf best (max best (/ (* reps n) 1d6 (- (now) t0)))))))))

(let ((dir (merge-pathnames "data/" (directory-namestring *load-truename*))))
  (format t "~&~32a ~9a ~9a ~8a~%" "stream" "in B" "out B" "MB/s")
  ;; ONLY=text in the environment limits the run to files whose name contains it
  (dolist (path (remove-if-not
                 (lambda (p)
                   (search (or (sb-ext:posix-getenv "ONLY") "") (file-namestring p)))
                 (sort (append (directory (merge-pathnames "*.br" dir))
                               (directory (merge-pathnames "*.z" dir))
                               (directory (merge-pathnames "*.woff" dir)))
                       #'string< :key #'namestring)))
    (multiple-value-bind (mbs n) (bench path)
      (format t "~&~32a ~9d ~9d ~8,1f~%"
              (format nil "~a.~a" (pathname-name path) (pathname-type path))
              (with-open-file (s path) (file-length s)) n mbs))))
