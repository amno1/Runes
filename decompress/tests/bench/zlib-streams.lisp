;;;; tests/bench/zlib-streams.lisp — zlib-compress files with libz for benchmarking.
;;;;
;;;;   sbcl --script zlib-streams.lisp LEVEL OUT.z IN     (run from tests/bench)
;;;;
;;;; Writes OUT.z (compress2 at LEVEL) and OUT.size (the input size, passed as
;;;; :SIZE-HINT by bench.lisp).  Needs libz; LIBZ overrides its path.

(sb-alien:load-shared-object (or (sb-ext:posix-getenv "LIBZ") "libz.so"))

(sb-alien:define-alien-routine ("compress2" compress2) sb-alien:int
  (dest sb-sys:system-area-pointer) (dest-len (* sb-alien:unsigned-long))
  (source sb-sys:system-area-pointer) (source-len sb-alien:unsigned-long)
  (level sb-alien:int))

(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(destructuring-bind (level out in) (rest sb-ext:*posix-argv*)
  (let* ((data (slurp in))
         (cap (+ 1024 (* 2 (length data))))
         (z (make-array cap :element-type '(unsigned-byte 8))))
    (sb-alien:with-alien ((n sb-alien:unsigned-long cap))
      (sb-sys:with-pinned-objects (data z)
        (unless (zerop (compress2 (sb-sys:vector-sap z) (sb-alien:addr n)
                                  (sb-sys:vector-sap data) (length data) (parse-integer level)))
          (error "compress2 failed")))
      (with-open-file (s out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence z s :end n))
      (with-open-file (s (make-pathname :type "size" :defaults out) :direction :output :if-exists :supersede)
        (format s "~d~%" (length data)))
      (format t "~&~a: ~d B -> ~d B~%" out (length data) n))))
