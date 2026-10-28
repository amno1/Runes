;;;; tests/bench/profile.lisp — sb-sprof CPU profile of decoding one stream.
;;;;
;;;;   sbcl --script profile.lisp data/RedHatText-Regular.br   (from tests/bench)
;;;;   sbcl --script profile.lisp data/SourceCodePro-Regular.woff
;;;;
;;;; X.br is a Brotli stream, X.z a zlib stream, X.woff a WOFF 1.0 font (all
;;;; its zlib tables).  ~1 GB of output, sampled every 0.2 ms.
;;;;
;;;; For instruction-level detail use perf on the running process and map the
;;;; sampled addresses onto (disassemble 'decompress::command-loop/big-window).

(require :asdf)

(push (merge-pathnames "../../" (directory-namestring *load-truename*)) asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning)) (asdf:load-system "decompress"))

(require :sb-sprof)

(defun be (v i n)
  (loop with x = 0 for k below n
        do (setf x (logior (ash x 8) (aref v (+ i k))))
        finally (return x)))

(defun woff-tables (v)
  "The zlib-compressed tables of the WOFF 1.0 font V: a list of (stream . size)."
  (loop for i below (be v 12 2)
        for e = (+ 44 (* 20 i))
        for offset = (be v (+ e 4) 4)
        for comp = (be v (+ e 8) 4)
        for orig = (be v (+ e 12) 4)
        when (< comp orig)
          collect (cons (subseq v offset (+ offset comp)) orig)))

(let* ((path (second sb-ext:*posix-argv*))
       (v (with-open-file (s path :element-type '(unsigned-byte 8))
            (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v)))
       (type (pathname-type path))
       (streams (cond ((string= type "woff") (woff-tables v))
                      ((string= type "z") (list (cons v (length (decompress:zlib-decompress v)))))
                      (t (list (cons v (length (decompress:decompress v)))))))
       (n (reduce #'+ streams :key #'cdr))
       (reps (max 20 (floor 1000000000 n))))
  (sb-sprof:with-profiling (:max-samples 400000 :sample-interval 0.0002 :mode :cpu :report :flat)
    (dotimes (i reps)
      (loop for (z . size) in streams
            do (if (string= type "br")
                   (decompress:decompress z :size-hint size)
                   (decompress:zlib-decompress z :size-hint size))))))
