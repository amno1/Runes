;;;; tests/bench/bench.lisp — decode speed of decompress.
;;;;
;;;; Decodes every stream in data/ (made by make-data.sh) repeatedly, ~100 MB
;;;; of output per round, and prints the best of 5 rounds in MB/s of output:
;;;;   X.br    a Brotli stream, decoded with DECOMPRESS;
;;;;   X.z     a zlib stream, decoded with ZLIB-DECOMPRESS;
;;;;   X.woff  a WOFF 1.0 font: every zlib-compressed table is decoded.
;;;; Each stream gets its decoded size as :SIZE-HINT, as a font loader would
;;;; (data/X.size, or the table's origLength).  Compare with bench-c.lisp
;;;; (libbrotli and libz) on the same files.
;;;;
;;;;   sbcl --script bench.lisp            (run from tests/bench)
;;;;   ONLY=.z sbcl --script bench.lisp    (only files whose name contains .z)

(require :asdf)

(push (merge-pathnames "../../" (directory-namestring *load-truename*)) asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning)) (asdf:load-system "decompress"))

(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun now ()
  "Wall-clock seconds, microsecond resolution (GET-INTERNAL-REAL-TIME ticks in
milliseconds here, too coarse for ~100 ms rounds)."
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
  "(values decode-thunk decoded-bytes) for one data file."
  (let ((v (slurp path)))
    (flet ((size ()
             (with-open-file (s (make-pathname :type "size" :defaults path))
               (parse-integer (read-line s)))))
      (cond
        ((string= (pathname-type path) "woff")
         (let ((tables (woff-tables v)))
           (values (lambda ()
                     (loop for (z . n) in tables
                           do (decompress:zlib-decompress z :size-hint n)))
                   (reduce #'+ tables :key #'cdr))))
        ((string= (pathname-type path) "z")
         (let ((n (size)))
           (assert (= n (length (decompress:zlib-decompress v :size-hint n))))
           (values (lambda () (decompress:zlib-decompress v :size-hint n)) n)))
        (t
         (let ((n (size)))
           (assert (= n (length (decompress:decompress v :size-hint n))))
           (values (lambda () (decompress:decompress v :size-hint n)) n)))))))

(defun bench (path &key (rounds 5) (target 100000000))
  (multiple-value-bind (fn n) (job path)
    (let ((reps (max 10 (floor target n)))
          (best 0d0))
      (funcall fn)                                      ; warm-up
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
