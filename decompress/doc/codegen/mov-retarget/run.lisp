;;;; Decode one bench file repeatedly (~300 MB of output) and print MB/s.
;;;; The system must already be compiled (see ab.sh), so the process does no
;;;; compiling and `perf stat` counts load + decode only.
;;;;   FILE=SourceCodePro-Regular.woff BENCH=1 sbcl --script run.lisp
(require :asdf)
(defvar *root* (merge-pathnames "../../../" (directory-namestring *load-truename*)))
(push *root* asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "decompress"))
;; JOB and NOW from the regular bench, without its main loop
(let ((src (with-open-file (s (merge-pathnames "tests/bench/bench.lisp" *root*))
             (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s))))))
  (with-input-from-string (in (subseq src (search "(defun slurp" src) (search "(let ((dir" src)))
    (loop for form = (read in nil in) until (eq form in) do (eval form))))
(let ((f (merge-pathnames (concatenate 'string "tests/bench/data/"
                                       (or (sb-ext:posix-getenv "FILE") "SourceCodePro-Regular.woff"))
                          *root*)))
  (when (plusp (length (or (sb-ext:posix-getenv "BENCH") "")))
    (multiple-value-bind (fn n) (job f)
      (let ((reps (floor 300000000 n)) (t0 (now)))
        (dotimes (i reps) (funcall fn))
        (format t "~&MBS ~,1f~%" (/ (* reps n) 1d6 (- (now) t0)))))))
