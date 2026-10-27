;;;; tests/bench/profile.lisp — sb-sprof CPU profile of decoding one stream.
;;;;
;;;;   sbcl --script profile.lisp data/RedHatText-Regular.br   (from tests/bench)
;;;;
;;;; For instruction-level detail use perf on the running process and map the
;;;; sampled addresses onto (disassemble 'decompress::command-loop/big-window).

(require :asdf)

(push (merge-pathnames "../../" (directory-namestring *load-truename*)) asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning)) (asdf:load-system "decompress"))

(require :sb-sprof)

(let* ((br (second sb-ext:*posix-argv*))
       (v (with-open-file (s br :element-type '(unsigned-byte 8))
            (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v)))
       (n (length (decompress:decompress v)))
       (reps (max 20 (floor 300000000 n))))   ; ~300 MB of output: a few seconds
  (sb-sprof:with-profiling (:max-samples 200000 :mode :cpu :report :flat)
    (dotimes (i reps)
      (decompress:decompress v :size-hint n))))
