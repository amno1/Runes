;;;; tests/bench/perf-compare.lisp — CPU counters per output byte: decompress
;;;; against libdeflate.
;;;;
;;;; For every zlib stream in data/ (X.z, and the zlib tables of X.woff) and
;;;; each decoder, runs this file again as a child under `perf stat`, decoding
;;;; ~200 MB of output, and once more decoding nothing; the difference is the
;;;; decoding alone (loading, compiling and the warm-up decode cancel out).
;;;; Prints cycles, instructions and branch misses per output byte, and
;;;; instructions per cycle.  Both decoders get the exact output size and a
;;;; fresh output vector per call, as in bench.lisp.
;;;;
;;;;   sbcl --script perf-compare.lisp            (run from tests/bench)
;;;;   ONLY=.woff sbcl --script perf-compare.lisp (only files whose name contains .woff)
;;;;
;;;; Needs perf and libdeflate; LIBDEFLATE overrides its path.

(require :asdf)

(push (merge-pathnames "../../" (directory-namestring *load-truename*)) asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning)) (asdf:load-system "decompress"))

(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

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

(defun streams (path)
  "The zlib streams of PATH: a list of (stream . decoded size)."
  (let ((v (slurp path)))
    (if (string= (pathname-type path) "woff")
        (woff-tables v)
        (list (cons v (length (decompress:zlib-decompress v)))))))

;;; ---- the child: decode ROUNDS times with one decoder ---------------------

(defun libdeflate-decoder ()
  (sb-alien:load-shared-object (or (sb-ext:posix-getenv "LIBDEFLATE") "libdeflate.so"))
  (let ((d (sb-alien:alien-funcall
            (sb-alien:extern-alien "libdeflate_alloc_decompressor"
                                   (function sb-sys:system-area-pointer)))))
    (lambda (in n)
      (let ((out (make-array n :element-type '(unsigned-byte 8))))
        (sb-sys:with-pinned-objects (in out)
          (unless (zerop (sb-alien:alien-funcall
                          (sb-alien:extern-alien
                           "libdeflate_zlib_decompress"
                           (function sb-alien:int sb-sys:system-area-pointer
                                     sb-sys:system-area-pointer sb-alien:unsigned-long
                                     sb-sys:system-area-pointer sb-alien:unsigned-long
                                     sb-sys:system-area-pointer))
                          d (sb-sys:vector-sap in) (length in)
                          (sb-sys:vector-sap out) n (sb-sys:int-sap 0)))
            (error "libdeflate failed")))
        out))))

(defun child (decoder file rounds)
  (let ((decode (if (string= decoder "libdeflate")
                    (libdeflate-decoder)
                    (lambda (in n) (decompress:zlib-decompress in :size-hint n))))
        (streams (streams file)))
    (loop for (z . n) in streams do (funcall decode z n)) ; warm-up, in both runs
    (sb-ext:gc :full t)
    (loop repeat rounds
          do (loop for (z . n) in streams do (funcall decode z n)))))

;;; ---- the parent: run the children under perf -------------------------------

(defparameter *events* '("cycles" "instructions" "branch-misses"))

(defun counters (decoder file rounds)
  "perf's counts for one child run, in *EVENTS* order."
  (let* ((err (make-string-output-stream))
         (args (append (list "stat" "-x," "-e" (format nil "~{~a~^,~}" *events*) "--"
                             (namestring sb-ext:*runtime-pathname*) "--core" (namestring sb-ext:*core-pathname*)
                             "--script" (namestring *load-truename*)
                             "--child" decoder (namestring file) (princ-to-string rounds))))
         (p (sb-ext:run-program "perf" args :search t :output nil :error err)))
    (unless (zerop (sb-ext:process-exit-code p))
      (error "perf failed: ~a" (get-output-stream-string err)))
    (let ((lines (with-input-from-string (s (get-output-stream-string err))
                   (loop for l = (read-line s nil) while l collect l))))
      (loop for event in *events*
            collect (let ((line (find-if (lambda (l) (search (format nil ",~a" event) l)) lines)))
                      (unless line (error "no ~a count from perf" event))
                      (parse-integer line :end (position #\, line)))))))

(defun compare ()
  (let ((dir (merge-pathnames "data/" (directory-namestring *load-truename*))))
    (format t "~&~28a ~10a ~9a ~9a ~6a ~12a~%"
            "stream" "decoder" "cycles/B" "instr/B" "IPC" "misses/KB")
    (dolist (path (remove-if-not
                   (lambda (p) (search (or (sb-ext:posix-getenv "ONLY") "") (file-namestring p)))
                   (sort (append (directory (merge-pathnames "*.z" dir))
                                 (directory (merge-pathnames "*.woff" dir)))
                         #'string< :key #'namestring)))
      (let* ((out (reduce #'+ (streams path) :key #'cdr))
             (rounds (ceiling 200000000 out)))
        (dolist (decoder '("decompress" "libdeflate"))
          (destructuring-bind (cycles instructions misses)
              (mapcar #'- (counters decoder path rounds) (counters decoder path 0))
            (let ((bytes (* rounds out)))
              (format t "~&~28a ~10a ~9,2f ~9,2f ~6,2f ~12,2f~%"
                      (format nil "~a.~a" (pathname-name path) (pathname-type path))
                      decoder (/ cycles bytes) (/ instructions bytes)
                      (/ instructions cycles) (/ (* 1024.0 misses) bytes)))))))))

(let ((args (member "--child" sb-ext:*posix-argv* :test #'string=)))
  (if args
      (destructuring-bind (decoder file rounds) (rest args)
        (child decoder file (parse-integer rounds)))
      (compare)))
