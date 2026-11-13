;;;; tests/bench/3x3.lisp — 3x3 comparison of three decoders on the same cases.
;;;;
;;;; Same method as el-fmt's bench/3x3.lisp: 3 sets of 3 runs; per set the best
;;;; (highest MB/s) of the 3 runs for each case; the median of the 3 set bests
;;;; is the reported number.  Each run measures all three implementations back
;;;; to back, rotating which goes first, so drift of a noisy machine hits all of
;;;; them.  Every implementation is its own process (3x3-worker.lisp).
;;;;
;;;;   sbcl --script 3x3.lisp UNPIN_DIR [OUTPUT-FILE]
;;;;
;;;;   pinned     decompress in PINNED_DIR (default: this checkout) as it is
;;;;   unpinned   decompress in UNPIN_DIR, loaded with :DECOMPRESS-UNPIN-NIL on
;;;;              *FEATURES*: a copy whose hot functions say (SB-EXT:UNPIN-NIL)
;;;;   C          libdeflate (zlib) and libbrotlidec (Brotli), through SB-ALIEN
;;;;
;;;; LISP is the SBCL that runs the workers (default: the sbcl-unpin-nil build,
;;;; the only one that knows the declaration); ROUNDS, TARGET and ONLY are passed
;;;; on; CPU=n runs the workers under taskset -c n.  The decoded bytes of every
;;;; case are checksummed and must agree between the implementations.

(defparameter *sets* 3)
(defparameter *runs* 3)
(defparameter *here* (directory-namestring *load-truename*))
(defparameter *lisp*
  (or (sb-ext:posix-getenv "LISP")
      (namestring (merge-pathnames "repos/sbcl-unpin-nil/run-sbcl.sh" (user-homedir-pathname)))))
(defparameter *pinned-dir*
  (or (sb-ext:posix-getenv "PINNED_DIR")
      (namestring (truename (merge-pathnames "../../" *here*)))))

(defun split-tabs (line)
  (loop with start = 0
        for pos = (position #\Tab line :start start)
        collect (subseq line start pos)
        while pos do (setf start (1+ pos))))

(defun run-worker (impl dir unpin)
  "Run one worker: (values table names) with table name -> (mb/s checksum)."
  (let* ((cpu (sb-ext:posix-getenv "CPU"))
         (program (if cpu "taskset" *lisp*))
         (args (append (when cpu (list "-c" cpu *lisp*))
                       (list "--noinform" "--non-interactive" "--no-userinit"
                             "--load" (concatenate 'string *here* "3x3-worker.lisp"))))
         (env (append (list (format nil "IMPL=~a" (if (eq impl :c) "c" "lisp"))
                            (format nil "DIR=~a" (or dir ""))
                            (format nil "UNPIN=~a" (if unpin "1" "0")))
                      (sb-ext:posix-environ)))
         (errors (make-string-output-stream))
         (output (with-output-to-string (out)
                   (sb-ext:run-program program args :output out :error errors
                                                    :search t :environment env)))
         (table (make-hash-table :test 'equal))
         (names '()))
    (with-input-from-string (in output)
      (loop for line = (read-line in nil) while line
            do (let ((parts (split-tabs line)))
                 (when (= (length parts) 3)
                   (push (first parts) names)
                   (setf (gethash (first parts) table)
                         (list (let ((*read-default-float-format* 'double-float))
                                 (read-from-string (second parts)))
                               (third parts)))))))
    (when (zerop (hash-table-count table))
      (error "the ~a worker (~a) printed no results; its stderr:~%~a"
             impl (or dir "libdeflate") (get-output-stream-string errors)))
    (values table (nreverse names))))

(defun median (numbers)
  (nth (floor (length numbers) 2) (sort (copy-list numbers) #'<)))

(defun main (unpin-dir out-file)
  (let* ((impls (list (list :pinned *pinned-dir* nil)
                      (list :unpinned unpin-dir t)
                      (list :c nil nil)))
         (set-bests (mapcar (lambda (i) (cons (first i) '())) impls))
         (sums (make-hash-table :test 'equal))
         (names '()))
    (dotimes (s *sets*)
      (let ((runs (mapcar (lambda (i) (cons (first i) '())) impls)))
        (dotimes (r *runs*)
          (format *error-output* "  set ~D/~D run ~D/~D~%" (1+ s) *sets* (1+ r) *runs*)
          (let ((order (let ((k (mod (+ s r) 3)))
                         (append (nthcdr k impls) (subseq impls 0 k)))))
            (dolist (impl order)
              (multiple-value-bind (table order) (apply #'run-worker impl)
                (setf names order)
                (push table (cdr (assoc (first impl) runs)))
                (maphash (lambda (name v)
                           (let ((key name))
                             (let ((old (gethash key sums)))
                               (if (and old (string/= old (second v)))
                                   (error "output of ~a differs between implementations" name)
                                   (setf (gethash key sums) (second v))))))
                         table)))))
        (dolist (impl impls)
          (push (let ((best (make-hash-table :test 'equal)))
                  (dolist (name names best)
                    (setf (gethash name best)
                          (reduce #'max (mapcar (lambda (table) (first (gethash name table)))
                                                (cdr (assoc (first impl) runs)))))))
                (cdr (assoc (first impl) set-bests))))))
    (flet ((med (impl name)
             (let ((vals (mapcar (lambda (m) (gethash name m)) (cdr (assoc impl set-bests)))))
               (values (median vals)
                       (* 100 (/ (- (reduce #'max vals) (reduce #'min vals)) (median vals)))))))
      (let ((lines
              (list* "decompress, 3 sets x 3 runs: median of the per-set best (MB/s of output)"
                     (format nil "pinned:   ~a" *pinned-dir*)
                     (format nil "unpinned: ~a (:decompress-unpin-nil)" unpin-dir)
                     (format nil "C:        libdeflate (zlib), libbrotlidec (Brotli)")
                     (format nil "lisp:     ~a" *lisp*)
                     ""
                     (format nil "~32a ~9a ~9a ~9a ~9a ~9a ~8a" "case" "pinned" "unpinned" "C"
                             "unp/pin" "unp/C" "spread%")
                     (loop for name in names
                           collect (multiple-value-bind (p sp) (med :pinned name)
                                     (multiple-value-bind (u su) (med :unpinned name)
                                       (multiple-value-bind (c sc) (med :c name)
                                         (format nil "~32a ~9,1f ~9,1f ~9,1f ~8,2fx ~8,2fx ~8,1f"
                                                 name p u c (/ u p) (/ u c) (max sp su sc)))))))))
        (let ((text (format nil "~{~a~%~}~%unp/pin = unpinned / pinned (above 1: unpinned is faster).~%spread% = largest difference between the 3 set bests of a case, relative to its median, for the noisiest of the three columns.~%" lines)))
          (write-string text)
          (when out-file
            (with-open-file (s out-file :direction :output :if-exists :supersede)
              (write-string text s))))))))

(let ((args (cdr sb-ext:*posix-argv*)))
  (unless args
    (format *error-output* "usage: sbcl --script 3x3.lisp UNPIN_DIR [OUTPUT-FILE]~%")
    (sb-ext:exit :code 2))
  (main (namestring (truename (first args))) (second args)))
