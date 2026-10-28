;;;; tests/bench/literal-pairs.lisp — could two literals per lookup pay?
;;;;
;;;; A multi-symbol table (as ISA-L's) decodes two literals with one root
;;;; lookup when both codes fit in the root.  For every zlib stream in data/
;;;; (X.z, and the zlib tables of X.woff) this prints the share of output bytes
;;;; that are literals, and the share that greedy pairs of consecutive literals
;;;; would cover with a 10-, 11- and 12-bit root.
;;;;
;;;; The decoder is not changed: this reads INFLATE-BLOCK's definition from
;;;; src/inflate.lisp, replaces its FINISH-SYMBOL macro by one that also
;;;; records each symbol and its code length, and compiles the result in
;;;; place of the loaded INFLATE-BLOCK.
;;;;
;;;;   sbcl --script literal-pairs.lisp    (run from tests/bench)

(require :asdf)

(push (merge-pathnames "../../" (directory-namestring *load-truename*)) asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning)) (asdf:load-system "decompress"))

(in-package #:decompress)

;;; [0] literals, [1..3] pairs fitting in 10/11/12 bits, [4..6] the code
;;; length of a literal still waiting for a partner (0: none)
(sb-ext:defglobal **pair-stats** (make-array 7 :initial-element 0))

(defun record-symbol (sym len)
  (let ((v **pair-stats**))
    (cond ((< sym 256)
           (incf (svref v 0))
           (loop for k below 3
                 for root = (+ 10 k)
                 for pending = (svref v (+ 4 k))
                 do (if (and (> pending 0) (<= (+ pending len) root))
                        (setf (svref v (+ 1 k)) (1+ (svref v (+ 1 k)))
                              (svref v (+ 4 k)) 0)
                        (setf (svref v (+ 4 k)) (if (< len root) len 0)))))
          (t (fill v 0 :start 4)))))

(defparameter *recording-finish-symbol*
  ;; FINISH-SYMBOL as in inflate.lisp, plus the RECORD-SYMBOL call; a code
  ;; through a subtable is the root's bits plus the subtable entry's
  (read-from-string
   "(finish-symbol (entry)
      `(let ((%e ,entry) (%len 0))
         (declare (type (unsigned-byte 32) %e) (type fixnum %len))
         (setf %len (logand %e 31))
         (when (>= (logand %e 31) 16)
           (br-drop #.+lit-root-bits+)
           (setf %e (aref ltab (+ (ash %e -5) (low-bits val (- (logand %e 31) 16)))))
           (setf %len (+ #.+lit-root-bits+ (logand %e 31))))
         (record-symbol (ash %e -5) %len)
         (setf saved val)
         (br-drop (logand %e 31))
         (setf sym (ash %e -5))))"))

(defun replace-finish-symbol (form)
  "FORM with the macrolet binding of FINISH-SYMBOL replaced; signals an error
if there is none (inflate.lisp changed)."
  (let ((found nil))
    (labels ((walk (x)
               (cond ((and (consp x) (eq (car x) 'finish-symbol) (equal (cadr x) '(entry)))
                      (setf found t)
                      *recording-finish-symbol*)
                     ((consp x) (cons (walk (car x)) (walk (cdr x))))
                     (t x))))
      (prog1 (walk form)
        (unless found (error "No FINISH-SYMBOL macro in INFLATE-BLOCK"))))))

(let ((source (asdf:system-relative-pathname "decompress" "src/inflate.lisp"))
      (*package* (find-package '#:decompress)))
  (with-open-file (s source)
    (loop for form = (read s nil s)
          until (eq form s)
          when (and (consp form) (eq (car form) 'defun) (eq (cadr form) 'inflate-block))
            do (handler-bind ((warning #'muffle-warning))
                 (funcall (compile nil `(lambda () ,(replace-finish-symbol form)))))
               (return)
          finally (error "No INFLATE-BLOCK in ~a" source))))

(in-package #:cl-user)

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

(let ((dir (merge-pathnames "data/" (directory-namestring *load-truename*)))
      (stats decompress::**pair-stats**))
  (format t "~&~32a ~9a ~9a  ~19a~%" "stream" "out B" "literals" "bytes in pairs, root 10 / 11 / 12")
  (dolist (path (sort (append (directory (merge-pathnames "*.z" dir))
                              (directory (merge-pathnames "*.woff" dir)))
                      #'string< :key #'namestring))
    (let ((v (slurp path)) (out 0))
      (fill stats 0)
      (if (string= (pathname-type path) "woff")
          (loop for (z . n) in (woff-tables v)
                do (decompress:zlib-decompress z :size-hint n)
                   (incf out n))
          (incf out (length (decompress:zlib-decompress v))))
      (flet ((pct (pairs) (/ (* 200.0 pairs) out)))
        (format t "~&~32a ~9d ~8,1f%  ~5,1f% ~5,1f% ~5,1f%~%"
                (format nil "~a.~a" (pathname-name path) (pathname-type path))
                out (/ (* 100.0 (svref stats 0)) out)
                (pct (svref stats 1)) (pct (svref stats 2)) (pct (svref stats 3)))))))
