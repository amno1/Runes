;;;; tests/bench/woff2-streams.lisp — pull the Brotli stream out of WOFF2 files.
;;;;
;;;; For each X.woff2 given, writes X.br (the compressed font data stream) and
;;;; X.size (its decompressed size, from the table directory: what a WOFF2
;;;; loader passes as DECOMPRESS's :SIZE-HINT), and checks that decompress
;;;; decodes the stream to exactly that size.
;;;;
;;;;   sbcl --script woff2-streams.lisp data/*.woff2      (run from tests/bench)

(require :asdf)

(push (merge-pathnames "../../" (directory-namestring *load-truename*)) asdf:*central-registry*)

(handler-bind ((warning #'muffle-warning)) (asdf:load-system "decompress"))

(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun be (v i n)
  (let ((r 0))
    (dotimes (k n r)
      (setf r (logior (ash r 8) (aref v (+ i k)))))))

(defun base128 (v i)
  "UIntBase128 at I -> (values value next-index)."
  (let ((r 0))
    (loop (let ((b (aref v i)))
            (incf i)
            (setf r (logior (ash r 7) (logand b 127)))
            (unless (logbitp 7 b) (return (values r i)))))))

(defun woff2-stream (path)
  "(values stream decompressed-size) of the WOFF2 file at PATH."
  (let* ((v (slurp path))
         (ntables (be v 12 2))
         (comp (be v 20 4))
         (i 48) (total 0))
    (unless (= (be v 0 4) #x774F4632) (error "~a: not WOFF2" path))   ; 'wOF2'
    (dotimes (k ntables)
      (let* ((flags (aref v i)) (tag (logand flags 63)) (version (ash flags -6)))
        (incf i)
        (when (= tag 63) (incf i 4))                    ; explicit tag
        (multiple-value-bind (orig next) (base128 v i)
          (setf i next)
          ;; glyf/loca are transformed with version 0, every other table with
          ;; a nonzero version; transformed tables carry a transformLength
          (if (if (member tag '(10 11)) (= version 0) (/= version 0))
              (multiple-value-bind (tlen next) (base128 v i) (setf i next) (incf total tlen))
              (incf total orig)))))
    (values (subseq v i (+ i comp)) total)))

(dolist (arg (rest sb-ext:*posix-argv*))
  (multiple-value-bind (stream size) (woff2-stream arg)
    (let ((out (make-pathname :type "br" :defaults arg)))
      (with-open-file (s out :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence stream s))
      (with-open-file (s (make-pathname :type "size" :defaults arg) :direction :output :if-exists :supersede)
        (format s "~d~%" size))
      (assert (= size (length (decompress:decompress stream :size-hint size))))
      (format t "~&~a: stream ~d B -> ~d B~%" (file-namestring arg) (length stream) size))))
