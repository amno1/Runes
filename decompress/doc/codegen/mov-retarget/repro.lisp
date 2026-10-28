;;;; Minimal reproduction: a bit-reader update loop.  With stock SBCL each
;;;; modular update of BITS/ACC goes through a temporary register and is copied
;;;; back (MOV RAX,RBX; OR RAX,56; MOV RBX,RAX); with the retargeting pass
;;;; it is one instruction (OR RBX,56).
;;;;   sbcl --script repro.lisp
(defun f (v n a)
  (declare (optimize speed (safety 0)) (type (simple-array (unsigned-byte 32) (*)) a)
           (type sb-ext:word v n))
  (let ((bits n) (val v) (acc 0))
    (declare (type sb-ext:word bits val acc))
    (dotimes (i 1000)
      (let ((e (aref a (ldb (byte 10 0) val))))
        (setf val (ash val (- (logand e 31)))
              bits (ldb (byte 64 0) (- bits (logand e 31))))
        (setf bits (ldb (byte 64 0) (logior bits 56)))
        (setf acc (ldb (byte 64 0) (+ acc e)))))
    (values val bits acc)))
(disassemble 'f)
