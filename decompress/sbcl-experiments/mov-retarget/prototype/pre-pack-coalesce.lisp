;;;; PROTOTYPE, kept for the record: has known bugs (see ../README.md, "Bugs found on the way"). Use ../0001-retarget-result-moves.patch.

(in-package "SB-C")

(defvar *coalesce-result-moves* t)
(defvar *coalesced-moves* 0)
(defvar *why* nil)

;;; VOP ... => TMP followed by MOVE TMP => DEST, where TMP is written and read
;;; once and has DEST's SC: make the VOP write DEST and delete the MOVE.  DEST
;;; may be an argument of the VOP only when that argument targets the result
;;; (x :target r), so the VOP already expects them to share a register.
(defun coalesce-result-moves (component)
  (loop for block = (block-info (component-head component)) then (ir2-block-next block) while block do
    (do ((vop (ir2-block-start-vop block) (vop-next vop)))
        ((null vop))
      (let ((move (vop-next vop))
            (result (vop-results vop)))
        (when (and *why* move (vop-info-move-vop-p (vop-info move)))
          (let ((r (and result (tn-ref-tn result))) (d (tn-ref-tn (vop-results move))))
            (format t "~&mv=~s lt=~s tg=~s ~a -> ~a  vt=~a kinds=~a/~a sc=~a/~a same=~a~%" (vop-info-move-vop-p (vop-info move)) (and result (tn-ref-load-tn result)) (loop for a = (vop-args vop) then (tn-ref-across a) while a collect (list (and (tn-ref-target a) (eq (tn-ref-target a) result)) (eq (tn-ref-tn a) d))) (vop-info-name (vop-info vop)) (vop-info-name (vop-info move))
                    (and r (very-temporary-p r)) (and r (tn-kind r)) (tn-kind d) (and r (sc-name (tn-sc r))) (and (tn-sc d) (sc-name (tn-sc d)))
                    (and r (eq r (tn-ref-tn (vop-args move)))))))
        (when (and move result
                   (not (tn-ref-across result))
                   (eq (vop-info-move-vop-p (vop-info move)) :move)
                   (not (vop-info-move-vop-p (vop-info vop)))
                   (eq (tn-ref-tn (vop-args move)) (tn-ref-tn result)))
          (let ((tmp (tn-ref-tn result))
                (dest (tn-ref-tn (vop-results move))))
            (when (and (very-temporary-p tmp)
                       (eq (tn-kind tmp) :normal)
                       (null (tn-offset tmp))
                       (eq (tn-kind dest) :normal)
                       (eq (tn-sc tmp) (tn-sc dest))
                       (null (tn-ref-load-tn result))
                       (loop for arg = (vop-args vop) then (tn-ref-across arg)
                             while arg
                             always (or (neq (tn-ref-tn arg) dest)
                                        (eq (tn-ref-target arg) result)))
                       (<= (loop for arg = (vop-args vop) then (tn-ref-across arg)
                                 while arg count (eq (tn-ref-tn arg) dest))
                           1))
              (change-tn-ref-tn result dest)
              (incf *coalesced-moves*)
              (delete-vop move))))))))

(sb-int:encapsulate 'ir2-optimize 'coalesce
  (lambda (f component &optional stage)
    (prog1 (funcall f component stage)
      (when (and *coalesce-result-moves* (eq stage 'select-representations))
        (coalesce-result-moves component)))))
(defvar *why* nil)
