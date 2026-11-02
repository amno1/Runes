;;;; PROTOTYPE, kept for the record: has known bugs (see ../README.md, "Bugs found on the way"). Use ../0001-retarget-result-moves.patch.

(in-package "SB-C")

(defvar *retarget-result-moves* t)
(defvar *retargeted-moves* 0)
(defvar *retarget-rules* (list 1 2))

;;; Post-pack retargeting of single-use temporaries next to a register move.
;;;
;;; (1) VOP ... => TMP[A];  MOVE TMP => DEST[B]
;;;     B holds nothing live between the VOP and the MOVE (the MOVE overwrites
;;;     it and nothing runs in between), so TMP can be put in B, unless the VOP
;;;     itself reads B through an argument other than one that targets the
;;;     result (x :target r, which already expects to share), or uses B for a
;;;     temporary.  Blocks are walked backwards so that chains of moves
;;;     collapse.
;;; (2) MOVE SRC[B] => TMP[A];  VOP TMP ... => R[B]
;;;     with TMP's argument targeting R: the VOP clobbers B anyway, so TMP can
;;;     be put in B under the same conditions.
;;; Either way the MOVE becomes B => B, which DELETE-NO-OP-VOPS removes.
(defun %same-reg-p (tn as)
  (and (tn-offset tn)
       (eq (sc-sb (tn-sc tn)) (sc-sb (tn-sc as)))
       (= (tn-offset tn) (tn-offset as))))

(defun vop-uses-reg-p (vop reg except-arg)
  "Whether VOP reads REG's location through an argument other than EXCEPT-ARG
or its load TN, or uses it for a temporary."
  (or (loop for arg = (vop-args vop) then (tn-ref-across arg)
            while arg
            thereis (or (and (neq arg except-arg) (%same-reg-p (tn-ref-tn arg) reg))
                        (and (tn-ref-load-tn arg) (%same-reg-p (tn-ref-load-tn arg) reg))))
      (loop for temp = (vop-temps vop) then (tn-ref-across temp)
            while temp
            thereis (%same-reg-p (tn-ref-tn temp) reg))))

(defun retargetable-tmp-p (tmp reg)
  (and (very-temporary-p tmp)
       (eq (tn-kind tmp) :normal)
       (compatible-move-p tmp reg)
       (eq (sb-kind (sc-sb (tn-sc tmp))) :finite)
       (tn-offset tmp) (tn-offset reg)
       (/= (tn-offset tmp) (tn-offset reg))))

(defun plain-move-p (vop)
  (and vop (eq (vop-info-move-vop-p (vop-info vop)) :move)
       (null (tn-ref-load-tn (vop-args vop)))
       (null (tn-ref-load-tn (vop-results vop)))))

(defun retarget-result-moves (component)
  (loop for block = (block-info (component-head component)) then (ir2-block-next block) while block do
    (do ((vop (ir2-block-last-vop block) (vop-prev vop)))
        ((null vop))
      (let ((info (vop-info vop)))
        (unless (vop-info-save-p info)
          ;; (1)
          (let ((move (vop-next vop))
                (result (vop-results vop)))
            (when (and (member 1 *retarget-rules*) (plain-move-p move) result
                       (not (tn-ref-across result))
                       (null (tn-ref-load-tn result))
                       (eq (tn-ref-tn (vop-args move)) (tn-ref-tn result)))
              (let ((tmp (tn-ref-tn result))
                    (dest (tn-ref-tn (vop-results move))))
                (when (and (retargetable-tmp-p tmp dest)
                           (not (vop-uses-reg-p
                                 vop dest
                                 (loop for arg = (vop-args vop) then (tn-ref-across arg)
                                       while arg
                                       when (eq (tn-ref-target arg) result) return arg))))
                  (setf (tn-offset tmp) (tn-offset dest))
                  (incf *retargeted-moves*)))))
          ;; (2)
          (let ((move (vop-prev vop))
                (result (vop-results vop)))
            (when (and (member 2 *retarget-rules*) (plain-move-p move) result
                       (not (tn-ref-across result))
                       (null (tn-ref-load-tn result)))
              (let* ((tmp (tn-ref-tn (vop-results move)))
                     (src (tn-ref-tn (vop-args move)))
                     (r (tn-ref-tn result))
                     (arg (loop for arg = (vop-args vop) then (tn-ref-across arg)
                                while arg
                                when (eq (tn-ref-tn arg) tmp) return arg)))
                (when (and arg
                           (eq (tn-ref-target arg) result)
                           (%same-reg-p src r)
                           (compatible-move-p r tmp)
                           (retargetable-tmp-p tmp r)
                           (not (vop-uses-reg-p vop r arg)))
                  (setf (tn-offset tmp) (tn-offset r))
                  (incf *retargeted-moves*))))))))))

(sb-int:encapsulate 'ir2-optimize 'retarget
  (lambda (f component &optional stage)
    (when (and *retarget-result-moves* (eq stage 'regalloc))
      (retarget-result-moves component))
    (funcall f component stage)))
