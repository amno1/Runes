;;; ============================================================================
;;; Vector Projections & Point Movement (zone.rs / graphics.rs)
;;; ============================================================================

(declaim (inline project-point dual-project-point fast-project-x-p fast-project-y-p))

(defun project-point (gs x y)
  "Project 26.6 point (X, Y) onto the current Projection Vector."
  (declare (graphics-state gs) (s32 x y) (optimize (speed 3) (safety 0)))
  (let ((px (gs-proj-x gs))
        (py (gs-proj-y gs)))
    (cond ((and (= px #x4000) (zerop py)) x)
          ((and (zerop px) (= py #x4000)) y)
          (t (f26+ (mul-f2dot14 x px) (mul-f2dot14 y py))))))

(defun dual-project-point (gs x y)
  "Project 26.6 point (X, Y) onto the current Dual Projection Vector."
  (declare (graphics-state gs) (s32 x y) (optimize (speed 3) (safety 0)))
  (let ((dx (gs-dual-x gs))
        (dy (gs-dual-y gs)))
    (cond ((and (= dx #x4000) (zerop dy)) x)
          ((and (zerop dx) (= dy #x4000)) y)
          (t (f26+ (mul-f2dot14 x dx) (mul-f2dot14 y dy))))))

(defun zone-check-point (zone pt)
  (declare (tt-zone zone) (fixnum pt) (optimize (speed 3) (safety 0)))
  (unless (< -1 pt (zone-n-points zone))
    (hint-fail 'tt-hint-invalid-zone (format nil "point index ~D out of range" pt))))

(defun zone-project-cur (gs zone pt)
  (declare (graphics-state gs) (tt-zone zone) (fixnum pt))
  (zone-check-point zone pt)
  (project-point gs (aref (zone-cur-x zone) pt) (aref (zone-cur-y zone) pt)))

(defun zone-dual-project-orig (gs zone pt)
  (declare (graphics-state gs) (tt-zone zone) (fixnum pt))
  (zone-check-point zone pt)
  (dual-project-point gs (aref (zone-orig-x zone) pt) (aref (zone-orig-y zone) pt)))

(defun move-zone-point (gs zone zone-id pt distance &key (touch t))
  "Move point PT in ZONE by DISTANCE along the Freedom Vector so its projection
changes by DISTANCE. Respects v40 subpixel backward compatibility rules."
  (declare (graphics-state gs) (tt-zone zone) (type (integer 0 1) zone-id)
           (fixnum pt) (s32 distance) (optimize (speed 3) (safety 0)))
  (zone-check-point zone pt)
  (let* ((fx (gs-free-x gs))
         (fy (gs-free-y gs))
         (fdotp (dot-f2dot14 fx fy (gs-proj-x gs) (gs-proj-y gs))))
    ;; In v40 backward-compatibility mode, X-axis movement in the glyph zone (zone 1)
    ;; is suppressed (except for phantom points in some legacy cases, which Skrifa also blocks).
    ;; Twilight zone (zone 0) is always allowed to move in X!
    (when (not (= fx 0))
      (when (or (= zone-id 0) (not (gs-backward-compatibility gs)))
        (let ((dx (if (and (= fx #x4000) (= fdotp #x4000))
                      distance
                      (if (< (abs fdotp) #x400) ; FreeType minimum dot product threshold
                          0
                          (mul-div distance fx fdotp)))))
          (setf (aref (zone-cur-x zone) pt)
                (f26+ (aref (zone-cur-x zone) pt) dx))))
      (when touch
        (setf (aref (zone-flags zone) pt)
              (logior (aref (zone-flags zone) pt) +point-touched-x+))))
    (when (not (= fy 0))
      (let ((dy (if (and (= fy #x4000) (= fdotp #x4000))
                    distance
                    (if (< (abs fdotp) #x400)
                        0
                        (mul-div distance fy fdotp)))))
        (setf (aref (zone-cur-y zone) pt)
              (f26+ (aref (zone-cur-y zone) pt) dy)))
      (when touch
        (setf (aref (zone-flags zone) pt)
              (logior (aref (zone-flags zone) pt) +point-touched-y+))))))
