;;;; projections.lisp — TrueType hinting vector projections and point movement
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:runes/otf/tt-hint)

;;; ============================================================================
;;; Vector Projections & Point Movement (zone.rs / graphics.rs)
;;; ============================================================================

(declaim (inline project-point dual-project-point zone-check-point))

(defun project-point (gs x y)
  (declare (graphics-state gs) (s32 x y) (optimize (speed 3) (safety 0)))
  (let ((px (gs-proj-x gs))
        (py (gs-proj-y gs)))
    (cond ((and (= px #x4000) (zerop py)) x)
          ((and (zerop px) (= py #x4000)) y)
          (t (dot-f2dot14 x y px py)))))

(defun dual-project-point (gs x y)
  (declare (graphics-state gs) (s32 x y) (optimize (speed 3) (safety 0)))
  (let ((dx (gs-dual-x gs))
        (dy (gs-dual-y gs)))
    (cond ((and (= dx #x4000) (zerop dy)) x)
          ((and (zerop dx) (= dy #x4000)) y)
          (t (dot-f2dot14 x y dx dy)))))

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

(defun zone-dual-project-unscaled (gs zone1 pt1 zone2 pt2)
  "The unscaled (font unit) distance from PT2 of ZONE2 to PT1 of ZONE1 along
the dual projection vector."
  (declare (graphics-state gs) (tt-zone zone1 zone2) (fixnum pt1 pt2))
  (zone-check-point zone1 pt1)
  (zone-check-point zone2 pt2)
  (dual-project-point gs
                      (f26- (aref (zone-unscaled-x zone1) pt1) (aref (zone-unscaled-x zone2) pt2))
                      (f26- (aref (zone-unscaled-y zone1) pt1) (aref (zone-unscaled-y zone2) pt2))))

(defun move-zone-point (gs zone zone-id pt distance &key (touch t))
  "Move point PT in ZONE by DISTANCE along the Freedom Vector so its projection
changes by DISTANCE.  As FreeType's Direct_Move and skrifa's move_point, in
v40 backward compatibility mode x is never moved (except along a horizontal
projection vector) and y is not moved once IUP has run on both axes."
  (declare (graphics-state gs) (tt-zone zone) (type (integer 0 1) zone-id)
           (ignore zone-id) (fixnum pt) (s32 distance)
           (optimize (speed 3) (safety 0)))
  (zone-check-point zone pt)
  (let* ((fx (gs-free-x gs))
         (fy (gs-free-y gs))
         (px (gs-proj-x gs))
         (py (gs-proj-y gs))
         (raw (cond ((= fx #x4000) px)
                    ((= fy #x4000) py)
                    (t (dot-f2dot14 fx fy px py))))
         (axis-aligned (and (= raw #x4000) (or (= fx #x4000) (= fy #x4000))))
         ;; At small sizes fdotp can become too small and cause spikes.
         (fdotp (if (< (abs raw) #x400) #x4000 raw))
         (bc (gs-backward-compatibility gs))
         (post-iup (and bc (gs-did-iup-x gs) (gs-did-iup-y gs)))
         (horizontal (zerop py)))
    (declare (s32 raw fdotp))
    (flet ((delta (f)
             (declare (s32 f))
             (if axis-aligned distance (mul-div distance f fdotp))))
      (declare (inline delta))
      (when (not (zerop fx))
        (when (or (not bc) (and horizontal (not axis-aligned)))
          (setf (aref (zone-cur-x zone) pt)
                (f26+ (aref (zone-cur-x zone) pt) (delta fx))))
        (when touch
          (setf (aref (zone-flags zone) pt)
                (logior (aref (zone-flags zone) pt) +point-touched-x+))))
      (when (not (zerop fy))
        (when (or (not post-iup) (and horizontal (not axis-aligned)))
          (setf (aref (zone-cur-y zone) pt)
                (f26+ (aref (zone-cur-y zone) pt) (delta fy))))
        (when touch
          (setf (aref (zone-flags zone) pt)
                (logior (aref (zone-flags zone) pt) +point-touched-y+)))))))
