;;;; projections.lisp — TrueType hinting vector projections and point movement
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:runes/otf/tt-hint)

;;; Compiled for speed: the types are declared, and the checks left are the
;;; ones the font can trigger (stack, point and table indices, program bounds).
(declaim (optimize (speed 3) (safety 1) (debug 0)))

(declaim (ftype (function (graphics-state tt-zone fixnum) s32)
                zone-project-cur zone-dual-project-orig)
         (ftype (function (graphics-state tt-zone fixnum tt-zone fixnum &optional t) s32)
                zone-project-cur-diff zone-dual-project-orig-diff)
         (ftype (function (graphics-state tt-zone fixnum tt-zone fixnum) s32)
                zone-dual-project-unscaled)
         (ftype (function (graphics-state tt-zone (integer 0 1) fixnum s32
                           t)
                          t)
                %move-zone-point))

;;; ============================================================================
;;; Vector Projections & Point Movement (zone.rs / graphics.rs)
;;; ============================================================================

(declaim (inline project-point dual-project-point zone-check-point
                 zone-project-cur zone-dual-project-orig zone-project-cur-diff
                 zone-dual-project-orig-diff zone-dual-project-unscaled))

(defun project-point (gs x y)
  (declare (graphics-state gs) (s32 x y) (optimize (speed 3) (safety 0)))
  (case (gs-proj-axis gs)
    (0 x)
    (1 y)
    (t (dot-f2dot14 x y (gs-proj-x gs) (gs-proj-y gs)))))

(defun dual-project-point (gs x y)
  (declare (graphics-state gs) (s32 x y) (optimize (speed 3) (safety 0)))
  (case (gs-dual-axis gs)
    (0 x)
    (1 y)
    (t (dot-f2dot14 x y (gs-dual-x gs) (gs-dual-y gs)))))

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

(defun zone-project-cur-diff (gs zone1 pt1 zone2 pt2 &optional (check t))
  "The distance from PT2 of ZONE2 to PT1 of ZONE1, current positions, along
the projection vector: the difference projected, as FreeType and skrifa do,
which rounds once, not the difference of two projected positions.  With CHECK
false the caller has checked the point indices."
  (declare (graphics-state gs) (tt-zone zone1 zone2) (fixnum pt1 pt2))
  (when check
    (zone-check-point zone1 pt1)
    (zone-check-point zone2 pt2))
  (project-point gs
                 (f26- (aref (zone-cur-x zone1) pt1) (aref (zone-cur-x zone2) pt2))
                 (f26- (aref (zone-cur-y zone1) pt1) (aref (zone-cur-y zone2) pt2))))

(defun zone-dual-project-orig-diff (gs zone1 pt1 zone2 pt2 &optional (check t))
  "As ZONE-PROJECT-CUR-DIFF, for the original positions and the dual
projection vector."
  (declare (graphics-state gs) (tt-zone zone1 zone2) (fixnum pt1 pt2))
  (when check
    (zone-check-point zone1 pt1)
    (zone-check-point zone2 pt2))
  (dual-project-point gs
                      (f26- (aref (zone-orig-x zone1) pt1) (aref (zone-orig-x zone2) pt2))
                      (f26- (aref (zone-orig-y zone1) pt1) (aref (zone-orig-y zone2) pt2))))

(defun zone-dual-project-unscaled (gs zone1 pt1 zone2 pt2)
  "The unscaled (font unit) distance from PT2 of ZONE2 to PT1 of ZONE1 along
the dual projection vector."
  (declare (graphics-state gs) (tt-zone zone1 zone2) (fixnum pt1 pt2))
  (zone-check-point zone1 pt1)
  (zone-check-point zone2 pt2)
  (dual-project-point gs
                      (f26- (aref (zone-unscaled-x zone1) pt1) (aref (zone-unscaled-x zone2) pt2))
                      (f26- (aref (zone-unscaled-y zone1) pt1) (aref (zone-unscaled-y zone2) pt2))))

(defun %move-zone-point (gs zone zone-id pt distance touch)
  "As MOVE-ZONE-POINT, for a PT the caller has checked.  Move point PT in ZONE by DISTANCE along the Freedom Vector so its projection
changes by DISTANCE.  As FreeType's Direct_Move and skrifa's move_point, in
v40 backward compatibility mode x is never moved (except along a horizontal
projection vector) and y is not moved once IUP has run on both axes."
  (declare (graphics-state gs) (tt-zone zone) (type (integer 0 1) zone-id)
           (ignore zone-id) (fixnum pt) (s32 distance)
           (optimize (speed 3) (safety 0)))
  (case (gs-move-axis gs)
    ;; The common moves: along x or y with the projection on the same axis, so
    ;; the point moves by the distance.  In backward compatibility mode x is
    ;; never moved, and y not once IUP has run on both axes.
    (0 (unless (gs-backward-compatibility gs)
         (setf (aref (zone-cur-x zone) pt)
               (f26+ (aref (zone-cur-x zone) pt) distance)))
       (when touch
         (setf (aref (zone-flags zone) pt)
               (logior (aref (zone-flags zone) pt) +point-touched-x+))))
    (1 (unless (gs-post-iup gs)
         (setf (aref (zone-cur-y zone) pt)
               (f26+ (aref (zone-cur-y zone) pt) distance)))
       (when touch
         (setf (aref (zone-flags zone) pt)
               (logior (aref (zone-flags zone) pt) +point-touched-y+))))
    (t (move-zone-point-general gs zone pt distance touch))))

(declaim (inline move-zone-point))
(defun move-zone-point (gs zone zone-id pt distance touch)
  "Move point PT in ZONE by DISTANCE along the freedom vector so its projection
changes by DISTANCE (see %MOVE-ZONE-POINT), after checking PT."
  (declare (graphics-state gs) (tt-zone zone) (type (integer 0 1) zone-id)
           (fixnum pt) (s32 distance))
  (zone-check-point zone pt)
  (%move-zone-point gs zone zone-id pt distance touch))

(defun move-zone-point-general (gs zone pt distance touch)
  "MOVE-ZONE-POINT for any freedom and projection vectors."
  (declare (graphics-state gs) (tt-zone zone) (fixnum pt) (s32 distance)
           (optimize (speed 3) (safety 0)))
  (let* ((fx (gs-free-x gs))
         (fy (gs-free-y gs))
         (axis-aligned (gs-move-aligned gs))
         (fdotp (gs-fdotp gs))
         (bc (gs-backward-compatibility gs)))
    (declare (s32 fdotp))
    ;; What only some moves need is looked at only when they do.
    (flet ((delta (f)
             (declare (s32 f))
             (if axis-aligned distance (mul-div distance f fdotp)))
           (horizontal-p () (zerop (gs-proj-y gs)))
           (post-iup-p () (gs-post-iup gs)))
      (declare (inline delta horizontal-p post-iup-p))
      (when (not (zerop fx))
        (when (or (not bc) (and (not axis-aligned) (horizontal-p)))
          (setf (aref (zone-cur-x zone) pt)
                (f26+ (aref (zone-cur-x zone) pt) (delta fx))))
        (when touch
          (setf (aref (zone-flags zone) pt)
                (logior (aref (zone-flags zone) pt) +point-touched-x+))))
      (when (not (zerop fy))
        (when (or (not (post-iup-p)) (and (not axis-aligned) (horizontal-p)))
          (setf (aref (zone-cur-y zone) pt)
                (f26+ (aref (zone-cur-y zone) pt) (delta fy))))
        (when touch
          (setf (aref (zone-flags zone) pt)
                (logior (aref (zone-flags zone) pt) +point-touched-y+)))))))
