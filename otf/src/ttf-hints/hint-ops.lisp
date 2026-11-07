;;;; hint-ops.lisp — TrueType bytecode instruction handlers
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:runes/otf/tt-hint)

;;; Compiled for speed: the types are declared, and the checks left are the
;;; ones the font can trigger (stack, point and table indices, program bounds).
(declaim (optimize (speed 3) (safety 1) (debug 0)))

(declaim (inline read-cvt-val apply-min-distance decode-delta-arg)
         (ftype (function (tt-engine fixnum) s32) read-cvt-val)
         (ftype (function (graphics-state s32 s32) s32) apply-min-distance)
         (ftype (function (graphics-state fixnum s32 (integer 0 32)) (or null s32))
                decode-delta-arg)
         (ftype (function (tt-engine (signed-byte 32)) t) do-jump))

;;; ============================================================================
;;; 1. Push Instructions
;;; ============================================================================

;;; One bounds check for the whole run of operands, then raw reads with the
;;; program counter in a local, written back once.
(defmacro define-push (name width)
  (declare (type (integer 1 2) width))
  `(defun ,name (eng count)
     (declare (tt-engine eng) (type (integer 0 255) count) (optimize (speed 3) (safety 0)))
     (let* ((s (eng-stack eng))
            (top (vs-top s))
            (arr (vs-values s))
            (pc (eng-pc eng))
            (bytes (eng-bytes eng)))
       (declare (fixnum top pc))
       (when (> (+ top count) (length arr))
         (hint-fail 'tt-hint-stack-overflow "TrueType value stack overflow"))
       (when (> (+ pc (* ,width count)) (eng-end eng))
         (hint-fail 'tt-hint-invalid-opcode "Bytecode stream ended unexpectedly"))
       (dotimes (i count)
         (setf (aref arr (+ top i))
               ,(if (= width 1)
                    '(aref bytes (+ pc i))
                    '(let ((u (logior (ash (aref bytes (+ pc (* 2 i))) 8)
                                      (aref bytes (+ pc (* 2 i) 1)))))
                      (if (logbitp 15 u) (- u #x10000) u)))))
       (setf (eng-pc eng) (+ pc (* ,width count))
             (vs-top s) (+ top count)))))

(define-push push-bytes 1)
(define-push push-words 2)

;;; ============================================================================
;;; 2. Function & Control Flow Instructions
;;; ============================================================================

(defun op-fdef (eng)
  (declare (tt-engine eng))
  (let* ((key (vs-pop (eng-stack eng)))
         (fdefs (eng-fdefs eng))
         (def (or (when (< -1 key (length fdefs))
                    (let ((slot (svref fdefs key)))
                      (when (or (not (fdef-active-p slot)) (= (fdef-key slot) key))
                        slot)))
                  (find-if (lambda (d) (and (fdef-active-p d) (= (fdef-key d) key))) fdefs)
                  (find-if-not #'fdef-active-p fdefs))))
    (unless def
      (hint-fail 'tt-hint-limit-exceeded (format nil "Too many FDEF definitions (key ~D)" key)))
    (setf (fdef-active-p def) t
          (fdef-key def) key
          (fdef-program def) (eng-cur-program eng)
          (fdef-start def) (eng-pc eng))
    (skip-code-until eng nil t)
    (setf (fdef-end def) (eng-pc eng))))

(defun op-idef (eng)
  (declare (tt-engine eng))
  (let* ((opcode (ldb (byte 8 0) (vs-pop (eng-stack eng))))
         (idefs (eng-idefs eng))
         (def (or (find-if (lambda (d) (and (fdef-active-p d) (= (fdef-key d) opcode))) idefs)
                  (find-if-not #'fdef-active-p idefs))))
    (unless def
      (hint-fail 'tt-hint-limit-exceeded "Too many IDEF definitions"))
    (setf (fdef-active-p def) t
          (fdef-key def) opcode
          (fdef-program def) (eng-cur-program eng)
          (fdef-start def) (eng-pc eng))
    (skip-code-until eng nil t)
    (setf (fdef-end def) (eng-pc eng))))

(defun op-endf (eng)
  (declare (tt-engine eng))
  (when (<= (eng-call-top eng) 0)
    (hint-fail 'tt-hint-invalid-opcode "ENDF with empty call stack"))
  (let* ((idx (1- (eng-call-top eng)))
         (rec (svref (eng-call-stack eng) idx)))
    (if (> (call-loop-count rec) 1)
        (progn
          (decf (call-loop-count rec))
          (select-program eng (call-def-program rec) (call-def-start rec) (eng-end eng)))
        (progn
          (setf (eng-call-top eng) idx)
          (select-program eng (call-caller-program rec)
                          (call-return-pc rec)
                          (call-current-end rec))))))

(defun op-call (eng)
  (declare (tt-engine eng))
  (let* ((key (vs-pop (eng-stack eng)))
         (def (find-fdef eng key)))
    (unless def
      (hint-fail 'tt-hint-invalid-opcode (format nil "CALL to undefined FDEF ~D" key)))
    (invoke-function eng def 1)))

(defun op-loopcall (eng)
  (declare (tt-engine eng))
  (let* ((s (eng-stack eng))
         (key (vs-pop s))
         (count (vs-pop s))
         (def (find-fdef eng key)))
    (unless def
      (hint-fail 'tt-hint-invalid-opcode (format nil "LOOPCALL to undefined FDEF ~D" key)))
    (when (> count 0)
      (invoke-function eng def count))))

(defun do-jump (eng offset)
  (declare (tt-engine eng) (type (signed-byte 32) offset))
  (let ((target (+ (eng-pc eng) -1 offset)))
    ;; The glyph program is a slice of the font's bytes: a jump may not leave it.
    (when (or (< target (if (= (eng-cur-program eng) 2) (eng-glyph-base eng) 0))
              (> target (eng-end eng)))
      (hint-fail 'tt-hint-invalid-opcode "Jump target out of bounds"))
    (setf (eng-pc eng) target)))

;;; ============================================================================
;;; 3. Vector Instructions
;;; ============================================================================

(defun set-pv-to-axis (gs y-axis-p)
  (if y-axis-p
      (setf (gs-proj-x gs) 0      (gs-proj-y gs) #x4000
            (gs-dual-x gs) 0      (gs-dual-y gs) #x4000)
      (setf (gs-proj-x gs) #x4000 (gs-proj-y gs) 0
            (gs-dual-x gs) #x4000 (gs-dual-y gs) 0))
  (update-projection-state gs))

(defun set-fv-to-axis (gs y-axis-p)
  (if y-axis-p
      (setf (gs-free-x gs) 0      (gs-free-y gs) #x4000)
      (setf (gs-free-x gs) #x4000 (gs-free-y gs) 0))
  (update-projection-state gs))

(declaim (ftype (function (tt-zone fixnum tt-zone fixnum t t) (values s32 s32 &optional))
                line-vector-f2dot14))

(defun line-vector-f2dot14 (zone1 p1 zone2 p2 orig-p perp-p)
  (declare (tt-zone zone1 zone2) (fixnum p1 p2))
  (zone-check-point zone1 p1)
  (zone-check-point zone2 p2)
  (let* ((i1 (+ (zone-base zone1) p1))
         (i2 (+ (zone-base zone2) p2))
         (x1 (if orig-p (aref (zone-orig-x zone1) i1) (aref (zone-cur-x zone1) i1)))
         (y1 (if orig-p (aref (zone-orig-y zone1) i1) (aref (zone-cur-y zone1) i1)))
         (x2 (if orig-p (aref (zone-orig-x zone2) i2) (aref (zone-cur-x zone2) i2)))
         (y2 (if orig-p (aref (zone-orig-y zone2) i2) (aref (zone-cur-y zone2) i2)))
         (dx (f26- x1 x2))
         (dy (f26- y1 y2)))
    (declare (fixnum i1 i2))
    (if (and (zerop dx) (zerop dy))
        ;; Two points at one place: the vector is the x axis, even for the
        ;; perpendicular variants (confirmed by Greg Hitchcock, in FreeType).
        (values #x4000 0)
        (multiple-value-bind (ux uy) (normalize-f2dot14 dx dy)
          (declare (type s32 ux uy))
          (if perp-p
              (values (- uy) ux)
              (values ux uy))))))

(defun op-vtl (eng op)
  (declare (tt-engine eng) (type (unsigned-byte 8) op))
  (let* ((s (eng-stack eng))
         ;; As FreeType: the point on top of the stack is in ZP2, the one
         ;; below in ZP1, and the vector goes from the first to the second.
         (top (vs-pop s))
         (below (vs-pop s))
         (perp-p (logbitp 0 op))
         (gs (eng-gs eng))
         (z1 (eng-zp1 eng))
         (z2 (eng-zp2 eng)))
    (cond
      ((<= #x06 op #x07)
       (multiple-value-bind (ux uy) (line-vector-f2dot14 z1 below z2 top nil perp-p)
         (setf (gs-proj-x gs) ux (gs-proj-y gs) uy
               (gs-dual-x gs) ux (gs-dual-y gs) uy)))
      ((<= #x08 op #x09)
       (multiple-value-bind (ux uy) (line-vector-f2dot14 z1 below z2 top nil perp-p)
         (setf (gs-free-x gs) ux (gs-free-y gs) uy)))
      (t
       (multiple-value-bind (ox oy) (line-vector-f2dot14 z1 below z2 top t perp-p)
         (setf (gs-dual-x gs) ox (gs-dual-y gs) oy))
       (multiple-value-bind (cx cy) (line-vector-f2dot14 z1 below z2 top nil perp-p)
         (setf (gs-proj-x gs) cx (gs-proj-y gs) cy))))
    (update-projection-state gs)))

;;; ============================================================================
;;; 4. Point Movement Instructions
;;; ============================================================================

(defun read-cvt-val (eng idx)
  (declare (tt-engine eng) (fixnum idx))
  (let ((cvt (eng-cvt eng)))
    (if (< -1 idx (length cvt))
        (aref cvt idx)
        0)))

(defun op-mdap (eng round-p)
  "MDAP[0/1]: Move Direct Absolute Point."
  (declare (tt-engine eng))
  (let* ((pt (vs-pop (eng-stack eng)))
         (gs (eng-gs eng))
         (z0-id (gs-zp0 gs))
         (z0 (eng-zone eng z0-id))
         (cur-proj (zone-project-cur gs z0 pt))
         (dist (if round-p
                   (f26- (round-according-to-state (gs-round-state gs) cur-proj 0)
                         cur-proj)
                   0)))
    (%move-zone-point gs z0 z0-id pt dist t)
    (setf (gs-rp0 gs) pt
          (gs-rp1 gs) pt)))

(defun op-miap (eng round-p)
  "MIAP[0/1]: Move Indirect Absolute Point (matches FreeType Ins_MIAP / skrifa point.rs)."
  (declare (tt-engine eng))
  (let* ((s (eng-stack eng))
         (cvt-idx (vs-pop s))
         (pt (vs-pop s))
         (gs (eng-gs eng))
         (z0-id (gs-zp0 gs))
         (z0 (eng-zone eng z0-id))
         (distance (read-cvt-val eng cvt-idx)))
    (zone-check-point z0 pt)
    (when (zerop z0-id)
      (let ((x (mul-f2dot14 distance (gs-free-x gs)))
            (y (mul-f2dot14 distance (gs-free-y gs))))
        (setf (aref (zone-orig-x z0) pt) x
              (aref (zone-orig-y z0) pt) y
              (aref (zone-cur-x z0) pt)  x
              (aref (zone-cur-y z0) pt)  y)))
    (let ((org-proj (zone-project-cur gs z0 pt)))
      (when round-p
        ;; If |distance - org_proj| > control_value_cutin, fall back to org_proj;
        ;; then round once according to current round state.
        (when (> (abs (f26- distance org-proj)) (gs-control-value-cutin gs))
          (setf distance org-proj))
        (setf distance (round-according-to-state (gs-round-state gs) distance 0)))
      (%move-zone-point gs z0 z0-id pt (f26- distance org-proj) t))
    (setf (gs-rp0 gs) pt
          (gs-rp1 gs) pt)))

(defun apply-min-distance (gs org-dist cur-dist)
  (declare (graphics-state gs) (s32 org-dist cur-dist))
  (let ((min-d (gs-min-distance gs)))
    (if (>= org-dist 0)
        (max cur-dist min-d)
        (min cur-dist (f26-neg min-d)))))

(defun op-mdrp (eng flags)
  "MDRP[abcde]: Move Direct Relative Point (0xC0..0xDF)."
  (declare (tt-engine eng) (fixnum flags))
  (let* ((pt (vs-pop (eng-stack eng)))
         (gs (eng-gs eng))
         (set-rp0-p (logbitp 4 flags))
         (keep-min-p (logbitp 3 flags))
         (round-p (logbitp 2 flags))
         (z0 (eng-zp0 eng))
         (z1-id (gs-zp1 gs))
         (z1 (eng-zone eng z1-id))
         (rp0 (gs-rp0 gs))
         ;; Out of the twilight zone the original distance is measured on the
         ;; unscaled outline and scaled afterwards, as FreeType does.
         (org-dist (if (or (zerop z1-id) (zerop (gs-zp0 gs)))
                       (zone-dual-project-orig-diff gs z1 pt z0 rp0)
                       (mul-fix (zone-dual-project-unscaled gs z1 pt z0 rp0)
                                (eng-unscaled-scale eng)))))
    (when (and (plusp (gs-single-width-cutin gs))
               (< (abs (f26- (abs org-dist) (gs-single-width-value gs)))
                  (gs-single-width-cutin gs)))
      (setf org-dist (if (>= org-dist 0)
                         (gs-single-width-value gs)
                         (f26-neg (gs-single-width-value gs)))))
    (let ((dist (if round-p
                    (round-according-to-state (gs-round-state gs) org-dist 0)
                    org-dist)))
      (when keep-min-p
        (setf dist (apply-min-distance gs org-dist dist)))
      ;; The first measure above checked the points (or the unscaled one did).
      (let ((cur-dist (zone-project-cur-diff gs z1 pt z0 rp0 nil)))
        (%move-zone-point gs z1 z1-id pt (f26- dist cur-dist) t)))
    (setf (gs-rp1 gs) (gs-rp0 gs)
          (gs-rp2 gs) pt)
    (when set-rp0-p
      (setf (gs-rp0 gs) pt))))

(defun op-mirp (eng flags)
  "MIRP[abcde]: Move Indirect Relative Point (0xE0..0xFF).
Matches FreeType Ins_MIRP / Skrifa point.rs."
  (declare (tt-engine eng) (fixnum flags))
  (let* ((s (eng-stack eng))
         (cvt-idx (vs-pop s))
         (pt (vs-pop s))
         (gs (eng-gs eng))
         (set-rp0-p (logbitp 4 flags))
         (keep-min-p (logbitp 3 flags))
         (round-p (logbitp 2 flags))
         (cvt-dist (read-cvt-val eng cvt-idx))
         (z0 (eng-zp0 eng))
         (z1-id (gs-zp1 gs))
         (z1 (eng-zone eng z1-id))
         (rp0 (gs-rp0 gs)))
    (when (and (plusp (gs-single-width-cutin gs))
               (< (abs (f26- (abs cvt-dist) (gs-single-width-value gs)))
                  (gs-single-width-cutin gs)))
      (setf cvt-dist (if (>= cvt-dist 0)
                         (gs-single-width-value gs)
                         (f26-neg (gs-single-width-value gs)))))
    (when (zerop z1-id)
      (zone-check-point z0 rp0)
      (zone-check-point z1 pt)
      (let ((i0 (+ (zone-base z0) rp0))
            (dx (mul-f2dot14 cvt-dist (gs-free-x gs)))
            (dy (mul-f2dot14 cvt-dist (gs-free-y gs))))
        (declare (fixnum i0))
        (setf (aref (zone-orig-x z1) pt) (f26+ (aref (zone-orig-x z0) i0) dx)
              (aref (zone-orig-y z1) pt) (f26+ (aref (zone-orig-y z0) i0) dy)
              (aref (zone-cur-x z1) pt)  (aref (zone-orig-x z1) pt)
              (aref (zone-cur-y z1) pt)  (aref (zone-orig-y z1) pt))))
    ;; The points are checked once, here, for both measures.
    (zone-check-point z0 rp0)
    (zone-check-point z1 pt)
    (let ((org-dist (zone-dual-project-orig-diff gs z1 pt z0 rp0 nil))
          (cur-dist (zone-project-cur-diff gs z1 pt z0 rp0 nil)))
      (when (and (gs-auto-flip gs) (not (eq (minusp org-dist) (minusp cvt-dist))))
        (setf cvt-dist (f26-neg cvt-dist)))
      ;; Bit 2 (round-p) controls CVT cut-in and rounding; when round-p is NIL,
      ;; MIRP uses cvt-dist unrounded (NOT org-dist!).
      (let ((dist (if round-p
                      (let ((d (if (and (= (gs-zp0 gs) (gs-zp1 gs))
                                        (> (abs (f26- cvt-dist org-dist))
                                           (gs-control-value-cutin gs)))
                                   org-dist
                                   cvt-dist)))
                        (round-according-to-state (gs-round-state gs) d 0))
                      cvt-dist)))
        (when keep-min-p
          (setf dist (apply-min-distance gs org-dist dist)))
        (%move-zone-point gs z1 z1-id pt (f26- dist cur-dist) t)))
    (setf (gs-rp1 gs) (gs-rp0 gs)
          (gs-rp2 gs) pt)
    (when set-rp0-p
      (setf (gs-rp0 gs) pt))))

(declaim (ftype (function (tt-engine t) (values s32 tt-zone fixnum &optional))
                compute-ref-displacement))

(defun compute-ref-displacement (eng use-rp1-p)
  (declare (tt-engine eng))
  (let* ((gs (eng-gs eng))
         (z-id (if use-rp1-p (gs-zp0 gs) (gs-zp1 gs)))
         (zone (eng-zone eng z-id))
         (pt (if use-rp1-p (gs-rp1 gs) (gs-rp2 gs))))
    (zone-check-point zone pt)
    (let* ((idx (+ (zone-base zone) pt))
           (proj (project-point gs
                                (f26- (aref (zone-cur-x zone) idx) (aref (zone-orig-x zone) idx))
                                (f26- (aref (zone-cur-y zone) idx) (aref (zone-orig-y zone) idx)))))
      (declare (fixnum idx))
      (values proj zone pt))))

(defun op-shp (eng use-rp1-p)
  (declare (tt-engine eng))
  (let* ((gs (eng-gs eng))
         (s (eng-stack eng))
         (count (gs-loop gs))
         (z2-id (gs-zp2 gs))
         (z2 (eng-zone eng z2-id)))
    (setf (gs-loop gs) 1)
    ;; Like FreeType, do nothing when the loop count exceeds the stack.
    (when (< (vs-len s) count) (setf count 0))
    (let ((dist (compute-ref-displacement eng use-rp1-p)))
      (dotimes (i count)
        (let ((pt (vs-pop s)))
          (move-zone-point gs z2 z2-id pt dist t))))))

(defun op-shc (eng use-rp1-p)
  (declare (tt-engine eng))
  (let* ((gs (eng-gs eng))
         (contour-idx (vs-pop (eng-stack eng)))
         (z2-id (gs-zp2 gs))
         (z2 (eng-zone eng z2-id)))
    (unless (< -1 contour-idx (zone-n-contours z2))
      (hint-fail 'tt-hint-invalid-zone "SHC contour index out of range"))
    (multiple-value-bind (dist ref-zone ref-pt) (compute-ref-displacement eng use-rp1-p)
      (let* ((cb (+ (zone-contour-base z2) contour-idx))
             (start (if (zerop contour-idx) 0 (1+ (aref (zone-contours z2) (1- cb)))))
             (end (aref (zone-contours z2) cb)))
        (declare (fixnum cb))
        (loop for pt from start to end
              unless (and (eq z2 ref-zone) (= pt ref-pt))
                do (move-zone-point gs z2 z2-id pt dist t))))))

(defun op-shz (eng use-rp1-p)
  (declare (tt-engine eng))
  (let* ((gs (eng-gs eng))
         (zone-idx (vs-pop (eng-stack eng)))
         (z-id (if (zerop zone-idx) 0 1))
         (z (eng-zone eng z-id)))
    (multiple-value-bind (dist ref-zone ref-pt) (compute-ref-displacement eng use-rp1-p)
      (dotimes (pt (zone-n-glyph-points z))
        (unless (and (eq z ref-zone) (= pt ref-pt))
          (%move-zone-point gs z z-id pt dist nil))))))

(defun op-shpix (eng)
  (declare (tt-engine eng))
  (when (< (vs-len (eng-stack eng)) (1+ (gs-loop (eng-gs eng))))
    (setf (gs-loop (eng-gs eng)) 1)
    (return-from op-shpix))
  (let* ((gs (eng-gs eng))
         (s (eng-stack eng))
         (dist (vs-pop s))
         (count (gs-loop gs))
         (z2-id (gs-zp2 gs))
         (z2 (eng-zone eng z2-id))
         (fx (gs-free-x gs))
         (fy (gs-free-y gs)))
    (setf (gs-loop gs) 1)
    ;; As FreeType and skrifa: in backward compatibility mode only points
    ;; already touched in y (or any point, in a composite glyph, when the
    ;; freedom vector has a y component) are shifted, and not after IUP.
    (let* ((dx (mul-f2dot14 dist fx))
           (dy (mul-f2dot14 dist fy))
           (bc (gs-backward-compatibility gs))
           (did-iup (gs-post-iup gs))
           (twilight (or (zerop (gs-zp0 gs)) (zerop (gs-zp1 gs)) (zerop z2-id))))
      (dotimes (i count)
        (let ((pt (vs-pop s)))
          (zone-check-point z2 pt)
          (let ((idx (+ (zone-base z2) pt)))
            (declare (fixnum idx))
            (when (or (not bc)
                      twilight
                      (and (not did-iup)
                           (or (and (eng-composite-p eng) (not (zerop fy)))
                               (logtest (aref (zone-flags z2) idx) +point-touched-y+))))
              (when (not (zerop fx))
                (unless bc
                  (setf (aref (zone-cur-x z2) idx) (f26+ (aref (zone-cur-x z2) idx) dx)))
                (setf (aref (zone-flags z2) idx) (logior (aref (zone-flags z2) idx) +point-touched-x+)))
              (when (not (zerop fy))
                (unless (and bc did-iup)
                  (setf (aref (zone-cur-y z2) idx) (f26+ (aref (zone-cur-y z2) idx) dy)))
                (setf (aref (zone-flags z2) idx) (logior (aref (zone-flags z2) idx) +point-touched-y+))))))))))

(defun op-ip (eng)
  "IP: Interpolate Point by the last relative stretch.  Out of the twilight
zone the original distances are measured on the unscaled outline, as FreeType
does; they only matter as a ratio."
  (declare (tt-engine eng))
  (let* ((gs (eng-gs eng))
         (s (eng-stack eng))
         (count (gs-loop gs))
         (z0 (eng-zp0 eng))
         (z1 (eng-zp1 eng))
         (z2-id (gs-zp2 gs))
         (z2 (eng-zone eng z2-id))
         (rp1 (gs-rp1 gs))
         (rp2 (gs-rp2 gs))
         (twilight (or (zerop (gs-zp0 gs)) (zerop (gs-zp1 gs)) (zerop z2-id)))
         (old-range (if twilight
                        (zone-dual-project-orig-diff gs z1 rp2 z0 rp1)
                        (zone-dual-project-unscaled gs z1 rp2 z0 rp1)))
         (cur-range (zone-project-cur-diff gs z1 rp2 z0 rp1)))
    (setf (gs-loop gs) 1)
    ;; Like FreeType, do nothing when the loop count exceeds the stack.
    (when (< (vs-len s) count) (setf count 0))
    (dotimes (i count)
      (let* ((pt (vs-pop s))
             ;; RP1 was checked with the ranges above.
             (cur-dist (progn (zone-check-point z2 pt)
                              (zone-project-cur-diff gs z2 pt z0 rp1 nil)))
             (org-dist (if twilight
                           (zone-dual-project-orig-diff gs z2 pt z0 rp1 nil)
                           (zone-dual-project-unscaled gs z2 pt z0 rp1)))
             (new-dist (cond ((zerop org-dist) 0)
                             ((zerop old-range) org-dist)
                             (t (mul-div org-dist cur-range old-range)))))
        (%move-zone-point gs z2 z2-id pt (f26- new-dist cur-dist) t)))))

(defun iup-interpolate-contour (orig unscaled cur flags touch-mask start end)
  "IUP over the contour START..END of one axis: move the untouched points
between touched ones.  As FreeType's _iup_worker_interpolate, the position
between two touched points comes from the unscaled coordinates."
  (declare (type (simple-array s32 (*)) orig unscaled cur)
           (type (simple-array (unsigned-byte 8) (*)) flags)
           (type (unsigned-byte 8) touch-mask)
           (fixnum start end)
           (optimize (speed 3) (safety 0)))
  (labels ((interp-range (from to ref1 ref2)
             ;; Points FROM below TO, between the touched REF1 and REF2.
             (declare (fixnum from to ref1 ref2))
             (when (< from to)
               (when (> (aref unscaled ref1) (aref unscaled ref2))
                 (rotatef ref1 ref2))
               (let* ((orus1 (aref unscaled ref1))
                      (orus2 (aref unscaled ref2))
                      (org1 (aref orig ref1))
                      (org2 (aref orig ref2))
                      (cur1 (aref cur ref1))
                      (cur2 (aref cur ref2))
                      (delta1 (f26- cur1 org1))
                      (delta2 (f26- cur2 org2))
                      (scale (if (or (= cur1 cur2) (= orus1 orus2))
                                 0
                                 (div-fix (f26- cur2 cur1) (f26- orus2 orus1)))))
                 (declare (s32 orus1 orus2 org1 org2 cur1 cur2 delta1 delta2 scale))
                 ;; Whether the span is flat does not depend on the point:
                 ;; the test is out of the loops.
                 (if (or (= cur1 cur2) (= orus1 orus2))
                     (loop for i of-type fixnum from from below to
                           for a of-type s32 = (aref orig i)
                           do (setf (aref cur i)
                                    (cond ((<= a org1) (f26+ a delta1))
                                          ((>= a org2) (f26+ a delta2))
                                          (t cur1))))
                     (loop for i of-type fixnum from from below to
                           for a of-type s32 = (aref orig i)
                           do (setf (aref cur i)
                                    (cond ((<= a org1) (f26+ a delta1))
                                          ((>= a org2) (f26+ a delta2))
                                          (t (f26+ cur1 (mul-fix (f26- (aref unscaled i) orus1)
                                                                 scale)))))))))))
    (let ((first -1) (prev -1))
      (declare (fixnum first prev))
      (loop for i of-type fixnum from start to end
            when (logtest (aref flags i) touch-mask)
              do (cond ((< prev 0) (setf first i))
                       (t (interp-range (1+ prev) i prev i)))
                 (setf prev i))
      (cond
        ((< prev 0))
        ((= prev first)
         ;; One touched point: the whole contour shifts with it.
         (let ((delta (f26- (aref cur first) (aref orig first))))
           (unless (zerop delta)
             (loop for i of-type fixnum from start to end
                   unless (= i first)
                     do (setf (aref cur i) (f26+ (aref cur i) delta))))))
        (t
         (interp-range (1+ prev) (1+ end) prev first)
         (interp-range start first prev first))))))

(defun op-iup (eng x-axis-p)
  (declare (tt-engine eng))
  (let* ((gs (eng-gs eng))
         (z (eng-glyph eng))
         (n-contours (zone-n-contours z)))
    ;; In backward compatibility mode IUP runs until it has been done on
    ;; both axes (FreeType, skrifa).
    (when (gs-backward-compatibility gs)
      (let ((done (gs-post-iup gs)))
        (if x-axis-p
            (setf (gs-did-iup-x gs) t)
            (setf (gs-did-iup-y gs) t))
        (when (and (gs-did-iup-x gs) (gs-did-iup-y gs))
          (setf (gs-post-iup gs) t))
        (when done
          (return-from op-iup))))
    (let ((orig (if x-axis-p (zone-orig-x z) (zone-orig-y z)))
          (unscaled (if x-axis-p (zone-unscaled-x z) (zone-unscaled-y z)))
          (cur  (if x-axis-p (zone-cur-x z)  (zone-cur-y z)))
          (flags (zone-flags z))
          (mask (if x-axis-p +point-touched-x+ +point-touched-y+))
          (contours (zone-contours z))
          (base (zone-base z))
          (cbase (zone-contour-base z))
          (start 0))
      (declare (fixnum base cbase start))
      (dotimes (c n-contours)
        (let ((end (aref contours (+ cbase c))))
          (when (and (>= end start) (< end (zone-n-points z)))
            (iup-interpolate-contour orig unscaled cur flags mask (+ base start) (+ base end)))
          (setf start (1+ end)))))))

(defun op-isect (eng)
  (declare (tt-engine eng))
  (let* ((s (eng-stack eng))
         (b1 (vs-pop s)) (b0 (vs-pop s))
         (a1 (vs-pop s)) (a0 (vs-pop s))
         (pt (vs-pop s))
         (z0 (eng-zp0 eng)) (z1 (eng-zp1 eng)) (z2 (eng-zp2 eng)))
    (zone-check-point z0 b0) (zone-check-point z0 b1)
    (zone-check-point z1 a0) (zone-check-point z1 a1)
    (zone-check-point z2 pt)
    (let* ((ib0 (+ (zone-base z0) b0)) (ib1 (+ (zone-base z0) b1))
           (ia0 (+ (zone-base z1) a0)) (ia1 (+ (zone-base z1) a1))
           (ipt (+ (zone-base z2) pt))
           (ax0 (aref (zone-cur-x z1) ia0)) (ay0 (aref (zone-cur-y z1) ia0))
           (ax1 (aref (zone-cur-x z1) ia1)) (ay1 (aref (zone-cur-y z1) ia1))
           (bx0 (aref (zone-cur-x z0) ib0)) (by0 (aref (zone-cur-y z0) ib0))
           (bx1 (aref (zone-cur-x z0) ib1)) (by1 (aref (zone-cur-y z0) ib1))
           (dax (- ax1 ax0)) (day (- ay1 ay0))
           (dbx (- bx1 bx0)) (dby (- by1 by0))
           (dx (- bx0 ax0)) (dy (- by0 ay0))
           ;; As FreeType and skrifa: the cross and dot products of the two
           ;; directions stand in for sin and cos of the angle between them,
           ;; and a tan below 1/19 (3 degrees) is a grazing intersection.
           (disc (+ (mul-div dax (- dby) 64) (mul-div day dbx 64)))
           (dot (+ (mul-div dax dbx 64) (mul-div day dby 64))))
      (declare (fixnum ib0 ib1 ia0 ia1 ipt))
      (if (> (* 19 (abs disc)) (abs dot))
          (let ((v (+ (mul-div dx (- dby) 64) (mul-div dy dbx 64))))
            (setf (aref (zone-cur-x z2) ipt) (f26+ ax0 (mul-div v dax disc))
                  (aref (zone-cur-y z2) ipt) (f26+ ay0 (mul-div v day disc))))
          (setf (aref (zone-cur-x z2) ipt) (truncate (+ ax0 ax1 bx0 bx1) 4)
                (aref (zone-cur-y z2) ipt) (truncate (+ ay0 ay1 by0 by1) 4)))
      (setf (aref (zone-flags z2) ipt)
            (logior (aref (zone-flags z2) ipt) +point-touched+)))))

;;; ============================================================================
;;; 5. Delta Instructions
;;; ============================================================================

(defun decode-delta-arg (gs ppem arg band-offset)
  (declare (graphics-state gs) (fixnum ppem) (s32 arg) (type (integer 0 32) band-offset))
  (let ((target-ppem (+ (gs-delta-base gs) band-offset (ldb (byte 4 4) arg))))
    (when (= target-ppem ppem)
      (let* ((mag-code (ldb (byte 4 0) arg))
             (steps (if (>= mag-code 8) (- mag-code 7) (- mag-code 8)))
             (shift (gs-delta-shift gs)))
        (if (<= shift 6)
            (s32 (ash steps (- 6 shift)))
            (s32 (ash steps (- (- shift 6)))))))))

(defun op-deltap (eng band-offset)
  (declare (tt-engine eng) (fixnum band-offset))
  (let* ((s (eng-stack eng))
         (n (vs-pop s))
         (gs (eng-gs eng))
         (ppem (eng-ppem eng))
         (z0-id (gs-zp0 gs))
         (z0 (eng-zone eng z0-id)))
    (dotimes (i n)
      (let* ((pt (vs-pop s))
             (arg (vs-pop s))
             (delta (decode-delta-arg gs ppem arg band-offset)))
        (when delta
          (zone-check-point z0 pt)
          ;; As FreeType and skrifa: in backward compatibility mode only before
          ;; both IUPs, and only points touched in y, or any point of a
          ;; composite glyph when the freedom vector has a y component.
          (when (or (not (gs-backward-compatibility gs))
                    (and (not (gs-post-iup gs))
                         (or (and (eng-composite-p eng) (not (zerop (gs-free-y gs))))
                             (logtest (aref (zone-flags z0) (+ (zone-base z0) pt)) +point-touched-y+))))
            (%move-zone-point gs z0 z0-id pt delta t)))))))

(defun op-deltac (eng band-offset)
  (declare (tt-engine eng) (fixnum band-offset))
  (let* ((s (eng-stack eng))
         (n (vs-pop s))
         (gs (eng-gs eng))
         (ppem (eng-ppem eng))
         (cvt (eng-cvt eng)))
    (dotimes (i n)
      (let* ((cvt-idx (vs-pop s))
             (arg (vs-pop s))
             (delta (decode-delta-arg gs ppem arg band-offset)))
        (when (and delta (< -1 cvt-idx (length cvt)))
          (setf (aref cvt cvt-idx) (f26+ (aref cvt cvt-idx) delta)))))))

;;; ============================================================================
;;; 6. Master Instruction Dispatcher
;;; ============================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun opcode-range (low high)
    "The opcodes LOW to HIGH, for the keys of a CASE clause."
    (declare (type (integer 0 255) low high))
    (loop for op from low to high collect op)))

(defun dispatch-instruction (eng op)
  (declare (tt-engine eng) (type (unsigned-byte 8) op) (optimize (speed 3) (safety 0)))
  ;; The stack and the state are read where an arm uses them: many arms use
  ;; neither, or only one.
  (symbol-macrolet ((s (eng-stack eng))
                    (gs (eng-gs eng)))
    (case op
      (#.(opcode-range #xB0 #xB7) (push-bytes eng (1+ (- op #xB0))))
      (#.(opcode-range #xB8 #xBF) (push-words eng (1+ (- op #xB8))))
      (#.(opcode-range #xC0 #xDF) (op-mdrp eng (logand op #x1F)))
      (#.(opcode-range #xE0 #xFF) (op-mirp eng (logand op #x1F)))
      (#.(opcode-range #x68 #x69)
       (let ((val (vs-pop s)))
         (vs-push s (round-according-to-state (gs-round-state gs) val 0))))
      ;; NROUND[ab] leaves the value as it is; 0x6C-0x6F are not defined.
      (#.(opcode-range #x6A #x6F) nil)

      ((#x00 #x01)
       (let ((y-p (zerop op)))
         (set-pv-to-axis gs y-p)
         (set-fv-to-axis gs y-p)))
      ((#x02 #x03) (set-pv-to-axis gs (= op #x02)))
      ((#x04 #x05) (set-fv-to-axis gs (= op #x04)))
      ((#x06 #x07 #x08 #x09 #x86 #x87) (op-vtl eng op))
      (#x0A
       (let* ((y (vs-pop s)) (x (vs-pop s)))
         (multiple-value-bind (ux uy) (normalize-f2dot14 x y)
           (setf (gs-proj-x gs) ux (gs-proj-y gs) uy
                 (gs-dual-x gs) ux (gs-dual-y gs) uy)
           (update-projection-state gs))))
      (#x0B
       (let* ((y (vs-pop s)) (x (vs-pop s)))
         (multiple-value-bind (ux uy) (normalize-f2dot14 x y)
           (setf (gs-free-x gs) ux (gs-free-y gs) uy)
           (update-projection-state gs))))
      (#x0C (vs-push s (gs-proj-x gs)) (vs-push s (gs-proj-y gs)))
      (#x0D (vs-push s (gs-free-x gs)) (vs-push s (gs-free-y gs)))
      (#x0E (setf (gs-free-x gs) (gs-proj-x gs) (gs-free-y gs) (gs-proj-y gs))
       (update-projection-state gs))
      (#x0F (op-isect eng))
      (#x10 (setf (gs-rp0 gs) (vs-pop s)))
      (#x11 (setf (gs-rp1 gs) (vs-pop s)))
      (#x12 (setf (gs-rp2 gs) (vs-pop s)))
      (#x13 (let ((z (vs-pop s))) (unless (<= 0 z 1) (hint-fail 'tt-hint-invalid-zone "SZP0")) (setf (gs-zp0 gs) z)))
      (#x14 (let ((z (vs-pop s))) (unless (<= 0 z 1) (hint-fail 'tt-hint-invalid-zone "SZP1")) (setf (gs-zp1 gs) z)))
      (#x15 (let ((z (vs-pop s))) (unless (<= 0 z 1) (hint-fail 'tt-hint-invalid-zone "SZP2")) (setf (gs-zp2 gs) z)))
      (#x16 (let ((z (vs-pop s))) (unless (<= 0 z 1) (hint-fail 'tt-hint-invalid-zone "SZPS"))
              (setf (gs-zp0 gs) z (gs-zp1 gs) z (gs-zp2 gs) z)))
      (#x17 (setf (gs-loop gs) (max 0 (vs-pop s))))
      (#x18 (setf (gs-slots-dirty gs) t) (setf (rs-mode (gs-round-state gs)) :to-grid))
      (#x19 (setf (gs-slots-dirty gs) t) (setf (rs-mode (gs-round-state gs)) :to-half-grid))
      (#x1A (setf (gs-slots-dirty gs) t) (setf (gs-min-distance gs) (vs-pop s)))
      (#x1B (skip-code-until eng nil nil))
      (#x1C (do-jump eng (vs-pop s)))
      (#x1D (setf (gs-slots-dirty gs) t) (setf (gs-control-value-cutin gs) (vs-pop s)))
      (#x1E (setf (gs-slots-dirty gs) t) (setf (gs-single-width-cutin gs) (vs-pop s)))
      (#x1F (setf (gs-slots-dirty gs) t) (setf (gs-single-width-value gs) (mul-fix (vs-pop s) (eng-scale eng))))
      (#x20 (vs-dup s))
      (#x21 (vs-pop s))
      (#x22 (vs-clear s))
      (#x23 (vs-swap s))
      (#x24 (vs-push s (vs-len s)))
      (#x25 (vs-copy-index s (vs-pop s)))
      (#x26 (vs-move-index s (vs-pop s)))
      (#x27
       ;; P2, on top of the stack, is in ZP0; P1 below it is in ZP1.
       (let* ((p2 (vs-pop s)) (p1 (vs-pop s))
              (z1-id (gs-zp1 gs)) (z0-id (gs-zp0 gs))
              (z1 (eng-zone eng z1-id)) (z0 (eng-zone eng z0-id))
              ;; Half of it, truncated towards zero as in C.
              (dist (truncate (zone-project-cur-diff gs z0 p2 z1 p1) 2)))
         (%move-zone-point gs z1 z1-id p1 dist t)
         (%move-zone-point gs z0 z0-id p2 (f26-neg dist) t)))
      (#x28 nil)
      (#x29
       (let* ((pt (vs-pop s)) (z0 (eng-zp0 eng)))
         (zone-check-point z0 pt)
         (let ((idx (+ (zone-base z0) pt)))
           (declare (fixnum idx))
           (when (not (zerop (gs-free-x gs)))
             (setf (aref (zone-flags z0) idx) (logandc2 (aref (zone-flags z0) idx) +point-touched-x+)))
           (when (not (zerop (gs-free-y gs)))
             (setf (aref (zone-flags z0) idx) (logandc2 (aref (zone-flags z0) idx) +point-touched-y+))))))
      (#x2A (op-loopcall eng))
      (#x2B (op-call eng))
      (#x2C (op-fdef eng))
      (#x2D (op-endf eng))
      ((#x2E #x2F) (op-mdap eng (= op #x2F)))
      ;; IUP[a]: a = 0 (0x30) interpolates in y, a = 1 (0x31) in x, as in
      ;; the spec, FreeType and skrifa.
      ((#x30 #x31) (op-iup eng (= op #x31)))
      ((#x32 #x33) (op-shp eng (= op #x33)))
      ((#x34 #x35) (op-shc eng (= op #x35)))
      ((#x36 #x37) (op-shz eng (= op #x37)))
      (#x38 (op-shpix eng))
      (#x39 (op-ip eng))
      ((#x3A #x3B)
       (let* ((dist (vs-pop s)) (pt (vs-pop s))
              (z0 (eng-zp0 eng)) (z1-id (gs-zp1 gs)) (z1 (eng-zone eng z1-id))
              (rp0 (gs-rp0 gs)))
         (when (zerop z1-id)
           (zone-check-point z0 rp0) (zone-check-point z1 pt)
           (let ((i0 (+ (zone-base z0) rp0))
                 (dx (mul-f2dot14 dist (gs-free-x gs)))
                 (dy (mul-f2dot14 dist (gs-free-y gs))))
             (declare (fixnum i0))
             (setf (aref (zone-orig-x z1) pt) (f26+ (aref (zone-orig-x z0) i0) dx)
                   (aref (zone-orig-y z1) pt) (f26+ (aref (zone-orig-y z0) i0) dy)
                   (aref (zone-cur-x z1) pt)  (aref (zone-orig-x z1) pt)
                   (aref (zone-cur-y z1) pt)  (aref (zone-orig-y z1) pt))))
         (let ((cur-dist (zone-project-cur-diff gs z1 pt z0 rp0)))
           (%move-zone-point gs z1 z1-id pt (f26- dist cur-dist) t))
         (setf (gs-rp1 gs) (gs-rp0 gs) (gs-rp2 gs) pt)
         (when (= op #x3B) (setf (gs-rp0 gs) pt))))
      (#x3C
       (let* ((count (gs-loop gs)) (z0 (eng-zp0 eng)) (z1-id (gs-zp1 gs)) (z1 (eng-zone eng z1-id))
             (rp0 (gs-rp0 gs)))
         (setf (gs-loop gs) 1)
         (when (< (vs-len s) count) (setf count 0))
         (dotimes (i count)
           (let ((pt (vs-pop s)))
             (%move-zone-point gs z1 z1-id pt (zone-project-cur-diff gs z0 rp0 z1 pt)
                              t)))))
      (#x3D (setf (gs-slots-dirty gs) t) (setf (rs-mode (gs-round-state gs)) :to-double-grid))
      ((#x3E #x3F) (op-miap eng (= op #x3F)))
      (#x40 (push-bytes eng (fetch-u8 eng)))
      (#x41 (push-words eng (fetch-u8 eng)))
      (#x42
       (let* ((val (vs-pop s)) (idx (vs-pop s)) (store (eng-storage eng)))
         (setf (eng-tables-dirty eng) t)
         (when (< -1 idx (length store)) (setf (aref store idx) val))))
      (#x43
       (let ((idx (vs-pop s)) (store (eng-storage eng)))
         (vs-push s (if (< -1 idx (length store)) (aref store idx) 0))))
      (#x44
       (let* ((val (vs-pop s)) (idx (vs-pop s)) (cvt (eng-cvt eng)))
         (setf (eng-tables-dirty eng) t)
         (when (< -1 idx (length cvt)) (setf (aref cvt idx) val))))
      (#x45 (vs-push s (read-cvt-val eng (vs-pop s))))
      ((#x46 #x47)
       (let* ((pt (vs-pop s)) (z2 (eng-zp2 eng)))
         (zone-check-point z2 pt)
         (vs-push s (if (= op #x46)
                        (zone-project-cur gs z2 pt)
                        (zone-dual-project-orig gs z2 pt)))))
      (#x48
       (let* ((val (vs-pop s)) (pt (vs-pop s))
              (z2-id (gs-zp2 gs)) (z2 (eng-zone eng z2-id))
              (cur (zone-project-cur gs z2 pt)))
         (%move-zone-point gs z2 z2-id pt (f26- val cur) t)
         (when (zerop z2-id)
           (setf (aref (zone-orig-x z2) pt) (aref (zone-cur-x z2) pt)
                 (aref (zone-orig-y z2) pt) (aref (zone-cur-y z2) pt)))))
      ((#x49 #x4A)
       (let* ((p1 (vs-pop s)) (p2 (vs-pop s))
              (z0 (eng-zp0 eng)) (z1 (eng-zp1 eng)))
         (vs-push s (cond
                      ((= op #x49) (zone-project-cur-diff gs z0 p2 z1 p1))
                      ;; MD[0]: out of the twilight zone the original
                      ;; distance is that of the unscaled outline, scaled.
                      ((or (zerop (gs-zp0 gs)) (zerop (gs-zp1 gs)))
                       (zone-dual-project-orig-diff gs z0 p2 z1 p1))
                      (t (mul-fix (zone-dual-project-unscaled gs z0 p2 z1 p1)
                                  (eng-unscaled-scale eng)))))))
      (#x4B (vs-push s (eng-ppem eng)))
      (#x4C (vs-push s (eng-point-size eng)))
      (#x4D (setf (gs-auto-flip gs) t))
      (#x4E (setf (gs-auto-flip gs) nil))
      (#x4F nil)
      (#x50 (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (if (< a b) 1 0))))
      (#x51 (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (if (<= a b) 1 0))))
      (#x52 (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (if (> a b) 1 0))))
      (#x53 (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (if (>= a b) 1 0))))
      (#x54 (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (if (= a b) 1 0))))
      (#x55 (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (if (/= a b) 1 0))))
      (#x56
       (let ((v (round-according-to-state (gs-round-state gs) (vs-pop s) 0)))
         (vs-push s (if (logbitp 6 v) 1 0))))
      (#x57
       (let ((v (round-according-to-state (gs-round-state gs) (vs-pop s) 0)))
         (vs-push s (if (logbitp 6 v) 0 1))))
      (#x58 (when (zerop (vs-pop s)) (skip-code-until eng t nil)))
      (#x59 nil)
      (#x5A (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (if (and (/= a 0) (/= b 0)) 1 0))))
      (#x5B (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (if (or (/= a 0) (/= b 0)) 1 0))))
      (#x5C (vs-push s (if (zerop (vs-pop s)) 1 0)))
      (#x5D (op-deltap eng 0))
      (#x5E (setf (gs-slots-dirty gs) t) (setf (gs-delta-base gs) (vs-pop s)))
      (#x5F (setf (gs-slots-dirty gs) t) (setf (gs-delta-shift gs) (min 6 (max 0 (vs-pop s)))))
      (#x60 (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (f26+ a b))))
      (#x61 (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (f26- a b))))
      (#x62 (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (f26-div a b))))
      (#x63 (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (f26-mul a b))))
      (#x64 (vs-push s (f26-abs (vs-pop s))))
      (#x65 (vs-push s (f26-neg (vs-pop s))))
      (#x66 (vs-push s (f26-floor (vs-pop s))))
      (#x67 (vs-push s (f26-ceil (vs-pop s))))
      (#x70
       (let* ((val (vs-pop s)) (idx (vs-pop s)) (cvt (eng-cvt eng)))
         (setf (eng-tables-dirty eng) t)
         (when (< -1 idx (length cvt))
           (setf (aref cvt idx) (mul-fix val (eng-cvt-scale eng))))))
      (#x71 (op-deltap eng 16))
      (#x72 (op-deltap eng 32))
      (#x73 (op-deltac eng 0))
      (#x74 (op-deltac eng 16))
      (#x75 (op-deltac eng 32))
      (#x76 (setf (gs-slots-dirty gs) t) (set-super-round (gs-round-state gs) nil (vs-pop s)))
      (#x77 (setf (gs-slots-dirty gs) t) (set-super-round (gs-round-state gs) t   (vs-pop s)))
      (#x78 (let ((c (vs-pop s)) (off (vs-pop s))) (unless (zerop c) (do-jump eng off))))
      (#x79 (let ((c (vs-pop s)) (off (vs-pop s))) (when (zerop c) (do-jump eng off))))
      (#x7A (setf (gs-slots-dirty gs) t) (setf (rs-mode (gs-round-state gs)) :off))
      (#x7C (setf (gs-slots-dirty gs) t) (setf (rs-mode (gs-round-state gs)) :up-to-grid))
      (#x7D (setf (gs-slots-dirty gs) t) (setf (rs-mode (gs-round-state gs)) :down-to-grid))
      ((#x7E #x7F) (vs-pop s))
      (#x80
       (let ((count (gs-loop gs)) (z0 (eng-zp0 eng)))
         (setf (gs-loop gs) 1)
         (when (< (vs-len s) count) (setf count 0))
         (dotimes (i count)
           (let ((pt (vs-pop s)))
             (zone-check-point z0 pt)
             (unless (gs-backward-compatibility gs)
               (let ((idx (+ (zone-base z0) pt)))
                 (declare (fixnum idx))
                 (setf (aref (zone-flags z0) idx)
                       (logxor (aref (zone-flags z0) idx) +point-on-curve+))))))))
      ((#x81 #x82)
       (let ((hi (vs-pop s)) (lo (vs-pop s)) (z0 (eng-zp0 eng)))
         (unless (gs-backward-compatibility gs)
           (loop for pt from lo to hi
                 do (zone-check-point z0 pt)
                    (let ((idx (+ (zone-base z0) pt)))
                      (declare (fixnum idx))
                      (setf (aref (zone-flags z0) idx)
                            (if (= op #x81)
                                (logior (aref (zone-flags z0) idx) +point-on-curve+)
                                (logandc2 (aref (zone-flags z0) idx) +point-on-curve+))))))))
      (#x85 (vs-pop s))
      (#x88
       (let ((sel (vs-pop s))
             (res 0))
         (when (logbitp 0 sel) (setf res 40))
         (when (and (logbitp 1 sel) (eng-rotated-p eng))   (setf res (logior res (ash 1 8))))
         (when (and (logbitp 2 sel) (eng-stretched-p eng)) (setf res (logior res (ash 1 9))))
         (when (and (logbitp 3 sel) (plusp (length (eng-axis-coords eng))))
           (setf res (logior res (ash 1 10))))
         ;; Smooth (non-mono) rendering with ClearType-style hinting, as
         ;; FreeType's default target: grayscale (selector bit 5) is not
         ;; reported, the others are.
         (when (logbitp 6 sel) (setf res (logior res (ash 1 13))))
         (when (logbitp 10 sel) (setf res (logior res (ash 1 17))))
         ;; Selector bit 11, symmetrical smoothing, is reported above 20 ppem
         ;; only.  This is found by experiment against FreeType 2.15, not
         ;; documented: with it Liberation Sans' outlines match FreeType's
         ;; at every size from 8
         ;; to 40 ppem, and without it they differ from 21 ppem up (or, if
         ;; it is always reported, at every size below).
         (when (and (logbitp 11 sel) (> (eng-ppem eng) 20))
           (setf res (logior res (ash 1 18))))
         (when (logbitp 12 sel) (setf res (logior res (ash 1 19))))
         (vs-push s res)))
      (#x89 (op-idef eng))
      (#x8A (vs-roll s))
      (#x8B (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (max a b))))
      (#x8C (let ((b (vs-pop s)) (a (vs-pop s))) (vs-push s (min a b))))
      (#x8D (vs-pop s))
      (#x8E
       (let ((selector (vs-pop s))
             (val (vs-pop s)))
         (when (and (= (eng-cur-program eng) 1) (<= 1 selector 3))
           (setf (gs-slots-dirty gs) t)
           (let ((bit (ash 1 (1- selector))))
             (setf (gs-instruct-control gs)
                   (if (zerop val)
                       (logandc2 (gs-instruct-control gs) bit)
                       (logior (gs-instruct-control gs) bit)))
             (when (= selector 3)
               (setf (gs-backward-compatibility gs) (zerop val)))))))
      (#x91
       (let ((coords (eng-axis-coords eng)))
         (if (plusp (length coords))
             (loop for c across coords do (vs-push s c))
             (vs-push s 0))))
      (t
       (let ((idef (loop for d across (eng-idefs eng)
                         when (and (fdef-active-p d) (= (fdef-key d) op))
                           return d)))
         (if idef
             (invoke-function eng idef 1)
             (hint-fail 'tt-hint-invalid-opcode
                        (format nil "Unknown TrueType opcode #x~2,'0X" op))))))))
