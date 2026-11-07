;;;; hint-state.lisp — TrueType hinting package, conditions, math, rounding, zones and graphics state
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(defpackage #:runes/otf/tt-hint
  (:use #:cl)
  (:export
   #:tt-hint-error #:tt-hint-stack-overflow #:tt-hint-stack-underflow
   #:tt-hint-invalid-opcode #:tt-hint-invalid-zone #:tt-hint-limit-exceeded
   ;; Zones & Points
   #:tt-zone #:make-tt-zone #:ensure-zone-capacity #:copy-zone-into
   #:zone-unscaled-x #:zone-unscaled-y
   #:zone-orig-x #:zone-orig-y #:zone-cur-x #:zone-cur-y
   #:zone-flags #:zone-contours #:zone-n-points #:zone-n-glyph-points #:zone-n-contours
   #:+point-on-curve+ #:+point-touched-x+ #:+point-touched-y+ #:+point-touched+
   ;; Graphics State & Engine
   #:graphics-state #:make-graphics-state #:reset-graphics-state-per-glyph
   #:tt-engine #:make-tt-engine #:run-program
   ;; Rounding
   #:round-according-to-state #:set-super-round))

(in-package #:runes/otf/tt-hint)

;;; ============================================================================
;;; Conditions
;;; ============================================================================

(define-condition tt-hint-error (error)
  ((message :initarg :message :initform nil :reader tt-hint-error-message))
  (:report (lambda (c s)
             (format s "~A~@[: ~A~]" (type-of c) (tt-hint-error-message c)))))

(define-condition tt-hint-stack-overflow (tt-hint-error) ())
(define-condition tt-hint-stack-underflow (tt-hint-error) ())
(define-condition tt-hint-invalid-opcode (tt-hint-error) ())
(define-condition tt-hint-invalid-zone (tt-hint-error) ())
(define-condition tt-hint-limit-exceeded (tt-hint-error) ())

(defun hint-fail (type &optional message)
  (error type :message message))

;;; ============================================================================
;;; 32-bit Fixed-Point & F26Dot6 / F2Dot14 Math (math.rs)
;;; ============================================================================

(deftype s32 () '(signed-byte 32))

(declaim (inline s32 f26+ f26- f26-neg f26-abs f26-floor f26-ceil
                 f26-mul f26-div mul-div mul-fix div-fix
                 round-shift-14 mul-f2dot14 dot-f2dot14))

(defun s32 (x)
  "X reduced to a signed 32-bit integer, wrapping."
  (declare (integer x) (optimize (speed 3) (safety 0)))
  (sb-c::mask-signed-field 32 (ldb (byte 32 0) x)))

(defun f26+ (a b) (declare (s32 a b) (optimize (speed 3) (safety 0))) (s32 (+ a b)))
(defun f26- (a b) (declare (s32 a b) (optimize (speed 3) (safety 0))) (s32 (- a b)))
(defun f26-neg (a) (declare (s32 a) (optimize (speed 3) (safety 0))) (s32 (- a)))
(defun f26-abs (a) (declare (s32 a) (optimize (speed 3) (safety 0))) (s32 (abs a)))

(defun f26-floor (x)
  (declare (s32 x) (optimize (speed 3) (safety 0)))
  (logand x -64))

(defun f26-ceil (x)
  (declare (s32 x) (optimize (speed 3) (safety 0)))
  (f26-floor (f26+ x 63)))

(defun mul-div (a b c)
  "FreeType's FT_MulDiv: (a * b) / c with rounding to nearest."
  (declare (s32 a b c) (optimize (speed 3) (safety 0)))
  (let* ((neg (not (eq (minusp a) (not (eq (minusp b) (minusp c))))))
         (aa (abs a)) (ab (abs b)) (ac (abs c))
         (res (if (zerop ac)
                  #x7FFFFFFF
                  (values (floor (+ (* aa ab) (ash ac -1)) ac)))))
    (if neg (s32 (- (s32 res))) (s32 res))))

(defun f26-mul (a b)
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (mul-div a b 64))

(defun f26-div (a b)
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (mul-div a 64 b))

(defun mul-fix (a b)
  "FreeType's FT_MulFix (16.16)."
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (let ((ab (* a b)))
    (s32 (ash (+ ab #x8000 (ash ab -63)) -16))))

(defun div-fix (a b)
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (mul-div a #x10000 b))

(defun round-shift-14 (ab)
  "Sign-preserving rounded right shift by 14 for F2Dot14 products."
  (declare (fixnum ab) (optimize (speed 3) (safety 0)))
  (s32 (ash (+ ab #x2000 (ash ab -63)) -14)))

(defun mul-f2dot14 (a b)
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (round-shift-14 (* a b)))

(defun dot-f2dot14 (ax ay bx by)
  (declare (s32 ax ay bx by) (optimize (speed 3) (safety 0)))
  (round-shift-14 (+ (* ax bx) (* ay by))))

(defun normalize-f2dot14 (dx dy)
  "Normalize vector (DX, DY) into a 2.14 unit vector (values ux uy), as
FreeType's FT_Vector_NormLen does (a Newton iteration in 32-bit integer
arithmetic), so that the result agrees with its to the last bit."
  (declare (s32 dx dy) (optimize (speed 3) (safety 0)))
  (flet ((u32 (v) (ldb (byte 32 0) v)))
    (declare (inline u32))
    (let ((sx (if (minusp dx) -1 1))
          (sy (if (minusp dy) -1 1))
          (ux (u32 (abs dx)))
          (uy (u32 (abs dy))))
      (declare (fixnum sx sy) (type (unsigned-byte 32) ux uy))
      (cond
        ((zerop ux)
         (values 0 (if (plusp uy) (truncate (* sy #x10000) 4) 0)))
        ((zerop uy)
         (values (truncate (* sx #x10000) 4) 0))
        (t
         (flet ((len-of (ux uy)
                  (declare (type (unsigned-byte 32) ux uy))
                  (u32 (if (> ux uy) (+ ux (ash uy -1)) (+ uy (ash ux -1))))))
           (let* ((len (len-of ux uy))
                  (lz (- 32 (integer-length len)))
                  (shift (- lz (+ 15 (if (>= len (ash #xAAAAAAAA (- lz))) 1 0)))))
             (declare (type (unsigned-byte 32) len) (fixnum lz shift))
             (if (plusp shift)
                 (setf ux (u32 (ash ux shift))
                       uy (u32 (ash uy shift))
                       len (len-of ux uy))
                 (setf ux (ash ux shift)
                       uy (ash uy shift)
                       len (ash len shift)))
             (let* ((b (s32 (- #x10000 len)))
                    (x (s32 ux))
                    (y (s32 uy))
                    (u 0) (v 0))
               (declare (s32 b x y) (type (unsigned-byte 32) u v))
               (loop
                 (setf u (u32 (+ x (ash (s32 (* x b)) -16)))
                       v (u32 (+ y (ash (s32 (* y b)) -16))))
                 (let* ((z (truncate (s32 (- (s32 (u32 (+ (* u u) (* v v)))))) #x200))
                        (z (truncate (s32 (* z (ash (s32 (+ #x10000 b)) -8))) #x10000)))
                   (declare (s32 z))
                   (setf b (s32 (+ b z)))
                   (when (<= z 0) (return))))
               (values (truncate (s32 (* (s32 u) sx)) 4)
                       (truncate (s32 (* (s32 v) sy)) 4))))))))))

;;; ============================================================================
;;; TrueType Rounding Engine (round.rs)
;;; ============================================================================

(defstruct (round-state (:conc-name rs-))
  (mode :to-grid :type symbol)
  (threshold 0 :type s32)
  (phase 0 :type s32)
  (period 64 :type s32))

(defun round-according-to-state (rs distance compensation)
  "Round F26Dot6 DISTANCE with COMPENSATION according to ROUND-STATE RS."
  (declare (round-state rs) (s32 distance compensation) (optimize (speed 3) (safety 0)))
  (let ((mode (rs-mode rs)))
    (case mode
      ;; The usual one first: as the general case below, without its dispatch.
      (:to-grid
       (if (>= distance 0)
           (max 0 (logand (f26+ (f26+ distance compensation) 32) -64))
           (f26-neg (max 0 (logand (f26+ (f26+ (f26-neg distance) compensation) 32) -64)))))
      (:off distance)
      ((:super :super-45)
       ;; FreeType's Round_Super and Round_Super_45: the period is a power of
       ;; two for SROUND, so it masks; for S45ROUND it divides, truncating
       ;; towards zero as C does.  A result on the wrong side of zero is the
       ;; phase.
       (let ((period (rs-period rs))
             (phase (rs-phase rs))
             (threshold (rs-threshold rs))
             (45-p (eq mode :super-45)))
         (declare (s32 period phase threshold))
         (if (<= period 0)
             distance
             (flet ((grid (x)
                      (declare (s32 x))
                      (if 45-p
                          (* (truncate x period) period)
                          (logand x (- period)))))
               (declare (inline grid))
               (if (>= distance 0)
                   (let ((val (f26+ (grid (f26+ distance (f26+ (f26- threshold phase) compensation)))
                                    phase)))
                     (if (< val 0) phase val))
                   (let ((val (f26- (f26-neg (grid (f26- (f26+ (f26- threshold phase) compensation)
                                                         distance)))
                                    phase)))
                     (if (> val 0) (f26-neg phase) val)))))))
      (t
       (let* ((sign (if (>= distance 0) 1 -1))
              (val (if (>= distance 0)
                       (f26+ distance compensation)
                       (f26+ (f26-neg distance) compensation)))
              (res
                (ecase mode
                  (:to-grid        (logand (f26+ val 32) -64))
                  (:to-half-grid   (f26+ (f26-floor val) 32))
                  (:to-double-grid (logand (f26+ val 16) -32))
                  (:down-to-grid   (f26-floor val))
                  (:up-to-grid     (f26-ceil val))))
              (clamped (max 0 res)))
         (if (> sign 0) clamped (f26-neg clamped)))))))

(defun set-super-round (rs selector14-p grid-period-byte)
  "Configure RS for SROUND (SELECTOR14-P = nil) or S45ROUND (SELECTOR14-P = t)."
  (declare (round-state rs) (s32 grid-period-byte))
  (let* ((period-code (ldb (byte 2 6) grid-period-byte))
         (phase-code (ldb (byte 2 4) grid-period-byte))
         (thresh-code (ldb (byte 4 0) grid-period-byte))
         (period (if selector14-p
                     (ecase period-code
                       (0 (ash 1448 -6))
                       (1 (ash 1448 -5))
                       (2 (ash 1448 -4))
                       (3 (ash 1448 -5)))
                     (ecase period-code
                       (0 32)
                       (1 64)
                       (2 128)
                       (3 64))))
         (phase (ecase phase-code
                  (0 0)
                  (1 (ash period -2))
                  (2 (ash period -1))
                  (3 (if selector14-p
                         (ash (* 1448 3) -7)
                         (* 3 (ash period -2)))))))
    (setf (rs-mode rs) (if selector14-p :super-45 :super)
          (rs-period rs) period
          (rs-phase rs) phase
          (rs-threshold rs) (if (zerop thresh-code)
                                (1- period)
                                (ash (* (- thresh-code 4) period) -3)))))

;;; ============================================================================
;;; Point Zones with Arena Buffer Reuse (zone.rs)
;;; ============================================================================

(defconstant +point-on-curve+  #x01)
(defconstant +point-touched-x+ #x02)
(defconstant +point-touched-y+ #x04)
(defconstant +point-touched+   (logior +point-touched-x+ +point-touched-y+))

;;; Phantom points: the last four points of the glyph zone, pp1 to pp4.  In
;;; backward compatibility mode the hinted glyph's phantom points are the
;;; unrounded scaled ones, so pp1.x may be a fraction of a pixel; whoever
;;; returns the coordinates of a hinted outline must move it by -pp1.x
;;; (SHIFT-OUTLINE-BY-PP1 in scaler.lisp).

(defstruct (tt-zone (:conc-name zone-))
  (unscaled-x (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (unscaled-y (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (orig-x     (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (orig-y     (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (cur-x      (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (cur-y      (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (flags      (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (contours   (make-array 0 :element-type '(unsigned-byte 16)) :type (simple-array (unsigned-byte 16) (*)))
  ;; Active counts for the current glyph (n-points includes the 4 phantom points;
  ;; n-glyph-points excludes them so SHZ never depends on magic -4 subtraction).
  (n-points 0 :type fixnum)
  (n-glyph-points 0 :type fixnum)
  (n-contours 0 :type fixnum))

(declaim (inline ensure-zone-capacity))
(defun ensure-zone-capacity (zone needed-points needed-contours &key (n-glyph-points needed-points))
  "Ensure ZONE's underlying arrays can hold NEEDED-POINTS and NEEDED-CONTOURS,
reusing existing buffers in place when large enough."
  (declare (tt-zone zone) (fixnum needed-points needed-contours n-glyph-points))
  (when (> needed-points (length (zone-cur-x zone)))
    (let ((cap (max needed-points (* 2 (length (zone-cur-x zone))) 64)))
      (setf (zone-unscaled-x zone) (make-array cap :element-type 's32 :initial-element 0)
            (zone-unscaled-y zone) (make-array cap :element-type 's32 :initial-element 0)
            (zone-orig-x zone)     (make-array cap :element-type 's32 :initial-element 0)
            (zone-orig-y zone)     (make-array cap :element-type 's32 :initial-element 0)
            (zone-cur-x zone)      (make-array cap :element-type 's32 :initial-element 0)
            (zone-cur-y zone)      (make-array cap :element-type 's32 :initial-element 0)
            (zone-flags zone)      (make-array cap :element-type '(unsigned-byte 8) :initial-element 0))))
  (when (> needed-contours (length (zone-contours zone)))
    (let ((cap (max needed-contours (* 2 (length (zone-contours zone))) 16)))
      (setf (zone-contours zone) (make-array cap :element-type '(unsigned-byte 16) :initial-element 0))))
  (setf (zone-n-points zone) needed-points
        (zone-n-glyph-points zone) n-glyph-points
        (zone-n-contours zone) needed-contours)
  zone)

(defun allocate-zone (max-points max-contours)
  (ensure-zone-capacity (make-tt-zone) max-points max-contours))

(defun copy-zone-into (dst src)
  "Copy active points/flags of SRC into DST in place (used to snapshot/restore twilight)."
  (declare (tt-zone dst src))
  (let ((n (zone-n-points src))
        (c (zone-n-contours src)))
    (ensure-zone-capacity dst n c :n-glyph-points (zone-n-glyph-points src))
    (replace (zone-unscaled-x dst) (zone-unscaled-x src) :end1 n :end2 n)
    (replace (zone-unscaled-y dst) (zone-unscaled-y src) :end1 n :end2 n)
    (replace (zone-orig-x dst)     (zone-orig-x src)     :end1 n :end2 n)
    (replace (zone-orig-y dst)     (zone-orig-y src)     :end1 n :end2 n)
    (replace (zone-cur-x dst)      (zone-cur-x src)      :end1 n :end2 n)
    (replace (zone-cur-y dst)      (zone-cur-y src)      :end1 n :end2 n)
    (replace (zone-flags dst)      (zone-flags src)      :end1 n :end2 n)
    (replace (zone-contours dst)   (zone-contours src)   :end1 c :end2 c)
    dst))

;;; ============================================================================
;;; Graphics State (graphics.rs)
;;; ============================================================================

(defstruct (graphics-state (:conc-name gs-) (:copier copy-graphics-state))
  (proj-x #x4000 :type s32)
  (proj-y 0      :type s32)
  (dual-x #x4000 :type s32)
  (dual-y 0      :type s32)
  (free-x #x4000 :type s32)
  (free-y 0      :type s32)
  ;; What the three vectors come to, kept up to date by UPDATE-PROJECTION-STATE
  ;; wherever one is set: 0 for the x axis, 1 for the y axis, 2 for any other;
  ;; whether a move along the freedom vector is a move along an axis
  ;; projected on itself; and the freedom vector dotted with the projection
  ;; vector, FDOTP.  Moving a point reads them for each point.
  (proj-axis 0 :type (integer 0 2))
  (dual-axis 0 :type (integer 0 2))
  (move-aligned t :type boolean)
  (fdotp #x4000 :type s32)
  (rp0 0 :type fixnum)
  (rp1 0 :type fixnum)
  (rp2 0 :type fixnum)
  (zp0 1 :type (integer 0 1))
  (zp1 1 :type (integer 0 1))
  (zp2 1 :type (integer 0 1))
  (control-value-cutin 68 :type s32)
  (single-width-cutin 0   :type s32)
  (single-width-value 0   :type s32)
  (min-distance 64        :type s32)
  (delta-base 9  :type fixnum)
  (delta-shift 3 :type fixnum)
  (loop 1 :type fixnum)
  (auto-flip t :type boolean)
  (scan-control nil :type boolean)
  (scan-type 0      :type s32)
  (instruct-control 0 :type (unsigned-byte 8))
  (backward-compatibility t :type boolean)
  (did-iup-x nil :type boolean)
  (did-iup-y nil :type boolean)
  (round-state (make-round-state) :type round-state))

;; Nothing subtypes these: say so, and a type test is one comparison.
(declaim (sb-ext:freeze-type round-state tt-zone graphics-state))

(defun update-projection-state (gs)
  "Recompute the cached axis and FDOTP of the graphics state GS after one of
its vectors changed.  The rules are FreeType's Compute_Funcs and the clamp of
a too small FDOTP."
  (declare (graphics-state gs) (optimize (speed 3) (safety 0)))
  (let* ((px (gs-proj-x gs)) (py (gs-proj-y gs))
         (dx (gs-dual-x gs)) (dy (gs-dual-y gs))
         (fx (gs-free-x gs)) (fy (gs-free-y gs))
         (raw (cond ((= fx #x4000) px)
                    ((= fy #x4000) py)
                    (t (dot-f2dot14 fx fy px py)))))
    (declare (s32 raw))
    (setf (gs-proj-axis gs) (cond ((and (= px #x4000) (zerop py)) 0)
                                  ((and (zerop px) (= py #x4000)) 1)
                                  (t 2))
          (gs-dual-axis gs) (cond ((and (= dx #x4000) (zerop dy)) 0)
                                  ((and (zerop dx) (= dy #x4000)) 1)
                                  (t 2))
          (gs-move-aligned gs) (and (= raw #x4000) (or (= fx #x4000) (= fy #x4000)))
          ;; At small sizes FDOTP can become too small and cause spikes.
          (gs-fdotp gs) (if (< (abs raw) #x400) #x4000 raw))
    gs))

(defun reset-graphics-state-per-glyph (gs default-gs)
  (declare (graphics-state gs default-gs))
  (let ((source (if (logbitp 1 (gs-instruct-control default-gs))
                    (make-graphics-state
                     :instruct-control (gs-instruct-control default-gs)
                     :backward-compatibility (gs-backward-compatibility default-gs))
                    default-gs)))
    (setf (gs-proj-x gs) #x4000 (gs-proj-y gs) 0
          (gs-dual-x gs) #x4000 (gs-dual-y gs) 0
          (gs-free-x gs) #x4000 (gs-free-y gs) 0
          ;; All three are the x axis: see UPDATE-PROJECTION-STATE.
          (gs-proj-axis gs) 0 (gs-dual-axis gs) 0
          (gs-move-aligned gs) t (gs-fdotp gs) #x4000
          (gs-rp0 gs) 0 (gs-rp1 gs) 0 (gs-rp2 gs) 0
          (gs-zp0 gs) 1 (gs-zp1 gs) 1 (gs-zp2 gs) 1
          (gs-loop gs) 1
          (gs-auto-flip gs) t
          (gs-did-iup-x gs) nil (gs-did-iup-y gs) nil
          (gs-control-value-cutin gs)    (gs-control-value-cutin source)
          (gs-single-width-cutin gs)     (gs-single-width-cutin source)
          (gs-single-width-value gs)     (gs-single-width-value source)
          (gs-min-distance gs)           (gs-min-distance source)
          (gs-delta-base gs)             (gs-delta-base source)
          (gs-delta-shift gs)            (gs-delta-shift source)
          (gs-scan-control gs)           (gs-scan-control source)
          (gs-scan-type gs)              (gs-scan-type source)
          (gs-instruct-control gs)       (gs-instruct-control source)
          (gs-backward-compatibility gs) (gs-backward-compatibility source)
          )
    ;; The round state is copied into the one GS has, which no other state
    ;; shares (see MAKE-TT-HINT-ENGINE), not into a new one.
    (let ((rs (gs-round-state gs)) (from (gs-round-state source)))
      (setf (rs-mode rs) (rs-mode from)
            (rs-threshold rs) (rs-threshold from)
            (rs-phase rs) (rs-phase from)
            (rs-period rs) (rs-period from)))
    gs))

;;; Function types, for callers in the files after this one.
(declaim (ftype (function (round-state s32 s32) s32) round-according-to-state)
         (ftype (function (graphics-state) graphics-state) update-projection-state)
         (ftype (function (graphics-state graphics-state) graphics-state)
                reset-graphics-state-per-glyph))
