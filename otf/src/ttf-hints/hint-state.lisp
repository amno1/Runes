;;;; hint-state.lisp — TrueType hinting math, rounding, zones, and graphics state
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; A port of Fontations' skrifa src/outline/glyf/hint/ (math.rs, round.rs,
;;; graphics.rs, zone.rs), which follows FreeType's TrueType bytecode
;;; interpreter (ttinterp.c) in v40 subpixel mode.

(defpackage #:runes/otf/tt-hint
  (:use #:cl)
  (:export
   #:tt-hint-error #:tt-hint-Stack-overflow #:tt-hint-stack-underflow
   #:tt-hint-invalid-opcode #:tt-hint-invalid-zone #:tt-hint-limit-exceeded
   ;; Zones & Points
   #:tt-zone #:make-tt-zone #:zone-unscaled-x #:zone-unscaled-y
   #:zone-orig-x #:zone-orig-y #:zone-cur-x #:zone-cur-y
   #:zone-flags #:zone-contours #:zone-n-points #:zone-n-contours
   #:+point-on-curve+ #:+point-touched-x+ #:+point-touched-y+ #:+point-touched+
   ;; Graphics State
   #:graphics-state #:make-graphics-state #:reset-graphics-state-per-glyph
   ;; Rounding
   #:round-according-to-state #:parse-super-round))

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

(declaim (inline s32 f26+ f26- f26-neg f26-abs f26-mul f26-div
                 mul-div mul-fix div-fix mul-f2dot14 dot-f2dot14))

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

(defun mul-div-no-round (a b c)
  "FreeType's FT_MulDiv_No_Round: (a * b) / c truncated toward zero."
  (declare (s32 a b c) (optimize (speed 3) (safety 0)))
  (let* ((neg (not (eq (minusp a) (not (eq (minusp b) (minusp c))))))
         (aa (abs a)) (ab (abs b)) (ac (abs c))
         (res (if (zerop ac)
                  #x7FFFFFFF
                  (values (truncate (* aa ab) ac)))))
    (if neg (s32 (- (s32 res))) (s32 res))))

(defun f26-mul (a b)
  "Multiply two 26.6 numbers: (a * b + 32) >> 6."
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (mul-div a b 64))

(defun f26-div (a b)
  "Divide two 26.6 numbers: (a << 6) / b."
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (mul-div a 64 b))

(defun mul-fix (a b)
  "FreeType's FT_MulFix (16.16)."
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (let ((ab (* a b)))
    (s32 (ash (+ ab #x8000 (if (minusp ab) -1 0)) -16))))

(defun div-fix (a b)
  "FreeType's FT_DivFix (16.16)."
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (mul-div a #x10000 b))

(defun mul-f2dot14 (a b)
  "Multiply a value by a 2.14 unit vector component: (a * b + 8192) >> 14."
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (let ((ab (* a b)))
    (s32 (ash (+ ab #x2000 (if (minusp ab) -1 0)) -14))))

(defun dot-f2dot14 (ax ay bx by)
  "Dot product of two 2.14 vectors, returning a 2.14 value."
  (declare (s32 ax ay bx by) (optimize (speed 3) (safety 0)))
  (let ((ab (+ (* ax bx) (* ay by))))
    (s32 (ash (+ ab #x2000 (if (minusp ab) -1 0)) -14))))

(defun normalize-f2dot14 (dx dy)
  "Normalize vector (DX, DY) into a 2.14 unit vector (values ux uy).
Matches FreeType's FT_Vector_NormLen / TT_Normalize."
  (declare (s32 dx dy) (optimize (speed 3) (safety 0)))
  (if (and (zerop dx) (zerop dy))
      (values #x4000 0)
      (let* ((ax (abs dx))
             (ay (abs dy))
             ;; Exact integer square root in 64-bit integer arithmetic
             ;; (SBCL fixnums on x86-64 are 62-bit, so dx^2 + dy^2 never boxes!).
             (len-sq (+ (* ax ax) (* ay ay)))
             (len (isqrt len-sq)))
        (if (zerop len)
            (values #x4000 0)
            ;; Improve 1-bit precision with one Newton step scaled up, or direct floor:
            ;; FreeType normalizes to 0x4000 (16384).
            (let* ((shift (max 0 (- 14 (floor (integer-length len-sq) 2))))
                   (sx (ash dx shift))
                   (sy (ash dy shift))
                   (norm (isqrt (+ (* sx sx) (* sy sy)))))
              (if (zerop norm)
                  (values #x4000 0)
                  (values (s32 (truncate (* sx #x4000) norm))
                          (s32 (truncate (* sy #x4000) norm)))))))))

;;; ============================================================================
;;; TrueType Rounding Engine (round.rs)
;;; ============================================================================

(defstruct (round-state (:conc-name rs-))
  (mode :to-grid :type symbol) ; :to-half-grid :to-grid :to-double-grid :down-to-grid :up-to-grid :off :super :super-45
  (threshold 0 :type s32)
  (phase 0 :type s32)
  (period 64 :type s32))

(defun round-according-to-state (rs distance compensation)
  "Round F26Dot6 DISTANCE with COMPENSATION according to ROUND-STATE RS.
Matches FreeType's Round_* functions in ttinterp.c."
  (declare (round-state rs) (s32 distance compensation) (optimize (speed 3) (safety 0)))
  (let ((mode (rs-mode rs)))
    (when (eq mode :off)
      (return-from round-according-to-state distance))
    (let* ((sign (if (>= distance 0) 1 -1))
           (val (if (>= distance 0)
                    (f26+ distance compensation)
                    (f26+ (f26-neg distance) compensation)))
           (res
             (ecase mode
               (:to-grid
                (logand (f26+ val 32) -64))
               (:to-half-grid
                (f26+ (f26-floor val) 32))
               (:to-double-grid
                (logand (f26+ val 16) -32))
               (:down-to-grid
                (f26-floor val))
               (:up-to-grid
                (f26-ceil val))
               ((:super :super-45)
                (let* ((period (rs-period rs))
                       (phase (rs-phase rs))
                       (threshold (rs-threshold rs))
                       (v (logand (f26+ val (- threshold phase)) (- period))))
                  (f26+ v phase))))))
      ;; FreeType clamps negative results after rounding to 0 before reapplying sign.
      (let ((clamped (max 0 res)))
        (if (> sign 0) clamped (f26-neg clamped))))))

(defun parse-super-round (rs selector14-p)
  "Configure RS for SROUND (SELECTOR14-P = nil) or S45ROUND (SELECTOR14-P = t)
given the top byte of the stack operand."
  (declare (round-state rs))
  (lambda (grid-period-byte)
    (declare (s32 grid-period-byte))
    (let* ((period-code (ldb (byte 2 6) grid-period-byte))
           (phase-code (ldb (byte 2 4) grid-period-byte))
           (thresh-code (ldb (byte 4 0) grid-period-byte))
           (period (if selector14-p
                       ;; S45ROUND: period is based on 64 * sqrt(2)/2 ~= 45.25 -> 1448 >> 5
                       (ecase period-code
                         (0 (ash 1448 -6))
                         (1 (ash 1448 -5))
                         (2 (ash 1448 -4))
                         (3 (ash 1448 -5)))
                       ;; SROUND: 32, 64, 128
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
                                  (ash (* (- thresh-code 4) period) -3))))))

;;; ============================================================================
;;; Point Zones (zone.rs): Zone 0 (Twilight) and Zone 1 (Glyph)
;;; ============================================================================

(defconstant +point-on-curve+  #x01)
(defconstant +point-touched-x+ #x02)
(defconstant +point-touched-y+ #x04)
(defconstant +point-touched+   (logior +point-touched-x+ +point-touched-y+))

(defstruct (tt-zone (:conc-name zone-))
  ;; Unscaled font units (for GETVARIATION / SDPVTL / GC[orig] in some cases)
  (unscaled-x (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (unscaled-y (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  ;; Scaled 26.6 original coordinates (before hinting)
  (orig-x (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (orig-y (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  ;; Current 26.6 hinted coordinates
  (cur-x (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (cur-y (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  ;; Point flags (on-curve, touched-x, touched-y)
  (flags (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  ;; End point indices of each contour
  (contours (make-array 0 :element-type '(unsigned-byte 16)) :type (simple-array (unsigned-byte 16) (*)))
  (n-points 0 :type fixnum)
  (n-contours 0 :type fixnum))

(defun allocate-zone (max-points max-contours)
  (make-tt-zone
   :unscaled-x (make-array max-points :element-type 's32 :initial-element 0)
   :unscaled-y (make-array max-points :element-type 's32 :initial-element 0)
   :orig-x     (make-array max-points :element-type 's32 :initial-element 0)
   :orig-y     (make-array max-points :element-type 's32 :initial-element 0)
   :cur-x      (make-array max-points :element-type 's32 :initial-element 0)
   :cur-y      (make-array max-points :element-type 's32 :initial-element 0)
   :flags      (make-array max-points :element-type '(unsigned-byte 8) :initial-element 0)
   :contours   (make-array max-contours :element-type '(unsigned-byte 16) :initial-element 0)
   :n-points   max-points
   :n-contours max-contours))

;;; ============================================================================
;;; Graphics State (graphics.rs)
;;; ============================================================================

(defstruct (graphics-state (:conc-name gs-) (:copier copy-graphics-state))
  ;; Vectors (F2Dot14: #x4000 = 1.0)
  (proj-x #x4000 :type s32)
  (proj-y 0      :type s32)
  (dual-x #x4000 :type s32)
  (dual-y 0      :type s32)
  (free-x #x4000 :type s32)
  (free-y 0      :type s32)
  ;; Freedoms/Projection fast-path flags (:x, :y, or :both)
  (coord-mode :x :type symbol)
  ;; Reference points
  (rp0 0 :type fixnum)
  (rp1 0 :type fixnum)
  (rp2 0 :type fixnum)
  ;; Zone pointers (0 = Twilight, 1 = Glyph)
  (zp0 1 :type (integer 0 1))
  (zp1 1 :type (integer 0 1))
  (zp2 1 :type (integer 0 1))
  ;; Control Value & Distance thresholds (F26Dot6)
  (control-value-cutin 68 :type s32) ; 17/16 pixels = 68 in 26.6
  (single-width-cutin 0   :type s32)
  (single-width-value 0   :type s32)
  (min-distance 64        :type s32) ; 1 pixel = 64 in 26.6
  ;; Delta state
  (delta-base 9  :type fixnum)
  (delta-shift 3 :type fixnum)
  ;; Loop counter for SLOOP
  (loop 1 :type fixnum)
  ;; Auto-flip boolean for MIRP/MDRP
  (auto-flip t :type boolean)
  ;; Scan conversion parameters (stored for compliance, used if monochrome)
  (scan-control nil :type boolean)
  (scan-type 0      :type s32)
  ;; Instruction control flags (bit 0: inhibit grid-fitting, bit 1: ignore prep state,
  ;; bit 2: native ClearType mode)
  (instruct-control 0 :type (unsigned-byte 8))
  ;; v40 Subpixel / Backward compatibility mode:
  ;; In v40, X movement is blocked unless the font sets INSTCTRL bit 2 (native ClearType).
  (backward-compatibility t :type boolean)
  ;; Rounding state
  (round-state (make-round-state) :type round-state))

(defun reset-graphics-state-per-glyph (gs default-gs)
  "Reset GS before running a glyph's instructions.
If INSTCTRL bit 1 is set, reset to spec defaults; otherwise restore DEFAULT-GS
(which was recorded at the end of `prep') and reset only per-glyph fields."
  (declare (graphics-state gs default-gs))
  (let ((source (if (logbitp 1 (gs-instruct-control default-gs))
                    (make-graphics-state
                     :instruct-control (gs-instruct-control default-gs)
                     :backward-compatibility (gs-backward-compatibility default-gs))
                    default-gs)))
    (setf (gs-proj-x gs) #x4000 (gs-proj-y gs) 0
          (gs-dual-x gs) #x4000 (gs-dual-y gs) 0
          (gs-free-x gs) #x4000 (gs-free-y gs) 0
          (gs-coord-mode gs) :x
          (gs-rp0 gs) 0 (gs-rp1 gs) 0 (gs-rp2 gs) 0
          (gs-zp0 gs) 1 (gs-zp1 gs) 1 (gs-zp2 gs) 1
          (gs-loop gs) 1
          (gs-auto-flip gs) t
          (gs-control-value-cutin gs) (gs-control-value-cutin source)
          (gs-single-width-cutin gs)  (gs-single-width-cutin source)
          (gs-single-width-value gs)  (gs-single-width-value source)
          (gs-min-distance gs)        (gs-min-distance source)
          (gs-delta-base gs)          (gs-delta-base source)
          (gs-delta-shift gs)         (gs-delta-shift source)
          (gs-scan-control gs)        (gs-scan-control source)
          (gs-scan-type gs)           (gs-scan-type source)
          (gs-instruct-control gs)    (gs-instruct-control source)
          (gs-backward-compatibility gs) (gs-backward-compatibility source)
          (gs-round-state gs)         (copy-structure (gs-round-state source)))
    gs))
