;;;; wm.lisp — TrueType hinting math, rounding, arena zones, graphics state, and VM
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
    (s32 (ash (+ ab #x8000 (if (minusp ab) -1 0)) -16))))

(defun div-fix (a b)
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (mul-div a #x10000 b))

(defun round-shift-14 (ab)
  "Sign-preserving rounded right shift by 14 for F2Dot14 products."
  (declare (fixnum ab) (optimize (speed 3) (safety 0)))
  (s32 (ash (+ ab #x2000 (if (minusp ab) -1 0)) -14)))

(defun mul-f2dot14 (a b)
  (declare (s32 a b) (optimize (speed 3) (safety 0)))
  (round-shift-14 (* a b)))

(defun dot-f2dot14 (ax ay bx by)
  (declare (s32 ax ay bx by) (optimize (speed 3) (safety 0)))
  (round-shift-14 (+ (* ax bx) (* ay by))))

(defun normalize-f2dot14 (dx dy)
  "Normalize vector (DX, DY) into a 2.14 unit vector (values ux uy)."
  (declare (s32 dx dy) (optimize (speed 3) (safety 0)))
  (if (and (zerop dx) (zerop dy))
      (values #x4000 0)
      (let* ((ax (abs dx))
             (ay (abs dy))
             (len-sq (+ (* ax ax) (* ay ay))))
        (if (zerop len-sq)
            (values #x4000 0)
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
  (mode :to-grid :type symbol)
  (threshold 0 :type s32)
  (phase 0 :type s32)
  (period 64 :type s32))

(defun round-according-to-state (rs distance compensation)
  "Round F26Dot6 DISTANCE with COMPENSATION according to ROUND-STATE RS."
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
               (:to-grid        (logand (f26+ val 32) -64))
               (:to-half-grid   (f26+ (f26-floor val) 32))
               (:to-double-grid (logand (f26+ val 16) -32))
               (:down-to-grid   (f26-floor val))
               (:up-to-grid     (f26-ceil val))
               ((:super :super-45)
                (let* ((period (rs-period rs))
                       (phase (rs-phase rs))
                       (threshold (rs-threshold rs))
                       (v (logand (f26+ val (- threshold phase)) (- period))))
                  (f26+ v phase))))))
      (let ((clamped (max 0 res)))
        (if (> sign 0) clamped (f26-neg clamped))))))

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
  (round-state (make-round-state) :type round-state))

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
          (gs-rp0 gs) 0 (gs-rp1 gs) 0 (gs-rp2 gs) 0
          (gs-zp0 gs) 1 (gs-zp1 gs) 1 (gs-zp2 gs) 1
          (gs-loop gs) 1
          (gs-auto-flip gs) t
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
          (gs-round-state gs)            (copy-structure (gs-round-state source)))
    gs))

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
          (t (f26+ (mul-f2dot14 x px) (mul-f2dot14 y py))))))

(defun dual-project-point (gs x y)
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
    (when (not (zerop fx))
      (when (or (zerop zone-id) (not (gs-backward-compatibility gs)))
        (let ((dx (if (and (= fx #x4000) (= fdotp #x4000))
                      distance
                      (if (< (abs fdotp) #x400)
                          0
                          (mul-div distance fx fdotp)))))
          (setf (aref (zone-cur-x zone) pt)
                (f26+ (aref (zone-cur-x zone) pt) dx))))
      (when touch
        (setf (aref (zone-flags zone) pt)
              (logior (aref (zone-flags zone) pt) +point-touched-x+))))
    (when (not (zerop fy))
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

;;; ============================================================================
;;; Value Stack (value_stack.rs)
;;; ============================================================================

(defconstant +max-call-stack+ 128)
(defconstant +max-instructions+ 1000000)

(defstruct (value-stack (:conc-name vs-) (:constructor %make-value-stack))
  (values (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (top 0 :type fixnum))

(defun make-value-stack (max-elements)
  (%make-value-stack :values (make-array max-elements :element-type 's32 :initial-element 0)))

(declaim (inline vs-len vs-clear vs-push vs-pop vs-peek))

(defun vs-len (s) (declare (value-stack s)) (vs-top s))
(defun vs-clear (s) (declare (value-stack s)) (setf (vs-top s) 0))

(defun vs-push (s val)
  (declare (value-stack s) (s32 val) (optimize (speed 3) (safety 0)))
  (let ((top (vs-top s))
        (arr (vs-values s)))
    (when (>= top (length arr))
      (hint-fail 'tt-hint-stack-overflow "TrueType value stack overflow"))
    (setf (aref arr top) val
          (vs-top s) (1+ top))))

(defun vs-pop (s)
  (declare (value-stack s) (optimize (speed 3) (safety 0)))
  (let ((top (vs-top s)))
    (when (<= top 0)
      (hint-fail 'tt-hint-stack-underflow "TrueType value stack underflow"))
    (let ((new-top (1- top)))
      (setf (vs-top s) new-top)
      (aref (vs-values s) new-top))))

(defun vs-peek (s)
  (declare (value-stack s) (optimize (speed 3) (safety 0)))
  (let ((top (vs-top s)))
    (if (<= top 0) 0 (aref (vs-values s) (1- top)))))

(defun vs-dup (s)
  (declare (value-stack s))
  (let ((top (vs-top s)))
    (when (<= top 0)
      (hint-fail 'tt-hint-stack-underflow "DUP on empty stack"))
    (vs-push s (aref (vs-values s) (1- top)))))

(defun vs-swap (s)
  (declare (value-stack s))
  (let ((top (vs-top s))
        (arr (vs-values s)))
    (when (< top 2)
      (hint-fail 'tt-hint-stack-underflow "SWAP requires 2 elements"))
    (rotatef (aref arr (1- top)) (aref arr (- top 2)))))

(defun vs-roll (s)
  (declare (value-stack s))
  (let ((top (vs-top s))
        (arr (vs-values s)))
    (when (< top 3)
      (hint-fail 'tt-hint-stack-underflow "ROLL requires 3 elements"))
    (let ((a (aref arr (- top 3)))
          (b (aref arr (- top 2)))
          (c (aref arr (- top 1))))
      (setf (aref arr (- top 3)) b
            (aref arr (- top 2)) c
            (aref arr (- top 1)) a))))

(defun vs-copy-index (s idx)
  (declare (value-stack s) (fixnum idx))
  (let ((top (vs-top s)))
    (when (or (<= idx 0) (> idx top))
      (hint-fail 'tt-hint-stack-underflow "CINDEX index out of bounds"))
    (vs-push s (aref (vs-values s) (- top idx)))))

(defun vs-move-index (s idx)
  (declare (value-stack s) (fixnum idx))
  (let ((top (vs-top s))
        (arr (vs-values s)))
    (when (or (<= idx 0) (> idx top))
      (hint-fail 'tt-hint-stack-underflow "MINDEX index out of bounds"))
    (let* ((pos (- top idx))
           (val (aref arr pos)))
      (loop for i of-type fixnum from pos below (1- top)
            do (setf (aref arr i) (aref arr (1+ i))))
      (setf (aref arr (1- top)) val))))

;;; ============================================================================
;;; Program Definitions (FDEF / IDEF) and Call Stack
;;; ============================================================================

(deftype program-id () '(integer 0 2))

(defstruct (fn-def (:conc-name fdef-))
  (active-p nil :type boolean)
  (key 0 :type s32)       ; full s32 key for FDEF, or u8 opcode for IDEF
  (program 0 :type program-id)
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(defstruct (call-record (:conc-name call-))
  (caller-program 0 :type program-id)
  (return-pc 0 :type fixnum)
  (current-end 0 :type fixnum)
  (loop-count 1 :type fixnum)
  (def-start 0 :type fixnum)
  (def-program 0 :type program-id))

;;; ============================================================================
;;; VM Execution Context (engine.rs)
;;; ============================================================================

(defstruct (tt-engine (:conc-name eng-) (:constructor %make-tt-engine))
  (programs (vector #() #() #()) :type (simple-vector 3))
  (cur-program 0 :type program-id)
  (bytes (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (pc 0 :type fixnum)
  (end 0 :type fixnum)
  (stack nil :type value-stack)
  (call-stack (make-array +max-call-stack+) :type simple-vector)
  (call-top 0 :type fixnum)
  (fdefs (make-array 0) :type simple-vector)
  (idefs (make-array 0) :type simple-vector)
  (storage (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (default-storage (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (cvt (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (default-cvt (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (twilight nil :type tt-zone)
  (default-twilight nil :type tt-zone)
  (glyph nil :type tt-zone)
  (gs (make-graphics-state) :type graphics-state)
  (default-gs (make-graphics-state) :type graphics-state)
  (ppem 0 :type fixnum)
  (point-size 0 :type s32)
  (scale 0 :type s32)
  (cvt-scale 0 :type s32)
  (rotated-p nil :type boolean)
  (stretched-p nil :type boolean)
  (axis-coords #() :type simple-vector)
  (ops-executed 0 :type fixnum))

(defun make-tt-engine (&key (max-stack 256) (max-storage 64) (max-fdefs 64)
                            (max-idefs 8) (max-twilight 32) (cvt-len 0))
  (let ((call-stack (make-array +max-call-stack+))
        (fdefs (make-array (max max-fdefs 64)))
        (idefs (make-array (max max-idefs 8))))
    (dotimes (i +max-call-stack+) (setf (svref call-stack i) (make-call-record)))
    (dotimes (i (length fdefs))   (setf (svref fdefs i) (make-fn-def)))
    (dotimes (i (length idefs))   (setf (svref idefs i) (make-fn-def)))
    (%make-tt-engine
     :stack (make-value-stack (max max-stack 256))
     :call-stack call-stack
     :fdefs fdefs
     :idefs idefs
     :storage (make-array max-storage :element-type 's32 :initial-element 0)
     :default-storage (make-array max-storage :element-type 's32 :initial-element 0)
     :cvt (make-array cvt-len :element-type 's32 :initial-element 0)
     :default-cvt (make-array cvt-len :element-type 's32 :initial-element 0)
     :twilight (allocate-zone max-twilight 1)
     :default-twilight (allocate-zone max-twilight 1)
     :glyph (allocate-zone 128 16))))

(declaim (inline eng-zone eng-zp0 eng-zp1 eng-zp2 fetch-u8 fetch-i16))

(defun eng-zone (eng z)
  (declare (tt-engine eng) (type (integer 0 1) z) (optimize (speed 3) (safety 0)))
  (if (zerop z) (eng-twilight eng) (eng-glyph eng)))

(defun eng-zp0 (eng) (eng-zone eng (gs-zp0 (eng-gs eng))))
(defun eng-zp1 (eng) (eng-zone eng (gs-zp1 (eng-gs eng))))
(defun eng-zp2 (eng) (eng-zone eng (gs-zp2 (eng-gs eng))))

(defun fetch-u8 (eng)
  (declare (tt-engine eng) (optimize (speed 3) (safety 0)))
  (let ((pc (eng-pc eng)))
    (when (>= pc (eng-end eng))
      (hint-fail 'tt-hint-invalid-opcode "Bytecode stream ended unexpectedly"))
    (setf (eng-pc eng) (1+ pc))
    (aref (eng-bytes eng) pc)))

(defun fetch-i16 (eng)
  (declare (tt-engine eng) (optimize (speed 3) (safety 0)))
  (let* ((hi (fetch-u8 eng))
         (lo (fetch-u8 eng))
         (u (logior (ash hi 8) lo)))
    (if (logbitp 15 u) (- u 65536) u)))

(defun opcode-payload-bytes (eng op)
  (declare (tt-engine eng) (type (unsigned-byte 8) op) (optimize (speed 3) (safety 0)))
  (cond
    ((= op #x40) (fetch-u8 eng))
    ((= op #x41) (* 2 (fetch-u8 eng)))
    ((<= #xB0 op #xB7) (+ (- op #xB0) 1))
    ((<= #xB8 op #xBF) (* 2 (+ (- op #xB8) 1)))
    (t 0)))

(defun skip-code-until (eng stop-for-else-p stop-for-endf-p)
  (declare (tt-engine eng) (optimize (speed 3) (safety 0)))
  (let ((if-depth 0))
    (declare (fixnum if-depth))
    (loop
      (let* ((op (fetch-u8 eng))
             (payload (opcode-payload-bytes eng op)))
        (when (> (+ (eng-pc eng) payload) (eng-end eng))
          (hint-fail 'tt-hint-invalid-opcode "Truncated push payload while skipping bytecode"))
        (incf (eng-pc eng) payload)
        (cond
          (stop-for-endf-p
           (when (or (= op #x2C) (= op #x89))
             (hint-fail 'tt-hint-invalid-opcode "Nested FDEF/IDEF"))
           (when (= op #x2D)
             (return :endf)))
          (t
           (case op
             (#x58 (incf if-depth))
             (#x1B (when (and (zerop if-depth) stop-for-else-p)
                     (return :else)))
             (#x59 (if (zerop if-depth)
                       (return :eif)
                       (decf if-depth))))))))))

(defun select-program (eng prog-id start end)
  (declare (tt-engine eng) (type program-id prog-id) (fixnum start end))
  (let ((bytes (svref (eng-programs eng) prog-id)))
    (setf (eng-cur-program eng) prog-id
          (eng-bytes eng) bytes
          (eng-pc eng) start
          (eng-end eng) end)))

(defun find-fdef (eng key)
  "Find active FDEF for function number KEY."
  (declare (tt-engine eng) (s32 key))
  (let ((fdefs (eng-fdefs eng)))
    (or (when (< -1 key (length fdefs))
          (let ((def (svref fdefs key)))
            (when (and (fdef-active-p def) (= (fdef-key def) key))
              def)))
        (loop for def across fdefs
              when (and (fdef-active-p def) (= (fdef-key def) key))
                return def))))

(defun invoke-function (eng def count)
  (declare (tt-engine eng) (fn-def def) (fixnum count))
  (when (> count 0)
    (when (>= (eng-call-top eng) +max-call-stack+)
      (hint-fail 'tt-hint-limit-exceeded "Call stack overflow"))
    (let ((rec (svref (eng-call-stack eng) (eng-call-top eng))))
      (setf (call-caller-program rec) (eng-cur-program eng)
            (call-return-pc rec)      (eng-pc eng)
            (call-current-end rec)    (eng-end eng)
            (call-loop-count rec)     count
            (call-def-start rec)      (fdef-start def)
            (call-def-program rec)    (fdef-program def))
      (incf (eng-call-top eng))
      (select-program eng (fdef-program def) (fdef-start def) (fdef-end def)))))

(defun run-program (eng prog-id)
  (declare (tt-engine eng) (type program-id prog-id) (optimize (speed 3) (safety 1)))
  (let ((bytes (svref (eng-programs eng) prog-id)))
    (when (plusp (length bytes))
      (setf (eng-call-top eng) 0
            (eng-ops-executed eng) 0)
      (select-program eng prog-id 0 (length bytes))
      (loop while (< (eng-pc eng) (eng-end eng))
            do (when (> (incf (eng-ops-executed eng)) +max-instructions+)
                 (hint-fail 'tt-hint-limit-exceeded "Exceeded maximum instruction count"))
               (let ((op (fetch-u8 eng)))
                 (dispatch-instruction eng op)
                 (loop while (and (>= (eng-pc eng) (eng-end eng))
                                  (plusp (eng-call-top eng)))
                       do (op-endf eng)))))))
