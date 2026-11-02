;;;; cff-outlines.lisp — Glyph outlines from CFF and CFF2 charstrings
;;;; Copyright (c) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

;;; CFF outlines are cubic. CONTOURS keeps its TrueType form, on- and
;;; off-curve points describing quadratic curves, so each cubic is
;;; approximated by quadratics within *CUBIC-TOLERANCE* font units, and
;;; existing callers draw CFF glyphs unchanged. CUBIC-CONTOURS gives the
;;; exact outline of any glyph.

(in-package #:zpb-ttf)

(defparameter *cubic-tolerance* 1/4
  "Largest distance, in font units, between a CFF cubic curve and the
quadratic curves CONTOURS approximates it with.")

;;; The CFF font of a loader

(defun cff-table-info (loader)
  (or (table-info "CFF2" loader) (table-info "CFF " loader)))

(defun cff-outlines-p (loader)
  "True if LOADER's glyph outlines come from CFF or CFF2 charstrings."
  (and (null (table-info "glyf" loader)) (cff-table-info loader) t))

(defun loader-cff-font (loader)
  (or (%cff-font loader)
      (setf (%cff-font loader)
            (let ((info (cff-table-info loader)))
              (runes/otf/charstring:open-cff-font
               (runes/otf:font-data-slice (font-data loader) (offset info) (size info))
               :upem (units/em loader))))))

(defun loader-coords (loader)
  (or (normalized-coordinates loader) #()))

(defun loader-cff-subfont (loader index)
  "Subfont INDEX at the loader's instance, parsed once per instance."
  (or (gethash index (cff-subfonts loader))
      (setf (gethash index (cff-subfonts loader))
            (runes/otf/charstring:cff-subfont (loader-cff-font loader) index (loader-coords loader)))))

;;; Collecting the exact outline

(defun font-units (fixed)
  "A 16.16 value in font units: an integer when it is one, else a float."
  (let ((r (/ fixed 65536)))
    (if (integerp r) r (float r 1.0))))

(defclass outline-sink ()
  ((contours :initform '() :accessor outline-contours)
   (current :initform nil :accessor outline-current))
  (:documentation "Collects contours as vectors of (:move x y), (:line x y)
and (:curve cx0 cy0 cx1 cy1 x y) commands in font units."))

(defun outline-add (sink command)
  (vector-push-extend command (outline-current sink)))

(defmethod runes/otf/charstring:sink-move-to ((s outline-sink) x y)
  (setf (outline-current s) (make-array 8 :adjustable t :fill-pointer 0))
  (push (outline-current s) (outline-contours s))
  (outline-add s (list :move (font-units x) (font-units y))))

(defmethod runes/otf/charstring:sink-line-to ((s outline-sink) x y)
  (outline-add s (list :line (font-units x) (font-units y))))

(defmethod runes/otf/charstring:sink-curve-to ((s outline-sink) cx0 cy0 cx1 cy1 x y)
  (outline-add s (list :curve (font-units cx0) (font-units cy0) (font-units cx1) (font-units cy1)
                       (font-units x) (font-units y))))

(defmethod runes/otf/charstring:sink-close ((s outline-sink)))

(defun cff-glyph-outline (loader gid)
  "The exact outline of glyph GID of LOADER's CFF font, at the loader's instance."
  (let* ((font (loader-cff-font loader))
         (index (or (runes/otf/charstring:cff-subfont-index font gid)
                    (error 'unsupported-value :description "Glyph not covered by FDSelect"
                                              :actual-value gid :expected-values '("a covered glyph"))))
         (sink (make-instance 'outline-sink)))
    (runes/otf/charstring:cff-draw-glyph font (loader-cff-subfont loader index) gid sink
                                       (loader-coords loader))
    (map 'vector #'copy-seq (reverse (outline-contours sink)))))

;;; Quadratic approximation

(defun lerp (a b u) (+ a (* u (- b a))))

(defun split-cubic (p0 p1 p2 p3 u)
  "The parts of cubic P0..P3 (each (x . y)) before and after parameter U."
  (flet ((mid (a b) (cons (lerp (car a) (car b) u) (lerp (cdr a) (cdr b) u))))
    (let* ((a (mid p0 p1)) (b (mid p1 p2)) (c (mid p2 p3))
           (d (mid a b)) (e (mid b c)) (f (mid d e)))
      (values (list p0 a d f) (list f e c p3)))))

(defun cubic->quadratics (p0 p1 p2 p3 tolerance)
  "List of (control . end) quadratic pieces approximating cubic P0..P3.
One quadratic with control (3(P1+P2) - P0 - P3)/4 is within
sqrt(3)/36 * |P3 - 3P2 + 3P1 - P0| of the cubic; splitting into N parts
divides that by N^3."
  (let* ((dx (+ (- (car p3) (* 3 (car p2))) (* 3 (car p1)) (- (car p0))))
         (dy (+ (- (cdr p3) (* 3 (cdr p2))) (* 3 (cdr p1)) (- (cdr p0))))
         (err (* (/ (sqrt 3d0) 36) (sqrt (float (+ (* dx dx) (* dy dy)) 1d0))))
         (n (max 1 (ceiling (expt (/ err tolerance) 1/3))))
         (pieces '())
         (rest (list p0 p1 p2 p3)))
    (loop for k from n downto 1
          do (multiple-value-bind (piece remaining)
                 (if (= k 1) (values rest nil) (apply #'split-cubic (append rest (list (/ 1 k)))))
               (destructuring-bind (q0 q1 q2 q3) piece
                 (push (cons (cons (/ (- (* 3 (+ (car q1) (car q2))) (car q0) (car q3)) 4)
                                   (/ (- (* 3 (+ (cdr q1) (cdr q2))) (cdr q0) (cdr q3)) 4))
                             q3)
                       pieces))
               (setf rest remaining)))
    (nreverse pieces)))

(defun units (x)
  "X as a contour coordinate: an integer when it is one, else a float."
  (if (integerp x) x (float x 1.0)))

;;; The same approximation without rational arithmetic, which spent most of
;;; its time reducing fractions.  The points come from 16.16 charstring
;;; values, so each is an integer number of 1/65536 font units: the curve is
;;; computed in those integers, and each piece of the subdivision into N
;;; parts directly from the cubic's blossom.  The result is exactly that of
;;; CUBIC->QUADRATICS, which remains for points that are not 16.16 values.

(defun fixed-units (v)
  "V (font units, an integer or a float) times 65536, or NIL if that is not
an integer."
  (if (integerp v)
      (* v 65536)
      (multiple-value-bind (mantissa exponent sign) (integer-decode-float v)
        (let ((shift (+ exponent 16)))
          (when (>= shift 0)
            (* sign (ash mantissa shift)))))))

(defun ratio-units (numerator denominator)
  "NUMERATOR / DENOMINATOR as a contour coordinate, as UNITS gives it: an
integer, or the float nearest the exact quotient."
  (declare (integer numerator) (type (integer 1) denominator))
  (multiple-value-bind (q r) (floor numerator denominator)
    (cond ((zerop r) q)
          ;; Both exact as doubles: the double quotient is correctly
          ;; rounded, and rounding it again to a single float gives the
          ;; float nearest the exact quotient, unless it lies exactly
          ;; halfway between two single floats.
          ((and (< (abs numerator) (expt 2 53)) (< denominator (expt 2 53)))
           (let ((d (/ (float numerator 1d0) (float denominator 1d0))))
             (if (= (ldb (byte 29 0) (integer-decode-float d)) (ash 1 28))
                 (float (/ numerator denominator) 1.0)
                 (coerce d 'single-float))))
          (t (float (/ numerator denominator) 1.0)))))

(defun cubic->quadratics-fixed (x0 y0 x1 y1 x2 y2 x3 y3 tolerance)
  "As CUBIC->QUADRATICS, for points given times 65536 as integers: a list of
((control-x . control-y) . (end-x . end-y)) in contour coordinates."
  (declare (integer x0 y0 x1 y1 x2 y2 x3 y3))
  (let* ((dx (+ (- x3 (* 3 x2)) (* 3 x1) (- x0)))
         (dy (+ (- y3 (* 3 y2)) (* 3 y1) (- y0)))
         ;; The same double as for the unscaled points: the scale is 2^32.
         (err (* (/ (sqrt 3d0) 36)
                 (sqrt (* (float (+ (* dx dx) (* dy dy)) 1d0) (expt 2d0 -32)))))
         (n (max 1 (ceiling (expt (/ err tolerance) 1/3))))
         (n3 (* n n n))
         (pieces '()))
    (flet ((blossom (u v w p0 p1 p2 p3)
             ;; n^3 times the blossom of the cubic at u/n, v/n, w/n.
             (let ((a (- n u)) (b (- n v)) (c (- n w)))
               (+ (* a b c p0)
                  (* (+ (* a b w) (* a v c) (* u b c)) p1)
                  (* (+ (* a v w) (* u b w) (* u v c)) p2)
                  (* u v w p3))))
           (fixnum-blossom (u v w p0 p1 p2 p3)
             ;; The same in fixnums, when the caller has checked they fit.
             (declare (type (integer 0 64) u v w) (type (signed-byte 34) p0 p1 p2 p3))
             (let ((a (- n u)) (b (- n v)) (c (- n w)))
               (declare (type (integer 0 64) a b c))
               (+ (* a b c p0)
                  (* (+ (* a b w) (* a v c) (* u b c)) p1)
                  (* (+ (* a v w) (* u b w) (* u v c)) p2)
                  (* u v w p3)))))
      (declare (inline fixnum-blossom))
      (macrolet ((pieces-with (blossom)
                   `(dotimes (j n)
                      (let* ((k (1+ j))
                             (qx0 (,blossom j j j x0 x1 x2 x3)) (qy0 (,blossom j j j y0 y1 y2 y3))
                             (qx1 (,blossom j j k x0 x1 x2 x3)) (qy1 (,blossom j j k y0 y1 y2 y3))
                             (qx2 (,blossom j k k x0 x1 x2 x3)) (qy2 (,blossom j k k y0 y1 y2 y3))
                             (qx3 (,blossom k k k x0 x1 x2 x3)) (qy3 (,blossom k k k y0 y1 y2 y3))
                             (end-den (* n3 65536))
                             (control-den (* 4 end-den)))
                        (push (cons (cons (ratio-units (- (* 3 (+ qx1 qx2)) qx0 qx3) control-den)
                                          (ratio-units (- (* 3 (+ qy1 qy2)) qy0 qy3) control-den))
                                    (cons (ratio-units qx3 end-den) (ratio-units qy3 end-den)))
                              pieces)))))
        ;; |coordinate| < 2^17 font units and N <= 64: every product and sum
        ;; stays below 2^60, a fixnum.
        (if (and (<= n 64)
                 (every (lambda (p) (typep p '(signed-byte 34))) (list x0 y0 x1 y1 x2 y2 x3 y3)))
            (pieces-with fixnum-blossom)
            (pieces-with blossom))))
    (nreverse pieces)))

(defun curve-pieces (current cx0 cy0 cx1 cy1 x y)
  "The quadratic pieces of the curve from CURRENT, as CUBIC->QUADRATICS."
  (let ((fx0 (fixed-units (car current))) (fy0 (fixed-units (cdr current)))
        (fx1 (fixed-units cx0)) (fy1 (fixed-units cy0))
        (fx2 (fixed-units cx1)) (fy2 (fixed-units cy1))
        (fx3 (fixed-units x)) (fy3 (fixed-units y)))
    (if (and fx0 fy0 fx1 fy1 fx2 fy2 fx3 fy3)
        (cubic->quadratics-fixed fx0 fy0 fx1 fy1 fx2 fy2 fx3 fy3 *cubic-tolerance*)
        (cubic->quadratics (cons (rational (car current)) (rational (cdr current)))
                           (cons (rational cx0) (rational cy0))
                           (cons (rational cx1) (rational cy1))
                           (cons (rational x) (rational y))
                           *cubic-tolerance*))))

(defun outline->contours (outline)
  "TrueType-style contours of control points approximating OUTLINE."
  (map 'vector
       (lambda (commands)
         (let ((points (make-array (length commands) :adjustable t :fill-pointer 0))
               (current nil)
               (start nil))
           (flet ((add (x y on-curve-p)
                    (vector-push-extend (make-control-point (units x) (units y) on-curve-p) points)))
             (loop for command across commands
                   do (destructuring-bind (kind &rest args) command
                        (ecase kind
                          (:move (setf current (cons (first args) (second args)) start current)
                           (add (car current) (cdr current) t))
                          (:line (setf current (cons (first args) (second args)))
                           (add (car current) (cdr current) t))
                          (:curve
                           (destructuring-bind (cx0 cy0 cx1 cy1 x y) args
                             (loop for (control . end)
                                     in (curve-pieces current cx0 cy0 cx1 cy1 x y)
                                   do (add (car control) (cdr control) nil)
                                      (add (car end) (cdr end) t))
                             (setf current (cons x y)))))))
             ;; A contour is closed implicitly; drop a final point that
             ;; repeats the first.
             (let ((n (length points)))
               (when (and (> n 1) start
                          (on-curve-p (aref points (1- n)))
                          (= (x (aref points (1- n))) (car start))
                          (= (y (aref points (1- n))) (cdr start)))
                 (vector-pop points))))
           (coerce points 'simple-vector)))
       outline))

(defun contours-bounding-box (contours)
  "Bounds of all points of CONTOURS, on- and off-curve, as glyf records them."
  (let (xmin ymin xmax ymax)
    (loop for contour across contours
          do (loop for p across contour
                   do (setf xmin (if xmin (min xmin (x p)) (x p))
                            ymin (if ymin (min ymin (y p)) (y p))
                            xmax (if xmax (max xmax (x p)) (x p))
                            ymax (if ymax (max ymax (y p)) (y p)))))
    (if xmin
        (vector (floor xmin) (floor ymin) (ceiling xmax) (ceiling ymax))
        (empty-bounding-box))))

(defun initialize-cff-outline (glyph)
  "Set the exact outline of a CFF glyph."
  (setf (%cubic-contours glyph)
        (cff-glyph-outline (font-loader glyph) (font-index glyph))))

(defun initialize-cff-glyph (glyph)
  "Set the exact outline, contours and bounding box of a CFF glyph.  The
contours are approximated only now: CUBIC-CONTOURS does not need them."
  (unless (slot-boundp glyph 'cubic-contours)
    (initialize-cff-outline glyph))
  (let ((contours (outline->contours (%cubic-contours glyph))))
    (setf (contours glyph) contours
          (bounding-box glyph) (contours-bounding-box contours))))

;;; The exact outline of any glyph

(defgeneric cubic-contours (glyph)
  (:documentation "The exact outline of GLYPH: a vector of contours, each a
vector of (:move x y), (:line x y) and (:curve cx0 cy0 cx1 cy1 x y)
commands in font units, closed implicitly. TrueType quadratic curves are
given as the cubic curves that trace them exactly."))

(defmethod cubic-contours ((glyph glyph))
  (cond
    ((slot-boundp glyph 'cubic-contours) (%cubic-contours glyph))
    ((cff-outlines-p (font-loader glyph))
     (initialize-cff-outline glyph))
    (t
     (setf (%cubic-contours glyph)
           (map 'vector
                (lambda (contour)
                  (let ((commands (make-array 8 :adjustable t :fill-pointer 0))
                        (moved nil))
                    (do-contour-segments (p0 p1 p2) contour
                      (unless moved
                        (vector-push-extend (list :move (x p0) (y p0)) commands)
                        (setf moved t))
                      (if p1
                          ;; Degree elevation: the cubic with controls two
                          ;; thirds of the way to P1 is the same curve.
                          (vector-push-extend
                           (list :curve
                                 (units (+ (x p0) (* 2/3 (- (x p1) (x p0)))))
                                 (units (+ (y p0) (* 2/3 (- (y p1) (y p0)))))
                                 (units (+ (x p2) (* 2/3 (- (x p1) (x p2)))))
                                 (units (+ (y p2) (* 2/3 (- (y p1) (y p2)))))
                                 (x p2) (y p2))
                           commands)
                          (vector-push-extend (list :line (x p2) (y p2)) commands)))
                    (copy-seq commands)))
                (contours glyph))))))
