;;;; hint.lisp — CFF and CFF2 hinting
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; A port of Fontations' skrifa src/outline/cff/hint.rs, which follows
;;; FreeType's Adobe CFF hinter (psaux/psblues.c and pshints.c).
;;;
;;; A CFF font does not program its hinting: its Private DICT gives the
;;; alignment zones (the blues) and its charstrings the stem hints.  For a
;;; given pixel size the hinter moves the stem edges and the zones onto the
;;; pixel grid and maps every other y coordinate between them.  Only y is
;;; hinted, as in FreeType.

(in-package #:runes/otf/charstring)

;;; Hinting parameters (HintParams), from a subfont's Private DICT

;; At most 7 blue zones in BlueValues and FamilyBlues, 5 in OtherBlues and
;; FamilyOtherBlues; extra values are ignored (MAX_BLUE_VALUES in read-fonts).
(defconstant +max-blue-values+ 7)

(defstruct (hint-params (:conc-name hp-))
  "Alignment zones and the parameters that shape them, as fixed values.
Each blues vector holds (bottom . top) pairs."
  (blues #() :type simple-vector)
  (family-blues #() :type simple-vector)
  (other-blues #() :type simple-vector)
  (family-other-blues #() :type simple-vector)
  ;; Defaults from the CFF2 specification, Private DICT operators.
  (blue-scale (float->fixed 0.039625d0) :type s32)
  (blue-shift (fixed-from-int 7) :type s32)
  (blue-fuzz (fixed-from-int 1) :type s32)
  (language-group 0 :type fixnum))

(defun make-blues (values)
  "Blues from the delta-decoded VALUES of a blues operator: a vector of
(bottom . top) pairs, at most +MAX-BLUE-VALUES+, an odd last value dropped."
  (coerce (loop for (bottom top) on values by #'cddr
                for i below +max-blue-values+
                while top
                collect (cons bottom top))
          'simple-vector))

(defun private-dict-hint-params (entries)
  "The hinting parameters among the Private DICT ENTRIES."
  (let ((params (make-hint-params)))
    (loop for (op . value) in entries
          do (case op
               (:blue-values (setf (hp-blues params) (make-blues value)))
               (:family-blues (setf (hp-family-blues params) (make-blues value)))
               (:other-blues (setf (hp-other-blues params) (make-blues value)))
               (:family-other-blues (setf (hp-family-other-blues params) (make-blues value)))
               (:blue-scale (setf (hp-blue-scale params) value))
               (:blue-shift (setf (hp-blue-shift params) value))
               (:blue-fuzz (setf (hp-blue-fuzz params) value))
               (:language-group (setf (hp-language-group params) value))))
    params))

;;; Fixed point helpers of the hinter, on raw 16.16 values as font-types'
;;; Fixed: round, abs and twice wrap; half truncates toward zero.

(declaim (inline fixed-round fixed-abs twice half midpoint trunc-1/64))
(defun fixed-round (x) (declare (s32 x)) (s32 (logand (+ x #x8000) (lognot #xFFFF))))
(defun fixed-abs (x) (declare (s32 x)) (s32 (abs x)))
(defun twice (x) (declare (s32 x)) (s32 (* 2 x)))
(defun half (x) (declare (s32 x)) (values (truncate x 2)))
(defun midpoint (a b)
  "The point halfway between A and B, without overflowing."
  (declare (s32 a b))
  (fixed+ a (half (fixed- b a))))
(defun trunc-1/64 (x)
  "X with the bits below 1/64 cleared."
  (declare (s32 x))
  (logand x (lognot #x3FF)))

;;; Blue zones and the hinting state (HintState), for one subfont at one
;;; scale: the zones in character space and their flat edges in device
;;; space.

;; Default ascender and descender of an ideographic em box (ICF), and the
;; most zones FreeType keeps.
(defconstant +icf-top+ (fixed-from-int 880))
(defconstant +icf-bottom+ (fixed-from-int -120))
(defconstant +max-blues+ 7)
(defconstant +max-other-blues+ 5)

(defstruct (blue-zone (:conc-name bz-))
  (bottom-p nil)
  ;; In character space; the flat edge is the zone's baseline-side edge.
  (cs-bottom-edge 0 :type s32)
  (cs-top-edge 0 :type s32)
  (cs-flat-edge 0 :type s32)
  ;; The flat edge in device space, rounded to a pixel.
  (ds-flat-edge 0 :type s32))

(defstruct (hint-state (:conc-name hs-))
  "What the hinter needs for one subfont at one SCALE (charstring units to
pixels, 16.16)."
  (scale 0 :type s32)
  (blue-scale 0 :type s32)
  (blue-shift 0 :type s32)
  (blue-fuzz 0 :type s32)
  (language-group 0 :type fixnum)
  (suppress-overshoot nil)
  (do-em-box-hints nil)
  (boost 0 :type s32)
  ;; Stem darkening is not done, so this stays 0.
  (darken-y 0 :type s32)
  (zones #() :type simple-vector))

(defun em-box-hints-p (params)
  "True when an ideographic font (language group 1) has no usable blue zones
and the em box is to be hinted instead: no BlueValues, or only two zones
outside the ICF em box."
  (let ((blues (hp-blues params)))
    (and (= (hp-language-group params) 1)
         (case (length blues)
           (0 t)
           (2 (destructuring-bind (low high) (coerce blues 'list)
                (and (< (car low) +icf-bottom+) (< (cdr low) +icf-bottom+)
                     (> (car high) +icf-top+) (> (cdr high) +icf-top+))))))))

(defun collect-blue-zones (params darken-y)
  "The blue zones of PARAMS: the first of BlueValues is a bottom zone, the
rest top zones, then every OtherBlues zone is a bottom zone; zones of
negative height are left out.  The second value is the tallest height."
  (let ((zones '()) (max-height 0))
    (flet ((add (bottom top bottom-p)
             (when (>= (fixed- top bottom) 0)
               (setf max-height (max max-height (fixed- top bottom)))
               (push (if bottom-p
                         (make-blue-zone :bottom-p t :cs-bottom-edge bottom :cs-top-edge top
                                         :cs-flat-edge top)
                         ;; A top zone moves up by twice the darkening.
                         (let ((bottom (fixed+ bottom (twice darken-y))))
                           (make-blue-zone :cs-bottom-edge bottom
                                           :cs-top-edge (fixed+ top (twice darken-y))
                                           :cs-flat-edge bottom)))
                     zones))))
      (loop for (bottom . top) across (hp-blues params)
            for i below +max-blues+
            do (add bottom top (null zones)))
      (loop for (bottom . top) across (hp-other-blues params)
            for i below +max-other-blues+
            do (add bottom top t)))
    (values (coerce (nreverse zones) 'simple-vector) max-height)))

(defun nearest-family-edge (flat edges units-per-pixel)
  "The edge among EDGES closest to FLAT and less than a pixel away, the
first one when two are as close, or NIL; the second value is the distance."
  (let ((best nil) (best-diff +fixed-max+))
    (dolist (edge edges)
      (let ((diff (fixed-abs (fixed- flat edge))))
        (when (and (< diff best-diff) (< diff units-per-pixel))
          (setf best edge best-diff diff)
          (when (zerop diff) (return)))))
    (values best best-diff)))

(defun snap-to-family-blues (zones params darken-y units-per-pixel)
  "Move each zone's flat edge to the nearest family zone edge within a
pixel, so that the fonts of a family align."
  (let ((family (coerce (hp-family-blues params) 'list)))
    (loop for zone across zones
          for flat = (bz-cs-flat-edge zone)
          do (if (bz-bottom-p zone)
                 ;; A bottom zone: FamilyOtherBlues' top edges, then the top
                 ;; edge of the first FamilyBlues zone, which is a bottom zone.
                 (multiple-value-bind (edge diff)
                     (nearest-family-edge flat (map 'list #'cdr (hp-family-other-blues params))
                                          units-per-pixel)
                   (when edge (setf (bz-cs-flat-edge zone) edge))
                   (when family
                     (let* ((first-top (cdr (first family)))
                            (first-diff (fixed-abs (fixed- flat first-top))))
                       (when (and (< first-diff diff) (< first-diff units-per-pixel))
                         (setf (bz-cs-flat-edge zone) first-top)))))
                 ;; A top zone: the bottom edges of the other FamilyBlues zones.
                 (let ((edge (nearest-family-edge
                              flat (mapcar (lambda (blue) (fixed+ (car blue) (twice darken-y)))
                                           (rest family))
                              units-per-pixel)))
                   (when edge (setf (bz-cs-flat-edge zone) edge)))))))

(defun make-hint-state-for (params scale)
  "The hinting state for a subfont with hinting PARAMS at SCALE (HintState::new)."
  (let ((state (make-hint-state :scale scale
                                :blue-scale (hp-blue-scale params)
                                :blue-shift (hp-blue-shift params)
                                :blue-fuzz (hp-blue-fuzz params)
                                :language-group (hp-language-group params))))
    (if (em-box-hints-p params)
        ;; Synthetic em box hints are made when the hint map is built.
        (setf (hs-do-em-box-hints state) t)
        (build-blue-zones state params))
    state))

(defun build-blue-zones (state params)
  "Set STATE's zones from PARAMS, its overshoot suppression and boost, and
each zone's flat edge in device space (build_zones)."
  (let ((darken-y (hs-darken-y state))
        (scale (hs-scale state)))
    (multiple-value-bind (zones max-height) (collect-blue-zones params darken-y)
      (snap-to-family-blues zones params darken-y (fixed/ #x10000 scale))
      ;; A zone taller than 1/BlueScale lowers BlueScale.
      (when (and (plusp max-height) (> (hs-blue-scale state) (fixed/ #x10000 max-height)))
        (setf (hs-blue-scale state) (fixed/ #x10000 max-height)))
      ;; Below BlueScale, overshoots are suppressed and the zones boosted;
      ;; the boost stays under half a pixel, or the baseline could go negative.
      (when (< scale (hs-blue-scale state))
        (setf (hs-suppress-overshoot state) t
              (hs-boost state) (min (fixed- (float->fixed 0.6d0)
                                            (fixed-mul-div (float->fixed 0.6d0) scale (hs-blue-scale state)))
                                    #x7FFF)))
      (unless (zerop darken-y)
        (setf (hs-boost state) 0))
      (loop with boost = (hs-boost state)
            for zone across zones
            do (setf (bz-ds-flat-edge zone)
                     (fixed-round (fixed+ (fixed* (bz-cs-flat-edge zone) scale)
                                          (if (bz-bottom-p zone) (fixed-neg boost) boost)))))
      (setf (hs-zones state) zones))))
