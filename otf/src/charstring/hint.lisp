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

;;; Stem hints and hint edges (StemHint and Hint)

(defstruct (stem-hint (:conc-name stem-))
  "A stem hint from a charstring: its edges in character space and, once
used, in device space."
  (used-p nil)
  (min 0 :type s32)
  (max 0 :type s32)
  (ds-min 0 :type s32)
  (ds-max 0 :type s32))

;; Hint edge flags.  A ghost hint aligns a single edge, as the top and
;; bottom of a sans serif I; a pair is one edge of a stem.
(defconstant +ghost-bottom+ #x1)
(defconstant +ghost-top+ #x2)
(defconstant +pair-bottom+ #x4)
(defconstant +pair-top+ #x8)
(defconstant +locked+ #x10)
(defconstant +synthetic+ #x20)

(defstruct (hint (:copier copy-hint))
  "One edge of a stem hint, in character and device space.  No flags
means an invalid edge."
  (flags 0 :type (unsigned-byte 8))
  ;; Index of the stem hint it comes from, unless synthetic.
  (index 0 :type (unsigned-byte 8))
  (cs-coord 0 :type s32)
  (ds-coord 0 :type s32)
  (scale 0 :type s32))

(declaim (inline hint-flag-p hint-valid-p hint-bottom-p hint-top-p hint-pair-p
                 hint-pair-top-p hint-locked-p hint-synthetic-p lock-hint))
(defun hint-flag-p (hint flags) (logtest (hint-flags hint) flags))
(defun hint-valid-p (hint) (/= (hint-flags hint) 0))
(defun hint-bottom-p (hint) (hint-flag-p hint (logior +ghost-bottom+ +pair-bottom+)))
(defun hint-top-p (hint) (hint-flag-p hint (logior +ghost-top+ +pair-top+)))
(defun hint-pair-p (hint) (hint-flag-p hint (logior +pair-bottom+ +pair-top+)))
(defun hint-pair-top-p (hint) (hint-flag-p hint +pair-top+))
(defun hint-locked-p (hint) (hint-flag-p hint +locked+))
(defun hint-synthetic-p (hint) (hint-flag-p hint +synthetic+))
(defun lock-hint (hint) (setf (hint-flags hint) (logior (hint-flags hint) +locked+)))

;; Ghost hints are stems of width -21 (bottom) and -20 (top).
(defconstant +ghost-bottom-width+ (fixed-from-int -21))
(defconstant +ghost-top-width+ (fixed-from-int -20))

(defun stem-edge (stem bottom-p)
  "The character space coordinate and flags of STEM's bottom edge, or top
edge unless BOTTOM-P; flags 0 when STEM is a ghost hint of the other edge.
A stem of negative width is an inverted pair, its edges swapped."
  (let ((width (fixed- (stem-max stem) (stem-min stem))))
    (cond ((= width +ghost-bottom-width+)
           (if bottom-p (values (stem-max stem) +ghost-bottom+) (values 0 0)))
          ((= width +ghost-top-width+)
           (if bottom-p (values 0 0) (values (stem-min stem) +ghost-top+)))
          ((minusp width)
           (if bottom-p
               (values (stem-max stem) +pair-bottom+)
               (values (stem-min stem) +pair-top+)))
          (bottom-p (values (stem-min stem) +pair-bottom+))
          (t (values (stem-max stem) +pair-top+)))))

(defun setup-hint (hint stem index origin scale darken-y bottom-p)
  "Make HINT STEM's bottom edge, or top edge unless BOTTOM-P (Hint::setup).
A stem already placed keeps its device position and is locked."
  (multiple-value-bind (cs-coord flags) (stem-edge stem bottom-p)
    (setf (hint-flags hint) flags
          (hint-cs-coord hint) cs-coord)
    ;; A top edge moves up by twice the darkening.
    (when (hint-top-p hint)
      (setf (hint-cs-coord hint) (fixed+ (hint-cs-coord hint) (twice darken-y))))
    (setf (hint-cs-coord hint) (fixed+ (hint-cs-coord hint) origin)
          (hint-scale hint) scale
          (hint-index hint) index)
    (cond ((and (/= flags 0) (stem-used-p stem))
           (setf (hint-ds-coord hint) (if (hint-top-p hint) (stem-ds-max stem) (stem-ds-min stem)))
           (lock-hint hint))
          (t (setf (hint-ds-coord hint) (fixed* (hint-cs-coord hint) scale))))
    hint))

;;; Capturing stem edges by blue zones

(defun zone-captures-p (zone edge fuzz)
  "True when ZONE, widened by FUZZ, holds EDGE's character space coordinate."
  (<= (fixed- (bz-cs-bottom-edge zone) fuzz)
      (hint-cs-coord edge)
      (fixed+ (bz-cs-top-edge zone) fuzz)))

(defun captured-position (state zone edge overshoot)
  "Where a ZONE puts a captured EDGE in device space.  OVERSHOOT is how far
EDGE overshoots ZONE's flat edge in character space; an overshoot of at
least BlueShift is kept at a pixel or more."
  (let ((rounded (fixed-round (hint-ds-coord edge)))
        (flat (bz-ds-flat-edge zone)))
    (cond ((hs-suppress-overshoot state) flat)
          ((< overshoot (hs-blue-shift state)) rounded)
          ((bz-bottom-p zone) (min rounded (fixed- flat #x10000)))
          (t (max rounded (fixed+ flat #x10000))))))

(defun capture-adjustment (state bottom-edge top-edge)
  "How far the first zone that holds BOTTOM-EDGE or TOP-EDGE moves it, or
NIL when no zone does."
  (loop with fuzz = (hs-blue-fuzz state)
        for zone across (hs-zones state)
        do (cond ((and (bz-bottom-p zone) (hint-bottom-p bottom-edge)
                       (zone-captures-p zone bottom-edge fuzz))
                  (return (fixed- (captured-position
                                   state zone bottom-edge
                                   (fixed- (bz-cs-top-edge zone) (hint-cs-coord bottom-edge)))
                                  (hint-ds-coord bottom-edge))))
                 ((and (not (bz-bottom-p zone)) (hint-top-p top-edge)
                       (zone-captures-p zone top-edge fuzz))
                  (return (fixed- (captured-position
                                   state zone top-edge
                                   (fixed- (hint-cs-coord top-edge) (bz-cs-bottom-edge zone)))
                                  (hint-ds-coord top-edge)))))))

(defun capture (state bottom-edge top-edge)
  "Align a stem's edges to a blue zone that holds one of them: both valid
edges move by the same amount and are locked.  True when captured."
  (let ((adjustment (capture-adjustment state bottom-edge top-edge)))
    (when adjustment
      (dolist (edge (list bottom-edge top-edge))
        (when (hint-valid-p edge)
          (setf (hint-ds-coord edge) (fixed+ (hint-ds-coord edge) adjustment))
          (lock-hint edge)))
      t)))

;;; Hint masks (HintMask): which stem hints are active, bit I for the Ith
;;; stem hint of the charstring

(defconstant +max-hints+ 96)

(deftype hint-mask () `(simple-bit-vector ,+max-hints+))

(defun all-hints-mask ()
  (make-array +max-hints+ :element-type 'bit :initial-element 1))

(defun bytes-hint-mask (bytes)
  "The hint mask of a hintmask or cntrmask operator's BYTES, the first
stem hint in the first byte's most significant bit; NIL when too long."
  (when (<= (length bytes) (/ +max-hints+ 8))
    (let ((mask (make-array +max-hints+ :element-type 'bit :initial-element 0)))
      (loop for byte across bytes
            for base from 0 by 8
            do (loop for bit below 8
                     do (setf (sbit mask (+ base bit)) (ldb (byte 1 (- 7 bit)) byte))))
      mask)))

;;; The hint map (HintMap): the stem edges of the active hints, sorted by
;;; character space coordinate, each with its device position and the
;;; scale up to the next edge.  It maps any y coordinate to device space.

;; The least room left between the edges of two stems, half a pixel.
(defconstant +min-counter+ #x8000)
(defconstant +epsilon+ 1)

(defstruct (hint-map (:constructor %make-hint-map))
  (edges #() :type simple-vector)
  (length 0 :type fixnum)
  (valid-p nil)
  (scale 0 :type s32))

(defun make-hint-map-for (scale)
  (%make-hint-map :edges (map-into (make-array +max-hints+) #'make-hint) :scale scale))

(defun clear-hint-map (map)
  (setf (hint-map-length map) 0
        (hint-map-valid-p map) nil))

(defun hint-map-edge (map i) (svref (hint-map-edges map) i))

(defun hint-map-transform (map coord)
  "COORD mapped to device space: scaled from the highest edge at or below
it with that edge's scale, or uniformly below the first edge."
  (let ((length (hint-map-length map)))
    (if (zerop length)
        (fixed* coord (hint-map-scale map))
        (let* ((i (1- (or (position-if (lambda (edge) (< coord (hint-cs-coord edge)))
                                       (hint-map-edges map) :start 1 :end length)
                          length)))
               (edge (hint-map-edge map i))
               (scale (if (and (zerop i) (< coord (hint-cs-coord edge)))
                          (hint-map-scale map)
                          (hint-scale edge))))
          (fixed+ (fixed* (fixed- coord (hint-cs-coord edge)) scale)
                  (hint-ds-coord edge))))))

;;; Inserting the edges of a hint

(defun insertion-index (map coord)
  "The index of the first edge of MAP at or above COORD."
  (or (position-if (lambda (edge) (>= (hint-cs-coord edge) coord))
                   (hint-map-edges map) :end (hint-map-length map))
      (hint-map-length map)))

(defun overlaps-in-character-space-p (map index first second pair-p)
  "True when edges inserted at INDEX would land on an existing edge,
straddle one, or split a pair."
  (and (< index (hint-map-length map))
       (let ((current (hint-map-edge map index)))
         (or (= (hint-cs-coord current) (hint-cs-coord first))
             (and pair-p (<= (hint-cs-coord current) (hint-cs-coord second)))
             (hint-pair-top-p current)))))

(defun overlaps-in-device-space-p (map index first second pair-p)
  "True when edges inserted at INDEX would cross their neighbours in device
space."
  (or (and (plusp index)
           (< (hint-ds-coord first) (hint-ds-coord (hint-map-edge map (1- index)))))
      (and (< index (hint-map-length map))
           (let ((next (hint-ds-coord (hint-map-edge map index))))
             (or (and pair-p (> (hint-ds-coord second) next))
                 (> (hint-ds-coord first) next))))))

(defun place-with-initial-map (map initial first second pair-p)
  "Set the device positions of unlocked edges from the INITIAL map.  A
pair keeps its width: its middle goes through INITIAL and its edges are
half a width away at MAP's scale."
  (unless (or (hint-locked-p first) (null initial))
    (if pair-p
        (let ((middle (hint-map-transform
                       initial (midpoint (hint-cs-coord first) (hint-cs-coord second))))
              (half-width (fixed* (half (fixed- (hint-cs-coord second) (hint-cs-coord first)))
                                  (hint-map-scale map))))
          (setf (hint-ds-coord first) (fixed- middle half-width)
                (hint-ds-coord second) (fixed+ middle half-width)))
        (setf (hint-ds-coord first) (hint-map-transform initial (hint-cs-coord first))))))

(defun insert-edges-at (map index edges)
  "Put EDGES into MAP at INDEX, moving the ones above up."
  (let ((all (hint-map-edges map))
        (length (hint-map-length map))
        (count (length edges)))
    (replace all all :start1 (+ index count) :start2 index :end2 length)
    (loop for edge in edges
          for i from index
          do (setf (svref all i) edge))
    (setf (hint-map-length map) (+ length count))))

(defun insert-hint (map bottom top initial)
  "Insert the valid edges among BOTTOM and TOP into MAP, keeping it sorted,
unless they overlap edges already there; unlocked edges are placed with
the INITIAL map when there is one (HintMap::insert)."
  (let* ((pair-p (and (hint-valid-p bottom) (hint-valid-p top)))
         (first (copy-hint (if (hint-valid-p bottom) bottom top)))
         (second (copy-hint top))
         (edges (if pair-p (list first second) (list first))))
    ;; A pair upside down, or one that does not fit, is ignored, as FreeType does.
    (unless (or (and pair-p (< (hint-cs-coord top) (hint-cs-coord bottom)))
                (> (+ (hint-map-length map) (length edges)) +max-hints+))
      (let ((index (insertion-index map (hint-cs-coord first))))
        (unless (overlaps-in-character-space-p map index first second pair-p)
          (place-with-initial-map map initial first second pair-p)
          (unless (overlaps-in-device-space-p map index first second pair-p)
            (insert-edges-at map index edges)))))))

;;; Adjusting edges to the pixel grid

(declaim (inline fixed-fract))
(defun fixed-fract (x) (declare (s32 x)) (logand x #xFFFF))

(defun smallest-moves (bottom-ds top-ds)
  "The smallest moves up and down, the down one negative, that put one of
the edges at BOTTOM-DS and TOP-DS on a pixel boundary."
  (flet ((up (ds) (if (zerop (fixed-fract ds)) 0 (- #x10000 (fixed-fract ds)))))
    (values (min (up bottom-ds) (up top-ds))
            (max (- (fixed-fract bottom-ds)) (- (fixed-fract top-ds))))))

(defun edge-adjustment (map i j)
  "How far to move the unlocked hint with edges I and J (the same edge
for a single one) onto the pixel grid without crowding its neighbours.
The second value is true when the move is not the best, and the third
is the best move up."
  (let ((edges (hint-map-edges map))
        (last (1- (hint-map-length map))))
    (multiple-value-bind (move-up move-down)
        (smallest-moves (hint-ds-coord (svref edges i)) (hint-ds-coord (svref edges j)))
      (let ((room-up (or (>= j last)
                         (>= (hint-ds-coord (svref edges (1+ j)))
                             (+ (hint-ds-coord (svref edges j)) move-up +min-counter+))))
            (room-down (or (zerop i)
                           (<= (hint-ds-coord (svref edges (1- i)))
                               (- (+ (hint-ds-coord (svref edges i)) move-down) +min-counter+)))))
        (cond ((and room-up room-down)
               (values (if (< (- move-down) move-up) move-down move-up) nil move-up))
              (room-up (values move-up nil move-up))
              (room-down (values move-down (< move-up (- move-down)) move-up))
              (t (values 0 t move-up)))))))

(defun rescale-below (map i)
  "Set the scale of edge I-1, I above 0, so that it reaches edge I."
  (let ((a (hint-map-edge map i))
        (b (hint-map-edge map (1- i))))
    (when (/= (hint-cs-coord a) (hint-cs-coord b))
      (setf (hint-scale b) (fixed/ (fixed- (hint-ds-coord a) (hint-ds-coord b))
                                   (fixed- (hint-cs-coord a) (hint-cs-coord b)))))))

(defun adjust-hint-map (map)
  "Move every unlocked hint so that one of its edges is on a pixel
boundary, from the bottom up, and set the scales between the edges.
Hints that could not take their best move are moved up in a second pass
if the first left room (HintMap::adjust)."
  (let ((edges (hint-map-edges map))
        (length (hint-map-length map))
        (saved '()))
    (loop with i = 0
          while (< i length)
          do (let* ((pair-p (hint-pair-p (svref edges i)))
                    (j (if pair-p (1+ i) i)))
               (unless (hint-locked-p (svref edges i))
                 (multiple-value-bind (adjustment save-p move-up) (edge-adjustment map i j)
                   ;; Only an unlocked edge above can make room later.
                   (when (and save-p (< j (1- length)) (not (hint-locked-p (svref edges (1+ j)))))
                     (push (cons j (- move-up adjustment)) saved))
                   (dolist (k (if pair-p (list i j) (list i)))
                     (setf (hint-ds-coord (svref edges k))
                           (fixed+ (hint-ds-coord (svref edges k)) adjustment)))))
               (when (plusp i) (rescale-below map i))
               (when pair-p (rescale-below map j))
               (setf i (1+ j))))
    ;; SAVED is last first, the order of FreeType's second pass.
    (loop for (j . adjustment) in saved
          for edge = (svref edges j)
          when (>= (hint-ds-coord (svref edges (1+ j)))
                   (+ (hint-ds-coord edge) adjustment +min-counter+))
            do (setf (hint-ds-coord edge) (fixed+ (hint-ds-coord edge) adjustment))
               (when (hint-pair-p edge)
                 (let ((below (svref edges (1- j))))
                   (setf (hint-ds-coord below) (fixed+ (hint-ds-coord below) adjustment)))))))

;;; Building the hint map

(defun insert-em-box-hints (map state initial)
  "Insert locked ghost hints just outside the ideographic em box."
  (let ((scale (hs-scale state)))
    (flet ((ghost (cs-coord ds-offset flags)
             (make-hint :cs-coord cs-coord
                        :ds-coord (fixed+ (fixed-round (fixed* cs-coord scale)) ds-offset)
                        :scale scale
                        :flags (logior flags +locked+ +synthetic+))))
      (insert-hint map (ghost (fixed- +icf-bottom+ +epsilon+) (- +min-counter+) +ghost-bottom+)
                   (make-hint) initial)
      (insert-hint map (make-hint)
                   (ghost (fixed+ (fixed+ +icf-top+ +epsilon+) (twice (hs-darken-y state)))
                          +min-counter+ +ghost-top+)
                   initial))))

(defun stem-edges (stem index origin scale)
  "The bottom and top hint edges of STEM, the INDEXth stem hint."
  (values (setup-hint (make-hint) stem index origin scale 0 t)
          (setup-hint (make-hint) stem index origin scale 0 nil)))

(defun insert-locked-stems (map state mask stems origin initial)
  "Insert the stems of MASK that are locked or captured by a blue zone,
clearing them from MASK."
  (loop for stem across stems
        for i below +max-hints+
        when (= (sbit mask i) 1)
          do (multiple-value-bind (bottom top) (stem-edges stem i origin (hs-scale state))
               (when (or (hint-locked-p bottom) (hint-locked-p top) (capture state bottom top))
                 (insert-hint map bottom top initial)
                 (setf (sbit mask i) 0)))))

(defun insert-other-stems (map state mask stems origin initial)
  "Insert the stems still in MASK."
  (loop for stem across stems
        for i below +max-hints+
        when (= (sbit mask i) 1)
          do (multiple-value-bind (bottom top) (stem-edges stem i origin (hs-scale state))
               (insert-hint map bottom top initial))))

(defun insert-baseline-lock (map scale)
  "Lock y = 0 when no edge covers it, for glyphs without a baseline hint."
  (let ((length (hint-map-length map)))
    (when (or (zerop length)
              (plusp (hint-cs-coord (hint-map-edge map 0)))
              (minusp (hint-cs-coord (hint-map-edge map (1- length)))))
      (insert-hint map (make-hint :flags (logior +ghost-bottom+ +locked+ +synthetic+) :scale scale)
                   (make-hint) nil))))

(defun save-stem-positions (map stems)
  "Record in STEMS where MAP put their edges, so that later maps keep them."
  (loop for i below (hint-map-length map)
        for edge = (hint-map-edge map i)
        unless (hint-synthetic-p edge)
          do (let ((stem (aref stems (hint-index edge))))
               (if (hint-top-p edge)
                   (setf (stem-ds-max stem) (hint-ds-coord edge))
                   (setf (stem-ds-min stem) (hint-ds-coord edge)))
               (setf (stem-used-p stem) t))))

(defun build-hint-map (map state mask initial stems origin &optional initial-p)
  "Fill MAP from the STEMS active in MASK (all when NIL), placed through the
INITIAL map, which is built first when not yet valid.  The initial map,
INITIAL-P, holds every hint (HintMap::build)."
  (when (and (not initial-p) initial (not (hint-map-valid-p initial)))
    (build-hint-map initial state (all-hints-mask) nil stems origin t))
  (clear-hint-map map)
  (let ((mask (copy-seq (or mask (all-hints-mask))))
        (placed-by (if initial-p nil initial)))
    (when (hs-do-em-box-hints state)
      (insert-em-box-hints map state initial))
    (insert-locked-stems map state mask stems origin placed-by)
    (if initial-p
        (insert-baseline-lock map (hs-scale state))
        (insert-other-stems map state mask stems origin initial))
    (adjust-hint-map map)
    (unless initial-p
      (save-stem-positions map stems))
    (setf (hint-map-valid-p map) t)
    map))

;;; The hinting sink (HintingSink): takes the stem hints, masks and
;;; outline of a charstring and passes the outline to INNER, x scaled and
;;; y hinted, in pixels (16.16 kept to FreeType's 26.6 precision).

(defstruct (hinting-sink (:constructor %make-hinting-sink))
  (state nil :type hint-state)
  inner
  (stems (make-array 0 :adjustable t :fill-pointer t) :type vector)
  (mask (all-hints-mask) :type hint-mask)
  (initial-map nil :type hint-map)
  (map nil :type hint-map)
  ;; The last move in character space, and the last line in character
  ;; and device space, held back until the next command.
  (start-point nil)
  (pending-line nil))

(defun make-hinting-sink (state inner)
  (%make-hinting-sink :state state :inner inner
                      :initial-map (make-hint-map-for (hs-scale state))
                      :map (make-hint-map-for (hs-scale state))))

(defun close-subpath (sink)
  "Close the open subpath.  The closing line is drawn from its held back
device position, so with the hint map of the subpath's start, as
FreeType does; a line back to the start in character space is left out,
the close draws it."
  (let ((start (hinting-sink-start-point sink))
        (line (hinting-sink-pending-line sink))
        (inner (hinting-sink-inner sink)))
    (setf (hinting-sink-start-point sink) nil
          (hinting-sink-pending-line sink) nil)
    (when start
      (when (and line (not (equal start (subseq line 0 2))))
        (sink-line-to inner (third line) (fourth line)))
      (sink-close inner))))

(defun flush-pending-line (sink)
  (let ((line (hinting-sink-pending-line sink)))
    (when line
      (setf (hinting-sink-pending-line sink) nil)
      (sink-line-to (hinting-sink-inner sink) (third line) (fourth line)))))

(defun build-sink-map (sink map mask)
  (build-hint-map map (hinting-sink-state sink) mask (hinting-sink-initial-map sink)
                  (hinting-sink-stems sink) 0))

(defun hinted-y (sink y)
  "Y in device space through the current hint map, built when needed."
  (let ((map (hinting-sink-map sink)))
    (unless (hint-map-valid-p map)
      (build-sink-map sink map (hinting-sink-mask sink)))
    (trunc-1/64 (hint-map-transform map y))))

(defun scaled-x (sink x)
  (trunc-1/64 (fixed* x (hs-scale (hinting-sink-state sink)))))

(defmethod sink-hstem ((sink hinting-sink) min max)
  ;; Stems after the first map is built are ignored, as in FreeType.
  (let ((stems (hinting-sink-stems sink)))
    (when (and (< (length stems) +max-hints+)
               (not (hint-map-valid-p (hinting-sink-map sink))))
      (vector-push-extend (make-stem-hint :min min :max max) stems))))

(defmethod sink-hint-mask ((sink hinting-sink) bytes)
  ;; An invalid mask means every hint, as in FreeType.
  (let ((mask (or (bytes-hint-mask bytes) (all-hints-mask))))
    (unless (equal mask (hinting-sink-mask sink))
      (setf (hinting-sink-mask sink) mask
            (hint-map-valid-p (hinting-sink-map sink)) nil))))

(defmethod sink-counter-mask ((sink hinting-sink) bytes)
  ;; A map built only to place and lock the stems of the counter mask.
  (build-sink-map sink (make-hint-map-for (hs-scale (hinting-sink-state sink)))
                  (or (bytes-hint-mask bytes) (all-hints-mask))))

(defmethod sink-clear-hints ((sink hinting-sink))
  (let ((scale (hs-scale (hinting-sink-state sink))))
    (setf (fill-pointer (hinting-sink-stems sink)) 0
          (hinting-sink-map sink) (make-hint-map-for scale)
          (hinting-sink-initial-map sink) (make-hint-map-for scale)
          (hinting-sink-mask sink) (all-hints-mask))))

(defmethod sink-move-to ((sink hinting-sink) x y)
  (close-subpath sink)
  (setf (hinting-sink-start-point sink) (list x y))
  (sink-move-to (hinting-sink-inner sink) (scaled-x sink x) (hinted-y sink y)))

(defmethod sink-line-to ((sink hinting-sink) x y)
  (flush-pending-line sink)
  (setf (hinting-sink-pending-line sink) (list x y (scaled-x sink x) (hinted-y sink y))))

(defmethod sink-curve-to ((sink hinting-sink) cx0 cy0 cx1 cy1 x y)
  (flush-pending-line sink)
  (sink-curve-to (hinting-sink-inner sink)
                 (scaled-x sink cx0) (hinted-y sink cy0)
                 (scaled-x sink cx1) (hinted-y sink cy1)
                 (scaled-x sink x) (hinted-y sink y)))

;; Subpaths are closed by the next move, see CLOSE-SUBPATH.
(defmethod sink-close ((sink hinting-sink)))

(defmethod sink-finish ((sink hinting-sink))
  (close-subpath sink)
  (sink-finish (hinting-sink-inner sink)))
