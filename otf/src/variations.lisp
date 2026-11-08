;;;; variations.lisp — Instances of variable fonts
;;;; Copyright (c) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

;;; A loader can be set to an instance of a variable font. User axis values
;;; are normalized through 'fvar' and 'avar' as Fontations does
;;; (Fvar::user_to_normalized, SegmentMaps::apply). CFF2 outlines and
;;; TrueType outlines (with 'gvar', see gvar.lisp and glyf-outlines.lisp) are
;;; then drawn at that instance, advances and side bearings follow 'HVAR' and
;;; 'VVAR' or, without them, the phantom points, and the font-wide metrics
;;; follow 'MVAR'. An 'avar' 2.0 table's mappings are applied after its
;;; segment maps, as Fontations and HarfBuzz do.

(in-package #:zpb-ttf)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

;;; Fixed point helpers, on raw signed 16.16 values

(declaim (inline clamp-i32))
(defun clamp-i32 (x)
  (declare (type integer x)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (the (signed-byte 32) (max (- (ash 1 31)) (min (1- (ash 1 31)) x))))

(defun read-fixed-bits (fd pos) (runes/otf:font-data-read-i32 fd pos))

(declaim (inline f2dot14-bits->fixed))
(defun f2dot14-bits->fixed (bits)
  (declare (type (signed-byte 16) bits))
  (the (signed-byte 32) (ash bits 2)))

;;; Axes

(defun fvar-axes (loader)
  "List of (tag min default max) for each 'fvar' axis, as raw 16.16 values."
  (let ((info (table-info "fvar" loader)))
    (when info
      (let* ((fd (font-data loader))
             (fvar (runes/otf:read-fvar fd (offset info) (size info))))
        (loop for i below (runes/otf:fvar-axis-count fvar)
              collect (let* ((axis (runes/otf:fvar-axis-ref fvar i))
                             (pos (runes/otf::record-ref-offset axis)))
                        (list (runes/otf:variation-axis-record-axis-tag axis)
                              (read-fixed-bits fd (+ pos 4))
                              (read-fixed-bits fd (+ pos 8))
                              (read-fixed-bits fd (+ pos 12)))))))))

(defgeneric variation-axes (loader)
  (:documentation "List of (tag minimum default maximum) for each variation
axis of LOADER's font, in user units; NIL for a font that does not vary.")
  (:method ((loader font-loader))
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (loop for (tag min default max) in (fvar-axes loader)
          collect (list tag (/ min 65536) (/ default 65536) (/ max 65536)))))

(defun normalize-axis-value (value min default max)
  "VALUE (16.16) normalized to [-1, 1] for an axis (VariationAxisRecord::normalize)."
  (declare (type (signed-byte 32) value min default max)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((max (max max min))
         (value (max min (min max value)))
         (result (cond ((< value default)
                        (runes/otf/charstring::fixed-neg
                         (runes/otf/charstring::fixed/ (clamp-i32 (- default value))
                                                     (clamp-i32 (- default min)))))
                       ((> value default)
                        (runes/otf/charstring::fixed/ (clamp-i32 (- value default))
                                                    (clamp-i32 (- max default))))
                       (t 0))))
    (max #x-10000 (min #x10000 result))))

;;; avar 1.0 segment maps

(defun avar-segment-maps (loader)
  "For each axis, a vector of (from . to) 16.16 pairs from 'avar', or NIL.
The second value is the position just past the segment maps."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((info (table-info "avar" loader)))
    (when info
      (let* ((fd (font-data loader))
             (avar (runes/otf:read-avar fd (offset info) (size info)))
             (start (runes/otf:avar-axis-segment-maps-start avar))
             (axis-count (runes/otf:avar-axis-count avar)))
        (flet ((coord (p)
                 (f2dot14-bits->fixed (runes/otf:font-data-read-i16 fd p))))
          (if (zerop axis-count)
              (values nil start)
              ;; Each map is a count and COUNT (from to) pairs, and the next
              ;; axis's map follows it.  I is stepped first, so the loop ends
              ;; before reading past the last map.
              (loop for i below axis-count
                    for pos = start then (+ pos 2 (* count 4))
                    for count = (runes/otf:font-data-read-u16 fd pos)
                    collect (coerce (loop repeat count
                                          for p from (+ pos 2) by 4
                                          collect (cons (coord p) (coord (+ p 2))))
                                    'vector)
                      into maps
                    finally (return (values maps (+ pos 2 (* count 4)))))))))))

;;; avar 2.0: after the segment maps, each axis's coordinate moves by a
;;; delta from an ItemVariationStore, computed at the coordinates the
;;; segment maps gave (Fvar::user_to_normalized in Fontations).

(defun avar2-mapping (loader maps-end)
  "The ItemVariationStore of an 'avar' 2.0 table and the position of its
axis index map (or NIL), from the offsets at MAPS-END; NIL for 'avar' 1.0
or a table without a store."
  (declare (type (unsigned-byte 32) maps-end))
  (let* ((info (table-info "avar" loader))
         (fd (font-data loader))
         (start (the (unsigned-byte 32) (offset info))))
    (when (and (= (runes/otf:font-data-read-u16 fd start) 2)
               (<= (+ maps-end 8) (+ start (the (unsigned-byte 32) (size info)))))
      (let ((map-offset (runes/otf:font-data-read-u32 fd maps-end))
            (store-offset (runes/otf:font-data-read-u32 fd (+ maps-end 4))))
        (unless (zerop store-offset)
          (values (runes/otf:read-item-variation-store fd (+ start store-offset))
                  (unless (zerop map-offset) (+ start map-offset))))))))

(defun fixed->f2dot14-bits (c)
  "16.16 C to 2.14 bits, rounding as Fixed::to_f2dot14 does."
  (declare (type (signed-byte 32) c))
  (ash (the (signed-byte 32) (runes/otf/charstring::s32 (+ c 2))) -2))

(defun avar2-delta (store map-pos fd axis at)
  "The 'avar' 2.0 delta of AXIS at AT (normalized reals), or NIL when the
store cannot be read, which leaves the axis alone."
  (multiple-value-bind (outer inner)
      (if map-pos (delta-set-index map-pos fd axis) (values 0 axis))
    (ignore-errors (runes/otf/charstring:item-variation-delta store outer inner at))))

(defun add-avar2-delta (c delta)
  "16.16 C plus DELTA, which is in 2.14 units: times four, rounded half away
from zero, added wrapping, and clamped to [-1, 1]."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (max #x-10000
       (min #x10000
            (runes/otf/charstring::s32
             (+ c (runes/otf/charstring::round-half-away (/ delta 16384)))))))

(defun apply-avar2 (loader maps-end coords)
  "COORDS (16.16, after the segment maps) moved by 'avar' 2.0 deltas, all
computed at the coordinates before any is added."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (multiple-value-bind (store map-pos) (avar2-mapping loader maps-end)
    (if (null store)
        coords
        (let ((at (mapcar (lambda (c) (/ (fixed->f2dot14-bits c) 16384)) coords))
              (fd (font-data loader)))
          (loop for c in coords
                for axis from 0
                for delta = (avar2-delta store map-pos fd axis at)
                collect (if delta (add-avar2-delta c delta) c))))))

(defun apply-segment-map (maps coord)
  "COORD (16.16) mapped through one axis's segment map, a vector of (from
. to) pairs, as Fontations' SegmentMaps::apply, which follows HarfBuzz."
  (declare (simple-vector maps) (type (signed-byte 32) coord)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (labels ((from (i) (car (aref maps i)))
           (to (i) (cdr (aref maps i)))
           ;; COORD moved as pair I moves its from value
           (shift (i) (+ (- coord (from i)) (to i))))
    (case (length maps)
      (0 coord)
      (1 (shift 0))
      (t
       ;; Some fonts repeat the -1 -> -1 or 1 -> 1 caps; skip the repeat.
       (let* ((end (length maps))
              (start (if (and (= (from 0) #x-10000)
                              (= (to 0) #x-10000)
                              (= (from 1) #x-10000))
                         1
                         0))
              (end (if (and (= (from (1- end)) #x10000)
                            (= (to (1- end)) #x10000)
                            (= (from (- end 2)) #x10000))
                       (1- end)
                       end))
              (i (position coord maps :key #'car :start start :end end)))
         (if i
             ;; COORD is a from value: pairs I to J all have it.
             (let ((j (1- (or (position coord maps :key #'car :test-not #'= :start i :end end)
                              end))))
               (cond ((= i j) (to i))
                     ;; Three: the middle one.
                     ((= j (+ i 2)) (to (1+ i)))
                     ;; More or two: the one nearer 0 on the from side, and
                     ;; at 0 the one with the smaller to value.
                     ((minusp coord) (to j))
                     ((plusp coord) (to i))
                     ((< (abs (to i)) (abs (to j))) (to i))
                     (t (to j))))
             ;; Between from values: interpolate between the pairs around
             ;; COORD, or shift by the first or last pair outside them.
             (let ((k (or (position-if (lambda (from) (< coord from)) maps
                                       :key #'car :start start :end end)
                          end)))
               (cond ((= k start) (shift start))
                     ((= k end) (shift (1- end)))
                     (t (runes/otf/charstring::fixed+
                         (to (1- k))
                         (runes/otf/charstring::fixed-mul-div
                          (runes/otf/charstring::fixed- (to k) (to (1- k)))
                          (runes/otf/charstring::fixed- coord (from (1- k)))
                          (runes/otf/charstring::fixed- (from k) (from (1- k))))))))))))))

(defun normalize-coordinates (loader user-coordinates)
  "Normalized coordinates (reals in [-1, 1], one per axis) for
USER-COORDINATES, an alist of axis tag to user value."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((coords
          ;; Every axis with the tag is set, to permit non-linear
          ;; interpolation; a tag given twice takes its last value.
          (loop for (tag min default max) in (fvar-axes loader)
                for setting = (find tag user-coordinates :key #'car :test #'string= :from-end t)
                collect (if setting
                            (normalize-axis-value (runes/otf/charstring:float->fixed (cdr setting))
                                                  min default max)
                            0))))
    (multiple-value-bind (segment-maps maps-end) (avar-segment-maps loader)
      (loop for maps in segment-maps
            for cell on coords
            do (setf (car cell) (apply-segment-map maps (car cell))))
      (when maps-end
        (setf coords (apply-avar2 loader maps-end coords))))
    ;; To 2.14, rounding as Fixed::to_f2dot14 does.
    (mapcar (lambda (c) (/ (fixed->f2dot14-bits c) 16384)) coords)))

;;; Selecting an instance

(defgeneric variation-coordinates (loader)
  (:documentation "The instance LOADER is set to, as an alist of axis tag to
user value; NIL for the default instance.")
  (:method ((loader font-loader))
    (user-coordinates loader)))

(defgeneric (setf variation-coordinates) (coordinates loader)
  (:documentation "Set LOADER to the instance given by COORDINATES, an alist
of axis tag (a string such as \"wght\") to user value; axes not named stay
at their default. NIL selects the default instance. Glyphs found before
the change keep their old outlines; find them again.")
  (:method (coordinates (loader font-loader))
    (when coordinates
      (unless (table-info "fvar" loader)
        (error 'unsupported-value :description "Font is not variable"
                                  :actual-value coordinates :expected-values '(nil))))
    (set-instance loader (copy-alist coordinates)
                  (when coordinates (normalize-coordinates loader coordinates)))
    coordinates))

(defun set-instance (loader user-coordinates normalized-coordinates)
  "Set LOADER's instance and drop everything that depends on it."
  (setf (user-coordinates loader) user-coordinates
        (normalized-coordinates loader) normalized-coordinates
        (%gvar-coords-cache loader) :unset
        (%blend-coords-cache loader) :unset)
  ;; Outlines, bounding boxes and subfonts depend on the instance.
  (when (glyph-cache loader)
    (fill (the simple-vector (glyph-cache loader)) nil))
  (clrhash (cff-subfonts loader))
  ;; Kerning varies too ('GPOS' with GDEF deltas).
  (clrhash (gpos-kerning-cache loader)))

(defstruct (metrics-positions (:conc-name mp-))
  "Where an 'HVAR' or 'VVAR' table has its item variation store and its two
delta-set index maps (the advance's, then the side bearing's), as positions in
the font data; NIL for what it does not have."
  (store nil :type (or null (unsigned-byte 32)))
  (advance-map nil :type (or null (unsigned-byte 32)))
  (bearing-map nil :type (or null (unsigned-byte 32))))

(declaim (sb-ext:freeze-type metrics-positions))

(defun metrics-positions (loader tag)
  "The METRICS-POSITIONS of the table TAG (\"HVAR\" or \"VVAR\") of LOADER, or NIL
if the font has none.  Found once: the table does not change with the instance."
  (let ((cached (assoc tag (%metrics-positions-cache loader) :test #'string=)))
    (if cached
        (cdr cached)
        (let* ((info (table-info tag loader))
               (positions
                 (when info
                   (let ((fd (font-data loader)) (table (offset info)))
                     (flet ((field (at)
                              ;; Offsets in the table are from its start; 0 is none.
                              (let ((off (runes/otf:font-data-read-u32 fd (+ table at))))
                                (and (plusp off) (+ table off)))))
                       (make-metrics-positions :store (field 4)
                                               :advance-map (field 8)
                                               :bearing-map (field 12)))))))
          (push (cons tag positions) (%metrics-positions-cache loader))
          positions))))

(defun blend-coordinates (loader)
  "The normalized coordinates of LOADER's instance as raw 2.14 numbers in a
simple vector, or NIL for the default instance.  Worked out once for an
instance (SET-INSTANCE forgets it)."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((cache (%blend-coords-cache loader)))
    (if (eq cache :unset)
        (setf (%blend-coords-cache loader)
              (let ((coords (normalized-coordinates loader)))
                (and coords
                     (let ((raw (make-array (length coords))))
                       (dotimes (i (length raw) raw)
                         (setf (svref raw i)
                               (runes/otf/charstring::f2dot14-from-real (elt coords i))))))))
        cache)))

(declaim (ftype (function (t (unsigned-byte 32) (unsigned-byte 32) (unsigned-byte 32) simple-vector) (values fixnum &optional)) item-variation-delta-at))
(defun item-variation-delta-at (fd store outer inner coords)
  "The change of item (OUTER, INNER) of the item variation store at position
STORE of FD, at COORDS (a vector of raw 2.14 numbers), in 16.16 units without
rounding: the same as RUNES/OTF/CHARSTRING:ITEM-VARIATION-DELTA, with the store
read where it lies.  0 for an item the store does not have."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) store)
           (type (unsigned-byte 32) outer inner) (type simple-vector coords))
  (flet ((u16 (pos) (runes/otf:font-data-read-u16 fd pos))
         (u32 (pos) (runes/otf:font-data-read-u32 fd pos)))
    (declare (inline u16 u32))
    (if (or (zerop (length coords))
            (and (= outer #xFFFF) (= inner #xFFFF))
            (>= outer (u16 (+ store 6))))
        0
        (let ((data-offset (u32 (+ store 8 (* 4 outer))))
              (region-offset (u32 (+ store 2))))
          (if (or (zerop data-offset) (zerop region-offset)
                  (>= inner (u16 (+ store data-offset))))
              0
              (let* ((data (+ store data-offset))
                     (regions (+ store region-offset))
                     (axis-count (u16 regions))
                     (region-count (u16 (+ regions 2)))
                     (first-region (+ regions 4))
                     (word-delta-count (u16 (+ data 2)))
                     (words (logand #x7FFF word-delta-count))
                     (long-p (logbitp 15 word-delta-count))
                     (count (u16 (+ data 4)))
                     (wide (if long-p 4 2))
                     (narrow (if long-p 2 1))
                     (row-size (if long-p
                                   (+ (* 4 words) (* 2 (- count words)))
                                   (+ (* 2 words) (- count words))))
                     (row (+ data 6 (* 2 count) (* inner row-size)))
                     (sum 0))
                (declare (type (unsigned-byte 16) axis-count region-count word-delta-count
                               words count)
                         (type fixnum sum))
                (dotimes (i count sum)
                  (let* ((pos (+ row (if (< i words)
                                         (* i wide)
                                         (+ (* words wide) (* (- i words) narrow)))))
                         (delta (if (< i words)
                                    (if long-p
                                        (runes/otf:font-data-read-i32 fd pos)
                                        (runes/otf:font-data-read-i16 fd pos))
                                    (if long-p
                                        (runes/otf:font-data-read-i16 fd pos)
                                        (runes/otf:font-data-read-i8 fd pos))))
                         (region-index (u16 (+ data 6 (* 2 i)))))
                    (unless (< region-index region-count)
                      (runes/otf/charstring::fail 'runes/otf/charstring::charstring-malformed
                                                  (format nil "region ~D out of range" region-index)))
                    ;; A zero delta contributes nothing at any location.
                    (unless (zerop delta)
                      (incf sum (* (the (signed-byte 32) delta)
                                   (runes/otf/charstring::region-scalar
                                    fd (+ first-region (* region-index axis-count 6))
                                    axis-count coords))))))))))))

(defun metrics-delta (loader tag map-accessor gid &key direct)
  "Change at the loader's instance, in font units, of a metric of GID from
the HVAR or VVAR table TAG.  MAP-ACCESSOR (MP-ADVANCE-MAP or MP-BEARING-MAP)
gives the position of the metric's DeltaSetIndexMap.  Without a map, GID
indexes the store directly when DIRECT is true, as for advances; otherwise the
result is NIL, since the font does not vary that metric through this table."
  (declare (type (unsigned-byte 32) gid) (type function map-accessor))
  (let ((coords (blend-coordinates loader))
        (positions (metrics-positions loader tag)))
    (if (or (null coords) (null positions))
        (and direct 0)
        (let ((fd (font-data loader))
              (store (mp-store positions))
              (map-position (funcall map-accessor positions)))
          (flet ((round-delta (sum)
                   (declare (type fixnum sum))
                   ;; Round the 16.16 delta to an integer as FreeType does.
                   (ash (+ sum #x8000) -16)))
            (if (null map-position)
                (if direct
                    (if (or (null store) (> gid #xFFFF))
                        0
                        (round-delta (item-variation-delta-at fd store 0 gid coords)))
                    nil)
                (multiple-value-bind (outer inner) (delta-set-index map-position fd gid)
                  (declare (type (unsigned-byte 32) outer inner))
                  (if (or (null store) (> inner #xFFFF))
                      0
                      (round-delta (item-variation-delta-at fd store outer inner coords))))))))))

(defun advance-width-delta (loader gid)
  "Change of GID's advance width at the loader's instance, from 'HVAR'."
  (metrics-delta loader "HVAR" #'mp-advance-map gid :direct t))

(defun lsb-delta (loader gid)
  "Change of GID's left side bearing from the 'HVAR' LSB mapping, or NIL."
  (metrics-delta loader "HVAR" #'mp-bearing-map gid))

(defun advance-height-delta (loader gid)
  "Change of GID's advance height at the loader's instance, from 'VVAR'."
  (metrics-delta loader "VVAR" #'mp-advance-map gid :direct t))

(defun tsb-delta (loader gid)
  "Change of GID's top side bearing from the 'VVAR' TSB mapping, or NIL."
  (metrics-delta loader "VVAR" #'mp-bearing-map gid))

(defun delta-set-index (pos fd index)
  "Outer and inner delta-set index for INDEX from the DeltaSetIndexMap at POS."
  (declare (type (unsigned-byte 32) pos index) (type runes/otf:font-data fd)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((format (runes/otf:font-data-read-u8 fd pos))
         (entry-format (runes/otf:font-data-read-u8 fd (+ pos 1)))
         (map-count (if (zerop format)
                        (runes/otf:font-data-read-u16 fd (+ pos 2))
                        (runes/otf:font-data-read-u32 fd (+ pos 2))))
         (data (+ pos (if (zerop format) 4 6))))
    (if (zerop map-count)
        (values (ash index -16) (ldb (byte 16 0) index))
        (let* ((entry-size (1+ (ldb (byte 2 4) entry-format)))
               (bit-count (1+ (ldb (byte 4 0) entry-format)))
               ;; Past the end, the last entry applies.
               (entry-pos (+ data (* (min index (1- map-count)) entry-size)))
               (entry (let ((v 0))
                        (dotimes (i entry-size v)
                          (setf v (logior (ash v 8) (runes/otf:font-data-read-u8 fd (+ entry-pos i))))))))
          (values (ldb (byte 16 0) (ash entry (- bit-count)))
                  (ldb (byte bit-count 0) entry))))))

;;; Font-wide metrics ('MVAR')

(defgeneric variation-metric-delta (loader tag)
  (:documentation "Change of the font-wide metric TAG (an MVAR tag string such
as \"xhgt\" or \"cpht\") at LOADER's instance, in font units; 0 at the
default instance or when the font does not vary the metric.")
  (:method ((loader font-loader) tag)
    (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let ((coords (normalized-coordinates loader))
          (info (table-info "MVAR" loader)))
      (if (or (null coords) (null info))
          0
          (let* ((mvar (runes/otf:read-mvar (font-data loader) (offset info) (size info)))
                 (store (runes/otf:mvar-item-variation-store-offset mvar))
                 (record (when store
                           (loop for i below (runes/otf:mvar-value-record-count mvar)
                                 for r = (runes/otf:mvar-value-record-ref mvar i)
                                 when (string= (runes/otf:mvar-value-record-value-tag r) tag)
                                   return r))))
            (if record
                ;; Rounded to an integer as FreeType does.
                (ash (+ (runes/otf/charstring:item-variation-delta
                         store
                         (runes/otf:mvar-value-record-delta-set-outer-index record)
                         (runes/otf:mvar-value-record-delta-set-inner-index record)
                         coords)
                        #x8000)
                     -16)
                0))))))

;;; The metrics zpb-ttf reports follow the instance, with the tags skrifa
;;; applies to them.

(defmacro define-varied-metric (reader tag &optional condition)
  `(defmethod ,reader :around ((loader font-loader))
     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     (let ((value (call-next-method)))
       (if (and (normalized-coordinates loader) ,@(when condition (list condition)))
           (+ value (variation-metric-delta loader ,tag))
           value))))

(define-varied-metric ascender "hasc")
(define-varied-metric descender "hdsc")
(define-varied-metric line-gap "hlgp")
(define-varied-metric underline-position "undo")
(define-varied-metric underline-thickness "unds")
;; Without 'vhea' these are made up from the horizontal metrics.
(define-varied-metric vascender "vasc" (not (vhea-missing-p loader)))
(define-varied-metric vdescender "vdsc" (not (vhea-missing-p loader)))

;;; For code that reads tables of its own, as a renderer reads 'COLR'

(defun table-position (loader tag)
  "Where the table TAG (a string such as \"COLR\") of LOADER's font is:
(VALUES FONT-DATA POSITION SIZE), to be read with FONT-DATA-READ-U16 and the
others; NIL when the font has no such table."
  (let ((info (table-info tag loader)))
    (when info
      (values (font-data loader) (offset info) (size info)))))

(defun variation-delta (loader store index &optional map)
  "The change, at LOADER's instance, of the item INDEX of the item variation
store at position STORE of its font data, in 16.16 fixed point without rounding;
0 at the default instance.  INDEX is an outer index shifted 16 bits left and an
inner one, or goes through the DeltaSetIndexMap at position MAP, when there is
one."
  (let ((coords (blend-coordinates loader)))
    (if (or (null coords) (zerop store))
        0
        (let ((fd (font-data loader)))
          (multiple-value-bind (outer inner)
              (if (and map (plusp map))
                  (delta-set-index map fd index)
                  (values (ash index -16) (ldb (byte 16 0) index)))
            (item-variation-delta-at fd store outer inner coords))))))
