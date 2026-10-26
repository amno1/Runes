;;;; variations.lisp — Instances of variable fonts
;;;; Copyright (c) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

;;; A loader can be set to an instance of a variable font. User axis values
;;; are normalized through 'fvar' and 'avar' as Fontations does
;;; (Fvar::user_to_normalized, SegmentMaps::apply). CFF2 outlines and
;;; TrueType outlines (with 'gvar', see gvar.lisp and glyf-outlines.lisp) are
;;; then drawn at that instance, advance widths follow 'HVAR' or, without it,
;;; the phantom points, and the font-wide metrics follow 'MVAR'. 'avar' 2.0
;;; mappings beyond the segment maps and side bearing variations are not
;;; applied.

(in-package #:zpb-ttf)

;;; Fixed point helpers, on raw signed 16.16 values

(defun clamp-i32 (x) (max (- (ash 1 31)) (min (1- (ash 1 31)) x)))

(defun read-fixed-bits (fd pos) (zpb-otf:font-data-read-i32 fd pos))

(defun f2dot14-bits->fixed (bits) (ash bits 2))

;;; Axes

(defun fvar-axes (loader)
  "List of (tag min default max) for each 'fvar' axis, as raw 16.16 values."
  (let ((info (table-info "fvar" loader)))
    (when info
      (let* ((fd (font-data loader))
             (fvar (zpb-otf:read-fvar fd (offset info) (size info))))
        (loop for i below (zpb-otf:fvar-axis-count fvar)
              collect (let* ((axis (zpb-otf:fvar-axis-ref fvar i))
                             (pos (zpb-otf::record-ref-offset axis)))
                        (list (zpb-otf:variation-axis-record-axis-tag axis)
                              (read-fixed-bits fd (+ pos 4))
                              (read-fixed-bits fd (+ pos 8))
                              (read-fixed-bits fd (+ pos 12)))))))))

(defgeneric variation-axes (loader)
  (:documentation "List of (tag minimum default maximum) for each variation
axis of LOADER's font, in user units; NIL for a font that does not vary.")
  (:method ((loader font-loader))
    (loop for (tag min default max) in (fvar-axes loader)
          collect (list tag (/ min 65536) (/ default 65536) (/ max 65536)))))

(defun normalize-axis-value (value min default max)
  "VALUE (16.16) normalized to [-1, 1] for an axis (VariationAxisRecord::normalize)."
  (let* ((max (max max min))
         (value (max min (min max value)))
         (result (cond ((< value default)
                        (zpb-otf/charstring::fixed-neg
                         (zpb-otf/charstring::fixed/ (clamp-i32 (- default value))
                                                     (clamp-i32 (- default min)))))
                       ((> value default)
                        (zpb-otf/charstring::fixed/ (clamp-i32 (- value default))
                                                    (clamp-i32 (- max default))))
                       (t 0))))
    (max #x-10000 (min #x10000 result))))

;;; avar 1.0 segment maps

(defun avar-segment-maps (loader)
  "For each axis, a vector of (from . to) 16.16 pairs from 'avar', or NIL."
  (let ((info (table-info "avar" loader)))
    (when info
      (let* ((fd (font-data loader))
             (avar (zpb-otf:read-avar fd (offset info) (size info)))
             (pos (zpb-otf:avar-axis-segment-maps-start avar)))
        (loop repeat (zpb-otf:avar-axis-count avar)
              collect (let ((count (zpb-otf:font-data-read-u16 fd pos)))
                        (prog1 (coerce (loop for i below count
                                             for p = (+ pos 2 (* i 4))
                                             collect (cons (f2dot14-bits->fixed (zpb-otf:font-data-read-i16 fd p))
                                                           (f2dot14-bits->fixed (zpb-otf:font-data-read-i16 fd (+ p 2)))))
                                       'vector)
                          (incf pos (+ 2 (* count 4))))))))))

(defun apply-segment-map (maps coord)
  "COORD (16.16) mapped through one axis's segment map, as SegmentMaps::apply."
  (let ((len (length maps)))
    (flet ((from (i) (car (aref maps i)))
           (to (i) (cdr (aref maps i))))
      (cond
        ((zerop len) coord)
        ((= len 1) (+ (- coord (from 0)) (to 0)))
        (t
         (let ((start 0) (end len))
           ;; Trim duplicate -1/+1 caps, as HarfBuzz does.
           (when (and (= (from 0) #x-10000) (= (to 0) #x-10000) (= (from 1) #x-10000))
             (incf start))
           (when (and (= (from (1- end)) #x10000) (= (to (1- end)) #x10000) (= (from (- end 2)) #x10000))
             (decf end))
           (let ((i (loop for k from start below end when (= coord (from k)) return k)))
             (if i
                 (let ((j i))
                   (loop while (and (< (1+ j) end) (= coord (from (1+ j)))) do (incf j))
                   (cond ((= i j) (to i))
                         ((= (+ i 2) j) (to (1+ i)))
                         ((minusp coord) (to j))
                         ((plusp coord) (to i))
                         (t (if (< (abs (to i)) (abs (to j))) (to i) (to j)))))
                 (let ((k (or (loop for k from start below end when (< coord (from k)) return k) end)))
                   (cond
                     ((= k start) (+ (- coord (from start)) (to start)))
                     ((= k end) (+ (- coord (from (1- end))) (to (1- end))))
                     (t (let ((bf (from (1- k))) (bt (to (1- k))) (af (from k)) (at (to k)))
                          (zpb-otf/charstring::fixed+
                           bt (zpb-otf/charstring::fixed-mul-div
                               (zpb-otf/charstring::fixed- at bt)
                               (zpb-otf/charstring::fixed- coord bf)
                               (zpb-otf/charstring::fixed- af bf)))))))))))))))

(defun normalize-coordinates (loader user-coordinates)
  "Normalized coordinates (reals in [-1, 1], one per axis) for
USER-COORDINATES, an alist of axis tag to user value."
  (let* ((axes (fvar-axes loader))
         (coords (make-list (length axes) :initial-element 0)))
    ;; Every axis with the tag is set, to permit non-linear interpolation.
    (loop for (tag . value) in user-coordinates
          do (loop for (axis-tag min default max) in axes
                   for cell on coords
                   when (string= tag axis-tag)
                     do (setf (car cell)
                              (normalize-axis-value (zpb-otf/charstring:float->fixed value)
                                                    min default max))))
    (loop for maps in (avar-segment-maps loader)
          for cell on coords
          do (setf (car cell) (apply-segment-map maps (car cell))))
    ;; To 2.14, rounding as Fixed::to_f2dot14 does.
    (mapcar (lambda (c) (/ (ash (zpb-otf/charstring::s32 (+ c 2)) -2) 16384)) coords)))

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
        (normalized-coordinates loader) normalized-coordinates)
  ;; Outlines, bounding boxes and subfonts depend on the instance.
  (when (glyph-cache loader)
    (fill (glyph-cache loader) nil))
  (clrhash (cff-subfonts loader)))

(defun advance-width-delta (loader gid)
  "Change of GID's advance width at the loader's instance, in font units, from 'HVAR'."
  (let ((coords (normalized-coordinates loader))
        (info (table-info "HVAR" loader)))
    (if (or (null coords) (null info))
        0
        (let* ((fd (font-data loader))
               (hvar (zpb-otf:read-hvar fd (offset info) (size info)))
               (store (zpb-otf:hvar-item-variation-store-offset hvar))
               (map-offset (zpb-otf:hvar-advance-width-mapping-offset-offset hvar)))
          (multiple-value-bind (outer inner)
              (if (and map-offset (plusp map-offset))
                  (delta-set-index (+ (offset info) map-offset) fd gid)
                  (values 0 gid))
            (if (or (null store) (> inner #xFFFF))
                0
                ;; Round the 16.16 delta to an integer as FreeType does.
                (ash (+ (zpb-otf/charstring:item-variation-delta store outer inner coords) #x8000) -16)))))))

(defun delta-set-index (pos fd index)
  "Outer and inner delta-set index for INDEX from the DeltaSetIndexMap at POS."
  (let* ((format (zpb-otf:font-data-read-u8 fd pos))
         (entry-format (zpb-otf:font-data-read-u8 fd (+ pos 1)))
         (map-count (if (zerop format)
                        (zpb-otf:font-data-read-u16 fd (+ pos 2))
                        (zpb-otf:font-data-read-u32 fd (+ pos 2))))
         (data (+ pos (if (zerop format) 4 6))))
    (if (zerop map-count)
        (values (ash index -16) (ldb (byte 16 0) index))
        (let* ((entry-size (1+ (ldb (byte 2 4) entry-format)))
               (bit-count (1+ (ldb (byte 4 0) entry-format)))
               ;; Past the end, the last entry applies.
               (entry-pos (+ data (* (min index (1- map-count)) entry-size)))
               (entry (let ((v 0))
                        (dotimes (i entry-size v)
                          (setf v (logior (ash v 8) (zpb-otf:font-data-read-u8 fd (+ entry-pos i))))))))
          (values (ldb (byte 16 0) (ash entry (- bit-count)))
                  (ldb (byte bit-count 0) entry))))))

;;; Font-wide metrics ('MVAR')

(defgeneric variation-metric-delta (loader tag)
  (:documentation "Change of the font-wide metric TAG (an MVAR tag string such
as \"xhgt\" or \"cpht\") at LOADER's instance, in font units; 0 at the
default instance or when the font does not vary the metric.")
  (:method ((loader font-loader) tag)
    (let ((coords (normalized-coordinates loader))
          (info (table-info "MVAR" loader)))
      (if (or (null coords) (null info))
          0
          (let* ((mvar (zpb-otf:read-mvar (font-data loader) (offset info) (size info)))
                 (store (zpb-otf:mvar-item-variation-store-offset mvar))
                 (record (when store
                           (loop for i below (zpb-otf:mvar-value-record-count mvar)
                                 for r = (zpb-otf:mvar-value-record-ref mvar i)
                                 when (string= (zpb-otf:mvar-value-record-value-tag r) tag)
                                   return r))))
            (if record
                ;; Rounded to an integer as FreeType does.
                (ash (+ (zpb-otf/charstring:item-variation-delta
                         store
                         (zpb-otf:mvar-value-record-delta-set-outer-index record)
                         (zpb-otf:mvar-value-record-delta-set-inner-index record)
                         coords)
                        #x8000)
                     -16)
                0))))))

;;; The metrics zpb-ttf reports follow the instance, with the tags skrifa
;;; applies to them.

(defmacro define-varied-metric (reader tag &optional condition)
  `(defmethod ,reader :around ((loader font-loader))
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
