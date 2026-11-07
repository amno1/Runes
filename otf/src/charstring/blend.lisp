;;;; blend.lisp — CFF2 blend state over an ItemVariationStore
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; A port of Fontations' read-fonts src/ps/cff/blend.rs, with the region
;;; scalar computation of VariationRegion::compute_scalar
;;; (src/tables/variations.rs).
;;;
;;; Fontations precomputes the scalars of the first 16 regions and computes
;;; the rest on demand to avoid allocation; here all scalars of the active
;;; ItemVariationData are computed when it is selected. The values are the
;;; same.

(in-package #:runes/otf/charstring)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(defun f2dot14-from-real (x)
  "Raw 2.14 bits of the normalized coordinate X, clamped to [-2, 2)."
  (declare (type real x)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (max -32768 (min 32767 (round-half-away (* (rational x) 16384)))))

(defun fixed-mul-div (a b c)
  "A * B / C, rounded, like font-types' Fixed::mul_div (FreeType's FT_MulDiv)."
  (declare (type (signed-byte 32) a b c) (optimize (speed 3))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((negative (oddp (+ (if (minusp a) 1 0) (if (minusp b) 1 0) (if (minusp c) 1 0))))
         ;; At most 2^62 + 2^30: 63 bits.
         (result (if (zerop c)
                     #x7FFFFFFF
                     (values (floor (the (unsigned-byte 63)
                                         (+ (* (abs a) (abs b)) (ash (abs c) -1)))
                                    (abs c))))))
    (declare (type (unsigned-byte 63) result))
    (s32 (if negative (- (s32 result)) (s32 result)))))

(defun region-scalar (font-data region-pos axis-count coords)
  "Scalar of the VariationRegion at REGION-POS for COORDS, a vector of raw
2.14 values; missing trailing coordinates are 0."
  (declare (type runes/otf:font-data font-data) (fixnum region-pos)
           (type (unsigned-byte 16) axis-count) (simple-vector coords)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((scalar #x10000))
    (declare (type (signed-byte 32) scalar))
    (dotimes (i axis-count scalar)
      (let* ((pos (+ region-pos (* i 6)))
             (start2 (runes/otf:font-data-read-i16 font-data pos))
             (peak2 (runes/otf:font-data-read-i16 font-data (+ pos 2)))
             (end2 (runes/otf:font-data-read-i16 font-data (+ pos 4)))
             (coord2 (the fixnum (if (< i (length coords)) (svref coords i) 0))))
        (declare (type fixnum start2 peak2 end2 coord2 pos))
        ;; An axis with no peak does not restrict the region, and a
        ;; coordinate at the peak contributes a factor of one.
        (unless (or (zerop peak2) (= coord2 peak2))
          (let ((peak (ash peak2 2))
                (start (ash start2 2))
                (end (ash end2 2))
                (coord (ash coord2 2)))
            ;; An invalid axis is ignored.
            (unless (or (> start peak) (> peak end) (and (minusp start) (plusp end)))
              (cond ((or (< coord start) (> coord end))
                     (return 0))
                    ((< coord peak)
                     (setf scalar (fixed-mul-div scalar (fixed- coord start) (fixed- peak start))))
                    (t
                     (setf scalar (fixed-mul-div scalar (fixed- end coord) (fixed- end peak))))))))))))

(defclass item-variation-blend-state ()
  ((store :initarg :store :reader blend-store
          :documentation "A RUNES/OTF:ITEM-VARIATION-STORE.")
   (coords :initarg :coords :reader blend-coords
           :documentation "Normalized coordinates as raw 2.14 values.")
   ;; Set only by BLEND-SET-STORE-INDEX, through the internal writers.
   (store-index :initform nil :reader blend-store-index :writer %set-store-index)
   (scalars :initform #() :reader blend-scalars :writer %set-scalars))
  (:documentation "Blend state for CFF2 charstrings and DICTs at a location in
the font's variation space."))

(defun make-blend-state (store coords store-index)
  "Blend state for the RUNES/OTF:ITEM-VARIATION-STORE STORE at COORDS, a
sequence of normalized axis coordinates (reals in [-1, 1], one per axis in
'fvar' order). STORE-INDEX is the initial vsindex: for a charstring, the
vsindex of its subfont's Private DICT, or 0 if that has none."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((state (make-instance 'item-variation-blend-state
                              :store store
                              :coords (map 'vector #'f2dot14-from-real coords))))
    (blend-set-store-index state store-index)
    state))

(defmethod blend-region-count ((b item-variation-blend-state))
  (length (the vector (blend-scalars b))))

(defmethod blend-set-store-index ((b item-variation-blend-state) index)
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (unless (eql index (blend-store-index b))
    (let* ((store (blend-store b))
           (data (when (< -1 index (runes/otf:item-variation-store-item-variation-data-count store))
                   (runes/otf:item-variation-store-item-variation-data-offsets-ref store index)))
           (regions (runes/otf:item-variation-store-variation-region-list-offset store)))
      (unless (and data regions)
        (fail 'charstring-malformed (format nil "no item variation data ~D" index)))
      (let* ((fd (runes/otf::table-ref-font-data regions))
             (axis-count (runes/otf:variation-region-list-axis-count regions))
             (region-count (runes/otf:variation-region-list-region-count regions))
             (first-region (runes/otf:variation-region-list-variation-regions-start regions))
             (coords (the simple-vector (blend-coords b))))
        (%set-scalars
         (map 'vector
              (lambda (i)
                (let ((region-index (runes/otf:item-variation-data-region-indexes-ref data i)))
                  (unless (< region-index region-count)
                    (fail 'charstring-malformed
                          (format nil "region ~D out of range" region-index)))
                  (region-scalar fd (+ first-region (* region-index axis-count 6))
                                 axis-count coords)))
              (alexandria:iota (runes/otf:item-variation-data-region-index-count data)))
         b)
        (%set-store-index index b))))
  b)

(defun item-variation-delta (store outer inner coords)
  "Delta of item (OUTER, INNER) of the RUNES/OTF:ITEM-VARIATION-STORE STORE at
COORDS (normalized reals, one per axis), in 16.16 units without rounding,
like Fontations' ItemVariationStore::compute_delta. 0 for an item the store
does not have."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((coords (map 'vector #'f2dot14-from-real coords)))
    (if (or (zerop (length coords))
            (and (= outer #xFFFF) (= inner #xFFFF))
            (>= outer (runes/otf:item-variation-store-item-variation-data-count store)))
        0
        (let* ((data (runes/otf:item-variation-store-item-variation-data-offsets-ref store outer))
               (regions (runes/otf:item-variation-store-variation-region-list-offset store)))
          (if (or (null data) (null regions)
                  (>= inner (runes/otf:item-variation-data-item-count data)))
              0
              (let ((fd (runes/otf::table-ref-font-data regions))
                    (axis-count (runes/otf:variation-region-list-axis-count regions))
                    (region-count (runes/otf:variation-region-list-region-count regions))
                    (first-region (runes/otf:variation-region-list-variation-regions-start regions))
                    (sum 0))
                (dotimes (i (runes/otf:item-variation-data-region-index-count data) sum)
                  (let ((delta (runes/otf:item-variation-data-delta-set-ref data inner i))
                        (region-index (runes/otf:item-variation-data-region-indexes-ref data i)))
                    (unless (< region-index region-count)
                      (fail 'charstring-malformed (format nil "region ~D out of range" region-index)))
                    ;; A zero delta contributes nothing at any location.
                    (unless (zerop delta)
                      (incf sum (* delta (region-scalar fd (+ first-region (* region-index axis-count 6))
                                                        axis-count coords))))))))))))
