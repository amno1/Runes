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

(in-package #:zpb-otf/charstring)

(defun f2dot14-from-real (x)
  "Raw 2.14 bits of the normalized coordinate X, clamped to [-2, 2)."
  (max -32768 (min 32767 (round (* x 16384)))))

(defun fixed-mul-div (a b c)
  "A * B / C, rounded, like font-types' Fixed::mul_div (FreeType's FT_MulDiv)."
  (let* ((sign (if (oddp (count-if #'minusp (list a b c))) -1 1))
         (result (if (zerop c)
                     #x7FFFFFFF
                     (floor (+ (* (abs a) (abs b)) (ash (abs c) -1)) (abs c)))))
    (s32 (* sign (s32 result)))))

(defun region-scalar (font-data region-pos axis-count coords)
  "Scalar of the VariationRegion at REGION-POS for COORDS, a vector of raw
2.14 values; missing trailing coordinates are 0."
  (let ((scalar #x10000))
    (dotimes (i axis-count scalar)
      (let* ((pos (+ region-pos (* i 6)))
             (start2 (zpb-otf:font-data-read-i16 font-data pos))
             (peak2 (zpb-otf:font-data-read-i16 font-data (+ pos 2)))
             (end2 (zpb-otf:font-data-read-i16 font-data (+ pos 4)))
             (coord2 (if (< i (length coords)) (aref coords i) 0)))
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
          :documentation "A ZPB-OTF:ITEM-VARIATION-STORE.")
   (coords :initarg :coords :reader blend-coords
           :documentation "Normalized coordinates as raw 2.14 values.")
   (store-index :initform nil :reader blend-store-index)
   (scalars :initform #() :reader blend-scalars))
  (:documentation "Blend state for CFF2 charstrings and DICTs at a location in
the font's variation space."))

(defun make-blend-state (store coords store-index)
  "Blend state for the ZPB-OTF:ITEM-VARIATION-STORE STORE at COORDS, a
sequence of normalized axis coordinates (reals in [-1, 1], one per axis in
'fvar' order). STORE-INDEX is the initial vsindex: for a charstring, the
vsindex of its subfont's Private DICT, or 0 if that has none."
  (let ((state (make-instance 'item-variation-blend-state
                              :store store
                              :coords (map 'vector #'f2dot14-from-real coords))))
    (blend-set-store-index state store-index)
    state))

(defmethod blend-region-count ((b item-variation-blend-state))
  (length (blend-scalars b)))

(defmethod blend-set-store-index ((b item-variation-blend-state) index)
  (unless (eql index (blend-store-index b))
    (let* ((store (blend-store b))
           (data (when (< -1 index (zpb-otf:item-variation-store-item-variation-data-count store))
                   (zpb-otf:item-variation-store-item-variation-data-offsets-ref store index)))
           (regions (zpb-otf:item-variation-store-variation-region-list-offset store)))
      (unless (and data regions)
        (fail 'charstring-malformed (format nil "no item variation data ~D" index)))
      (let* ((fd (zpb-otf::table-ref-font-data regions))
             (axis-count (zpb-otf:variation-region-list-axis-count regions))
             (region-count (zpb-otf:variation-region-list-region-count regions))
             (first-region (zpb-otf:variation-region-list-variation-regions-start regions))
             (coords (blend-coords b)))
        (setf (slot-value b 'scalars)
              (map 'vector
                   (lambda (i)
                     (let ((region-index (zpb-otf:item-variation-data-region-indexes-ref data i)))
                       (unless (< region-index region-count)
                         (fail 'charstring-malformed
                               (format nil "region ~D out of range" region-index)))
                       (region-scalar fd (+ first-region (* region-index axis-count 6))
                                      axis-count coords)))
                   (alexandria:iota (zpb-otf:item-variation-data-region-index-count data))))
        (setf (slot-value b 'store-index) index))))
  b)
