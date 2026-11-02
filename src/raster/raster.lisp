;;;; raster.lisp — strings rasterized into alpha maps, cached per font
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes/raster)

;;; Each font's alpha maps are cached like its bounding boxes, per DPI and
;;; string, but here rather than in the font, which knows nothing of
;;; rasterizing: weak on the font, and flushed with its other caches.

(defvar *alpha-maps* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq)
  "Font -> (STRING-ALPHA-MAPS . STRING-LINE-ALPHA-MAPS), two caches like
   FONT-STRING-BBOXES.")

(defvar *alpha-maps-lock* (bt:make-lock "runes/raster alpha maps"))

(defun %alpha-maps (font)
  (bt:with-lock-held (*alpha-maps-lock*)
    (or (gethash font *alpha-maps*)
        (setf (gethash font *alpha-maps*)
              (cons (make-font-cache font 10 1000 'text-pixarray-provider)
                    (make-font-cache font 10 1000 'text-line-pixarray-provider))))))

(defun font-string-alpha-maps (font) (car (%alpha-maps font)))
(defun font-string-line-alpha-maps (font) (cdr (%alpha-maps font)))

(defmethod flush-font-caches :after ((font font))
  (bt:with-lock-held (*alpha-maps-lock*)
    (remhash font *alpha-maps*)))

(defun text-pixarray-provider (dpi-x dpi-y font string)
  "Internal: cache provider for FONT-STRING-ALPHA-MAPS."
  (render-alpha-map dpi-x dpi-y font string
                    (font-cache-fetch (font-string-bboxes font) (cons dpi-x dpi-y) string)))

(defun text-pixarray-for-dpi (dpi-x dpi-y font string)
  "Render a text string in FONT for DPI-X x DPI-Y, returning an alpha mask and dimensions.
   Returns 5 values: alpha mask, min-x, max-y, width, height."
  (values-list
   (font-cache-fetch
    (font-string-alpha-maps font)
    (cons dpi-x dpi-y)
    string)))

(defun text-pixarray-for-drawable (drawable font string)
  "Render a text string in FONT for DRAWABLE, returning an alpha mask and dimensions.
   Returns 5 values: alpha mask, min-x, max-y, width, height."
  (multiple-value-bind (dpi-x dpi-y) (target-dpi drawable)
    (text-pixarray-for-dpi dpi-x dpi-y font string)))

(defun text-pixarray (drawable-or-dpi-x font-or-dpi-y &optional string-or-font (maybe-string nil m-p))
  "Render a text string of 'font', returning an alpha mask and dimensions.
   Can be called as (TEXT-PIXARRAY DPI-X DPI-Y FONT STRING) or
   (TEXT-PIXARRAY DRAWABLE FONT STRING).
   Returns 5 values: alpha mask, min-x, max-y, width, height."
  (if m-p
      (text-pixarray-for-dpi drawable-or-dpi-x font-or-dpi-y string-or-font maybe-string)
      (text-pixarray-for-drawable drawable-or-dpi-x font-or-dpi-y string-or-font)))

(defun text-line-pixarray-provider (dpi-x dpi-y font string)
  "Internal: cache provider for FONT-STRING-LINE-ALPHA-MAPS."
  (render-alpha-map dpi-x dpi-y font string
                    (font-cache-fetch (font-string-line-bboxes font) (cons dpi-x dpi-y) string)))

(defun text-line-pixarray-for-dpi (dpi-x dpi-y font string)
  "Render a text line in FONT for DPI-X x DPI-Y, returning an alpha mask and dimensions.
   Returns 5 values: alpha mask, min-x, max-y, width, height."
  (values-list
   (font-cache-fetch
    (font-string-line-alpha-maps font)
    (cons dpi-x dpi-y)
    string)))

(defun text-line-pixarray-for-drawable (drawable font string)
  "Render a text line in FONT for DRAWABLE, returning an alpha mask and dimensions.
   Returns 5 values: alpha mask, min-x, max-y, width, height."
  (multiple-value-bind (dpi-x dpi-y) (target-dpi drawable)
    (text-line-pixarray-for-dpi dpi-x dpi-y font string)))

(defun text-line-pixarray (drawable-or-dpi-x font-or-dpi-y &optional string-or-font (maybe-string nil m-p))
  "Render a text line of 'font', returning an alpha mask and dimensions.
   Can be called as (TEXT-LINE-PIXARRAY DPI-X DPI-Y FONT STRING) or
   (TEXT-LINE-PIXARRAY DRAWABLE FONT STRING).
   Returns 5 values: alpha mask, min-x, max-y, width, height."
  (if m-p
      (text-line-pixarray-for-dpi drawable-or-dpi-x font-or-dpi-y string-or-font maybe-string)
      (text-line-pixarray-for-drawable drawable-or-dpi-x font-or-dpi-y string-or-font)))
