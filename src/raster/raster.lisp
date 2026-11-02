;;;; raster.lisp — strings rasterized into alpha maps via cl-vectors and cl-aa
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

;;; Font rendering 
(defun make-state (font)
  "Wrapper around antialiasing and not antialiasing renderers."
  (if (font-antialias font)
      (aa:make-state)
      (aa-bin:make-state)))

(defun aa-bin/update-state (state paths)
  "Update state for not antialiasing renderer."
  (if (listp paths)
      (dolist (path paths)
        (aa-bin/update-state state path))
      (let ((iterator (paths:path-iterator-segmented paths)))
        (multiple-value-bind (i1 k1 e1) (paths:path-iterator-next iterator)
          (declare (ignore i1))
          (when (and k1 (not e1))
            ;; at least 2 knots
            (let ((first-knot k1))
              (loop
                (multiple-value-bind (i2 k2 e2) (paths:path-iterator-next iterator)
                  (declare (ignore i2))
                  (aa-bin:line-f state
                                 (paths:point-x k1) (paths:point-y k1)
                                 (paths:point-x k2) (paths:point-y k2))
                  (setf k1 k2)
                  (when e2
                    (return))))
              (aa-bin:line-f state
                             (paths:point-x k1) (paths:point-y k1)
                             (paths:point-x first-knot) (paths:point-y first-knot))))))))

(defun update-state (font state paths)
  "Wrapper around antialiasing and not antialiasing renderers."
  (if (font-antialias font)
      (vectors:update-state state paths)
      (aa-bin/update-state state paths)))

(defun cells-sweep (font state function &optional function-span)
  "Wrapper around antialiasing and not antialiasing renderers."
  (if (font-antialias font)
      (aa:cells-sweep state function function-span)
      (aa-bin:cells-sweep state function function-span)))

(defun render-alpha-map (dpi-x dpi-y font string bbox)
  "Rasterize STRING in FONT at DPI-X x DPI-Y into an alpha map covering
   BBOX (a pixel bounding box as returned by the bounding-box providers).
   Returns the list (ARRAY MIN-X MAX-Y WIDTH HEIGHT), or (NIL 0 0 0 0)
   when BBOX is empty."
  (with-font-loader (font-loader font)
    (let* ((min-x (xmin bbox))
           (min-y (ymin bbox))
           (max-x (xmax bbox))
           (max-y (ymax bbox))
           (width  (- max-x min-x))
           (height (- max-y min-y)))
      (if (or (<= width 0) (<= height 0))
          (list nil 0 0 0 0)
          (let* ((units->pixels-x (font-units->pixels-x dpi-x font))
                 (units->pixels-y (font-units->pixels-y dpi-y font))
                 (array (make-array (list height width)
                                    :initial-element 0
                                    :element-type '(unsigned-byte 8)))
                 (state (make-state font))
                 (paths (paths-ttf:paths-from-string font-loader string
                                                     :offset (paths:make-point (- min-x)
                                                                               max-y)
                                                     :scale-x units->pixels-x
                                                     :scale-y (- units->pixels-y)))
                 (thickness (* units->pixels-y (zpb-ttf:underline-thickness font-loader)))
                 (underline-offset (* units->pixels-y (zpb-ttf:underline-position font-loader))))
            ;; Decoration rectangles are in array coordinates, which are
            ;; shifted by -MIN-X relative to the glyph pen, so they span
            ;; 0..WIDTH (not 0..MAX-X).
            (when (font-underline font)
              (push (paths:make-rectangle-path 0 (- max-y underline-offset)
                                               width (+ (- max-y underline-offset) thickness))
                    paths))
            (when (font-strikethrough font)
              (let ((strike-offset (* 2 underline-offset)))
                (push (paths:make-rectangle-path 0 (+ max-y strike-offset)
                                                 width (+ max-y strike-offset thickness))
                      paths)))
            (when (font-overline font)
              ;; Above the ascender by the underline's distance below the
              ;; baseline.  UNDERLINE-OFFSET is negative (TrueType puts the
              ;; underline below the baseline), and array y grows downward.
              (let* ((ascend (* units->pixels-y (zpb-ttf:ascender font-loader)))
                     (top (+ (- max-y ascend) underline-offset)))
                (push (paths:make-rectangle-path 0 top width (- top thickness))
                      paths)))
            (update-state font state paths)
            (cells-sweep font state
                         ;; CELLS-SWEEP visits each pixel once, with the
                         ;; coverage of all the paths together: that is the
                         ;; pixel's alpha.  (The old blend with the array's
                         ;; previous value, always 0, turned full coverage
                         ;; into 254.)
                         (lambda (x y alpha)
                           (when (and (<= 0 x (1- width))
                                      (<= 0 y (1- height)))
                             (setf (aref array y x) (min 255 (abs alpha))))))
            (list array min-x max-y width height))))))

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
