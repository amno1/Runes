;;;; cl-vectors.lisp — the cl-vectors and cl-aa rasterizer of runes/raster
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes/raster)

;;; RENDER-ALPHA-MAP and RASTERIZE-GLYPH, the two functions a rasterizer
;;; provides; antron.lisp has the same two for runes/raster-antron.

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

(defun rasterize-glyph (font glyph sx sy)
  "Rasterize the zpb-ttf GLYPH of FONT, scaled by SX, SY pixels per font
   unit, with its origin at a whole pixel.  Call with the font loader held."
  (let* ((bbox (zpb-ttf:bounding-box glyph))
         (x0 (floor   (* sx (zpb-ttf:xmin bbox))))
         (x1 (ceiling (* sx (zpb-ttf:xmax bbox))))
         (y0 (floor   (* sy (zpb-ttf:ymin bbox))))
         (y1 (ceiling (* sy (zpb-ttf:ymax bbox))))
         (width  (max 0 (- x1 x0)))
         (height (max 0 (- y1 y0)))
         (stride (* 4 (ceiling width 4)))
         (data (make-array (* stride height) :element-type '(unsigned-byte 8)
                                             :initial-element 0)))
    (when (and (plusp width) (plusp height))
      (let ((state (make-state font)))
        ;; Y grows downward in the image, so the outline is flipped and
        ;; the glyph's top, Y1 above the baseline, lands on row 0.
        (update-state font state
                      (paths-ttf:paths-from-glyph glyph
                                                  :offset (paths:make-point (- x0) y1)
                                                  :scale-x sx
                                                  :scale-y (- sy)))
        (cells-sweep font state
                     (lambda (x y alpha)
                       (when (and (< -1 x width) (< -1 y height))
                         (setf (aref data (+ (* y stride) x))
                               (min 255 (abs alpha))))))))
    (%make-glyph-image :width width :height height :x (- x0) :y y1
                       :advance (* sx (zpb-ttf:advance-width glyph))
                       :data data)))
