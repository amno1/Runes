;;;; antron.lisp — the Antron rasterizer of runes/raster-antron
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes/raster)

;;; RENDER-ALPHA-MAP and RASTERIZE-GLYPH through Antron's CPU rasterizer,
;;; in place of cl-vectors.lisp: runes/raster-antron loads this file instead
;;; of that one, and the rest of runes/raster is the same.

(defvar *glyph-raster-path* (antron:make-path)
  "Thread-reused scratch path for single glyph rasterization.")

(defvar *glyph-raster-context* nil
  "Thread-reused scratch CPU context for rasterizing paths into coverage callbacks.")

(defun ensure-glyph-raster-context (width height)
  "Ensure a CPU-CONTEXT of at least WIDTH x HEIGHT is available."
  (let ((w (max 64 width))
        (h (max 64 height)))
    (if (or (null *glyph-raster-context*)
            (< (antron:cpu-context-width *glyph-raster-context*) w)
            (< (antron:cpu-context-height *glyph-raster-context*) h))
        (let ((new-w (max w (if *glyph-raster-context* (* 2 (antron:cpu-context-width *glyph-raster-context*)) 256)))
              (new-h (max h (if *glyph-raster-context* (* 2 (antron:cpu-context-height *glyph-raster-context*)) 256))))
          (setf *glyph-raster-context* (antron:make-cpu-context new-w new-h)))
        *glyph-raster-context*)))

(defun render-alpha-map (dpi-x dpi-y font string bbox)
  "Rasterize STRING in FONT at DPI-X x DPI-Y into an alpha map covering
   BBOX (a pixel bounding box as returned by the bounding-box providers)
   using Antron.
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
          (let* ((units->pixels-x (float (font-units->pixels-x dpi-x font) 1.0f0))
                 (units->pixels-y (float (font-units->pixels-y dpi-y font) 1.0f0))
                 (array (make-array (list height width)
                                    :initial-element 0
                                    :element-type '(unsigned-byte 8)))
                 (path (antron:make-path))
                 (ctx (ensure-glyph-raster-context width height))
                 (thickness (* units->pixels-y (zpb-ttf:underline-thickness font-loader)))
                 (underline-offset (* units->pixels-y (zpb-ttf:underline-position font-loader)))
                 (fn (lambda (x y alpha)
                       (when (and (<= 0 x (1- width))
                                  (<= 0 y (1- height)))
                         (setf (aref array y x) alpha)))))
            (antron:paths-from-string font-loader string
                                      :into path
                                      :offset-x (float (- min-x) 1.0f0)
                                      :offset-y (float max-y 1.0f0)
                                      :scale-x units->pixels-x
                                      :scale-y (- units->pixels-y))
            (flet ((add-rect (x y w h)
                     (let ((rx (float x 1.0f0))
                           (ry (float y 1.0f0))
                           (rw (float w 1.0f0))
                           (rh (float h 1.0f0)))
                       (antron:path-move-to path rx ry)
                       (antron:path-line-to path (+ rx rw) ry)
                       (antron:path-line-to path (+ rx rw) (+ ry rh))
                       (antron:path-line-to path rx (+ ry rh))
                       (antron:path-close path))))
              (when (font-underline font)
                (add-rect 0 (- max-y underline-offset) width thickness))
              (when (font-strikethrough font)
                (let ((strike-offset (* 2 underline-offset)))
                  (add-rect 0 (+ max-y strike-offset) width thickness)))
              (when (font-overline font)
                ;; Above the ascender by the underline's distance below the
                ;; baseline. UNDERLINE-OFFSET is negative (TrueType puts the
                ;; underline below the baseline), and array y grows downward.
                (let* ((ascend (* units->pixels-y (zpb-ttf:ascender font-loader)))
                       (top (+ (- max-y ascend) underline-offset)))
                  (add-rect 0 (- top thickness) width thickness))))
            (if (font-antialias font)
                (antron:rasterize-path-coverage-callback ctx path fn)
                (antron:rasterize-path-binary-callback ctx path fn))
            (list array min-x max-y width height))))))

(defun rasterize-glyph (font glyph sx sy)
  "Rasterize the zpb-ttf GLYPH of FONT, scaled by SX, SY pixels per font
   unit, with its origin at a whole pixel using Antron.
   Call with the font loader held."
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
      (let ((path (antron:path-reset *glyph-raster-path*))
            (ctx (ensure-glyph-raster-context width height))
            (fn (lambda (x y alpha)
                  (when (and (< -1 x width) (< -1 y height))
                    (setf (aref data (+ (* y stride) x)) alpha)))))
        (antron:paths-from-glyph glyph
                                 :into path
                                 :offset-x (float (- x0) 1.0f0)
                                 :offset-y (float y1 1.0f0)
                                 :scale-x (float sx 1.0f0)
                                 :scale-y (float (- sy) 1.0f0))
        (if (font-antialias font)
            (antron:rasterize-path-coverage-callback ctx path fn)
            (antron:rasterize-path-binary-callback ctx path fn))))
    (%make-glyph-image :width width :height height :x (- x0) :y y1
                       :advance (* sx (zpb-ttf:advance-width glyph))
                       :data data)))
