;;;; glyph-images.lisp — glyphs rasterized one at a time, and string layout
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:truetype)

;;; Each glyph of a face, at one size and DPI, is rasterized once and kept
;;; in *GLYPH-IMAGES*.  A backend draws a string by laying it out with
;;; LAYOUT-GLYPHS and placing each glyph's image at its pen position: an
;;; X server through RENDER glyph sets, a client-side renderer by blending
;;; the images into its own pixels.
;;;
;;; Glyphs are placed at whole pixels: each glyph's origin is the pen
;;; position, accumulated exactly from advances and kerning, rounded.  So
;;; a line never drifts from its measured width by more than half a pixel.

(defvar *max-glyph-faces* 256
  "The most faces, at one size and DPI each, whose rasterized glyphs are
   kept on the client.  Past it they are dropped and rasterized again when
   next used.")

(defstruct (glyph-image (:constructor %make-glyph-image))
  "One glyph rasterized for one face: an A8 image with rows padded to 4
   bytes, the image's origin (X, Y) relative to its top left, and the
   advance in pixels."
  (width   0 :type fixnum)
  (height  0 :type fixnum)
  (x       0 :type fixnum)
  (y       0 :type fixnum)
  (advance 0 :type real)
  (data    nil))

(defun glyph-image-stride (image)
  "Bytes per row of IMAGE's data: its width rounded up to 4."
  (* 4 (ceiling (glyph-image-width image) 4)))

(defvar *glyph-images* (make-hash-table :test 'equal)
  "Face key (see %FACE-KEY) -> hash table of glyph index -> GLYPH-IMAGE.
   Guarded by *FONT-LOADER-LOCK*, which rasterizing needs anyway.")

(defun face-key (font dpi-x dpi-y)
  (list (get-font-pathname font) (font-size font) dpi-x dpi-y
        (and (font-antialias font) t)))

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

(defun layout-glyphs (font string dpi-x dpi-y)
  "Lay out STRING in FONT: return a vector of (GLYPH-INDEX PEN-X IMAGE),
   PEN-X the exact pen position relative to the start, and the face key.
   Rasterizes the glyphs not yet rasterized for this face."
  (with-font-loader (loader font)
    (let* ((key (face-key font dpi-x dpi-y))
           (images (or (gethash key *glyph-images*)
                       (progn
                         (when (>= (hash-table-count *glyph-images*) *max-glyph-faces*)
                           (clrhash *glyph-images*))
                         (setf (gethash key *glyph-images*) (make-hash-table)))))
           (sx (font-units->pixels-x dpi-x font))
           (sy (font-units->pixels-y dpi-y font))
           (layout (make-array (length string)))
           (pen 0)
           (previous nil))
      ;; As PATHS-TTF:PATHS-FROM-STRING lays out a string, so a line is as
      ;; wide as TEXT-LINE-WIDTH says.
      (loop for char across string
            for i from 0
            for glyph = (zpb-ttf:find-glyph char loader)
            for index = (zpb-ttf:font-index glyph)
            do (when previous
                 (incf pen (* sx (+ (zpb-ttf:advance-width previous)
                                    (zpb-ttf:kerning-offset previous glyph loader)))))
               (setf (aref layout i)
                     (list index pen
                           (or (gethash index images)
                               (setf (gethash index images)
                                     (rasterize-glyph font glyph sx sy)))))
               (setf previous glyph))
      (values layout key))))


(defun font-decorations (font string x y dpi-x dpi-y line-p)
  "The underline, strikethrough and overline of STRING in FONT drawn at
   baseline (X, Y), across the ink box or, when LINE-P, the line box:
   a list of rectangles (LEFT TOP WIDTH HEIGHT) in pixels, y down, where
   RENDER-ALPHA-MAP draws them.  NIL when FONT has none."
  (when (or (font-underline font) (font-strikethrough font) (font-overline font))
    (let* ((bbox (if line-p
                     (font-cache-fetch (font-string-line-bboxes font) (cons dpi-x dpi-y) string)
                     (font-cache-fetch (font-string-bboxes font) (cons dpi-x dpi-y) string)))
           (left (+ x (xmin bbox)))
           (width (- (xmax bbox) (xmin bbox))))
      (when (plusp width)
        (multiple-value-bind (thickness underline-offset ascend)
            (with-font-loader (loader font)
              (let ((sy (font-units->pixels-y dpi-y font)))
                (values (* sy (zpb-ttf:underline-thickness loader))
                        (* sy (zpb-ttf:underline-position loader))
                        (* sy (zpb-ttf:ascender loader)))))
          (let ((height (max 1 (round thickness)))
                (bars '()))
            (flet ((bar (top) (push (list left (round top) width height) bars)))
              ;; UNDERLINE-OFFSET is negative: TrueType puts the underline
              ;; below the baseline, and screen y grows downward.
              (when (font-underline font)
                (bar (- y underline-offset)))
              (when (font-strikethrough font)
                (bar (+ y (* 2 underline-offset))))
              (when (font-overline font)
                (bar (- (+ (- y ascend) underline-offset) thickness))))
            (nreverse bars)))))))
