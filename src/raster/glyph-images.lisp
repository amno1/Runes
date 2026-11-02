;;;; glyph-images.lisp — glyphs rasterized one at a time, cached per face
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes/raster)

;;; Each glyph of a face, at one size and DPI, is rasterized once and kept
;;; in *GLYPH-IMAGES*.  A backend draws a string by laying it out with
;;; LAYOUT-GLYPHS and placing each glyph's image at its pen position: an
;;; X server through RENDER glyph sets, a client-side renderer by blending
;;; the images into its own pixels.
;;;
;;; Glyphs are placed at whole pixels: each glyph's origin is the pen
;;; position from LAYOUT-STRING, rounded.

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
   Guarded by RUNES's font-loader lock, which rasterizing needs anyway.")

(defun face-key (font dpi-x dpi-y)
  (list (font-source font) (font-size font) dpi-x dpi-y
        (and (font-antialias font) t)))

(defun layout-styled-glyphs (font string &key (dpi-x 96) (dpi-y 96)
                                              (letter-spacing 0.0)
                                              (word-spacing 0.0)
                                              kerning-table)
  "Lay out STRING in FONT with LAYOUT-STRING, and rasterize the glyphs not
   yet rasterized for this face.
   Returns (VALUES LAYOUT TOTAL-ADVANCE KEY)
   where LAYOUT is a vector of (GLYPH-INDEX PEN-X IMAGE)."
  (with-font-loader (loader font)
    (multiple-value-bind (positions total-advance)
        (layout-string font string :dpi-x dpi-x
                                   :letter-spacing letter-spacing
                                   :word-spacing word-spacing
                                   :kerning-table kerning-table)
      (let* ((key (face-key font dpi-x dpi-y))
             (images (or (gethash key *glyph-images*)
                         (progn
                           (when (>= (hash-table-count *glyph-images*) *max-glyph-faces*)
                             (clrhash *glyph-images*))
                           (setf (gethash key *glyph-images*) (make-hash-table)))))
             (sx (font-units->pixels-x dpi-x font))
             (sy (font-units->pixels-y dpi-y font)))
        (values (map 'vector
                     (lambda (entry)
                       (destructuring-bind (index pen) entry
                         (list index pen
                               (or (gethash index images)
                                   (setf (gethash index images)
                                         (rasterize-glyph font (zpb-ttf:index-glyph index loader)
                                                          sx sy))))))
                     positions)
                total-advance
                key)))))

(defun layout-glyphs (font string dpi-x dpi-y)
  "Lay out STRING in FONT: return a vector of (GLYPH-INDEX PEN-X IMAGE),
   PEN-X the exact pen position relative to the start, and the face key.
   Rasterizes the glyphs not yet rasterized for this face."
  (multiple-value-bind (layout advance key)
      (layout-styled-glyphs font string :dpi-x dpi-x :dpi-y dpi-y)
    (declare (ignore advance))
    (values layout key)))

