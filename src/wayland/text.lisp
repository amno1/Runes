;;;; text.lisp — text blended into client-side pixels
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes/wayland)

;;; A client draws the same strings again and again, and laying one out
;;; takes the font loader's lock and exact arithmetic on every glyph, so
;;; each string's glyph run is kept: its glyph images, each at its pen
;;; position rounded to a whole pixel.

(defvar *max-glyph-runs* 4000
  "The most glyph runs kept.  Past it they are all dropped and laid out
   again when next drawn.")

(defvar *glyph-runs* (make-hash-table :test 'equal)
  "(DPI FONT . STRING) -> its glyph run.")

(defvar *glyph-runs-lock* (bt:make-lock "runes/wayland glyph runs"))

(defun glyph-run (font string dpi)
  "STRING laid out in FONT at DPI: a vector of (PEN-X . GLYPH-IMAGE), PEN-X
   the glyph's origin in whole pixels from the start of the line."
  (let ((key (list* dpi font string)))
    (or (bt:with-lock-held (*glyph-runs-lock*)
          (gethash key *glyph-runs*))
        (let ((run (map 'vector (lambda (entry)
                                  (destructuring-bind (index pen image) entry
                                    (declare (ignore index))
                                    (cons (round pen) image)))
                        (layout-glyphs font string dpi dpi))))
          (bt:with-lock-held (*glyph-runs-lock*)
            (when (>= (hash-table-count *glyph-runs*) *max-glyph-runs*)
              (clrhash *glyph-runs*))
            (setf (gethash (list* dpi font (copy-seq string)) *glyph-runs*) run))))))

(defun flush-glyph-runs (&optional font)
  "Forget the glyph runs of FONT, or of every font."
  (bt:with-lock-held (*glyph-runs-lock*)
    (if font
        (let ((stale '()))
          (maphash (lambda (key run)
                     (declare (ignore run))
                     (when (eq (second key) font)
                       (push key stale)))
                   *glyph-runs*)
          (dolist (key stale)
            (remhash key *glyph-runs*)))
        (clrhash *glyph-runs*))))

(defmethod flush-font-caches :after ((font font))
  (flush-glyph-runs font))

;;; Drawing

(defun %draw-text (font string x y dpi line-p blend fill)
  (when (plusp (length string))
    (loop for (pen . image) across (glyph-run font string dpi)
          when (plusp (glyph-image-width image))
            do (funcall blend
                        (- (+ x pen) (glyph-image-x image))
                        (- y (glyph-image-y image))
                        (glyph-image-width image)
                        (glyph-image-height image)
                        (glyph-image-data image)
                        (glyph-image-stride image)))
    (loop for (left top width height) in (font-decorations font string x y dpi dpi line-p)
          do (funcall fill left top width height))))

(defun draw-text (font string x y &key (dpi 96) blend fill)
  "Draw STRING in FONT at DPI with its baseline at (X, Y), in pixels.
   BLEND is called as (BLEND LEFT TOP WIDTH HEIGHT DATA STRIDE) for each
   glyph, to blend the text colour through DATA, an A8 alpha map with rows
   STRIDE bytes apart, whose top left goes at (LEFT, TOP).  FILL is called
   as (FILL LEFT TOP WIDTH HEIGHT) to paint each underline, strikethrough
   and overline, spanning the ink bounding box."
  (%draw-text font string x y dpi nil blend fill))

(defun draw-text-line (font string x y &key (dpi 96) blend fill)
  "Like DRAW-TEXT, but the decorations span the advance-width line box
   (see TEXT-LINE-BOUNDING-BOX), as text laid out character by character
   expects."
  (%draw-text font string x y dpi t blend fill))

;;; A plain pixel buffer: WIDTH x HEIGHT pixels #x00RRGGBB, row by row.

(defun blend-alpha-into-buffer (pixels width height left top w h data stride colour)
  "Blend COLOUR into PIXELS through DATA, a W x H alpha map with rows
   STRIDE bytes apart, its top left at (LEFT, TOP), clipped to the buffer."
  (declare (type (simple-array (unsigned-byte 32) (*)) pixels)
           (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum width height left top w h stride)
           (type (unsigned-byte 32) colour))
  (let ((r (ldb (byte 8 16) colour))
        (g (ldb (byte 8 8) colour))
        (b (ldb (byte 8 0) colour)))
    (loop for row from (max 0 (- top)) below (min h (- height top))
          for py = (+ top row)
          do (loop for col from (max 0 (- left)) below (min w (- width left))
                   for a = (aref data (+ (* row stride) col))
                   unless (zerop a)
                     do (let* ((i (+ (* py width) left col))
                               (d (aref pixels i))
                               (na (- 255 a)))
                          (flet ((mix (src dst) (floor (+ (* src a) (* dst na) 127) 255)))
                            (setf (aref pixels i)
                                  (logior (ash (mix r (ldb (byte 8 16) d)) 16)
                                          (ash (mix g (ldb (byte 8 8) d)) 8)
                                          (mix b (ldb (byte 8 0) d))))))))))

(defun fill-rectangle-in-buffer (pixels width height left top w h colour)
  "Paint the W x H rectangle at (LEFT, TOP) of PIXELS in COLOUR, clipped
   to the buffer."
  (declare (type (simple-array (unsigned-byte 32) (*)) pixels)
           (type fixnum width height left top w h))
  (loop for py from (max 0 top) below (min height (+ top h))
        for a = (max 0 left)
        for b = (min width (+ left w))
        when (< a b)
          do (fill pixels colour :start (+ (* py width) a) :end (+ (* py width) b))))

(defun draw-text-into-buffer (pixels width height font string x y colour
                              &key (dpi 96) line-p)
  "Draw STRING in FONT in COLOUR into PIXELS, a WIDTH x HEIGHT buffer of
   #x00RRGGBB pixels, with its baseline at (X, Y).  LINE-P spans the
   decorations across the line box, as DRAW-TEXT-LINE does."
  (%draw-text font string x y dpi line-p
              (lambda (left top w h data stride)
                (blend-alpha-into-buffer pixels width height left top w h data stride colour))
              (lambda (left top w h)
                (fill-rectangle-in-buffer pixels width height left top w h colour))))
