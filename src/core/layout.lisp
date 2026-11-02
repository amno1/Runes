;;;; layout.lisp — glyph positions along a line, and text decorations
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes)

;;; Where each glyph of a string goes, without drawing it: a renderer
;;; places its own image of each glyph at the pen positions given here.
;;; Pen positions are exact; a renderer that draws at whole pixels rounds
;;; each one, so a line never drifts from its measured width by more than
;;; half a pixel.

(defun layout-string (font string &key (dpi-x 96)
                                       (letter-spacing 0.0)
                                       (word-spacing 0.0)
                                       kerning-table)
  "Lay out STRING in FONT on one line at DPI-X, applying LETTER-SPACING,
   WORD-SPACING (added after each space), and KERNING-TABLE (or the
   font's kerning table).
   Returns (VALUES LAYOUT TOTAL-ADVANCE) where LAYOUT is a vector of
   (GLYPH-INDEX PEN-X), PEN-X in pixels from the start of the line."
  (with-font-loader (loader font)
    (let* ((sx (font-units->pixels-x dpi-x font))
           (len (length string))
           (layout (make-array len))
           (pen 0.0f0)
           (lsp (coerce letter-spacing 'single-float))
           (wsp (coerce word-spacing 'single-float))
           (previous-glyph nil)
           (previous-char nil))
      (flet ((advance (glyph char)
               (+ (* sx (coerce (zpb-ttf:advance-width glyph) 'single-float))
                  (if (and (not (zerop wsp)) (char= char #\Space))
                      wsp
                      0.0f0))))
        (loop for i from 0 below len
              for char = (char string i)
              for glyph = (zpb-ttf:find-glyph char loader)
              do (when previous-glyph
                   (let ((kern (if kerning-table
                                   (kern-offset font previous-char char
                                                :custom-table kerning-table
                                                :dpi-x dpi-x)
                                   (* sx (coerce (zpb-ttf:kerning-offset
                                                  previous-glyph glyph loader)
                                                 'single-float)))))
                     (incf pen (+ (advance previous-glyph previous-char)
                                  (coerce kern 'single-float)
                                  lsp))))
                 (setf (aref layout i) (list (zpb-ttf:font-index glyph) pen)
                       previous-glyph glyph
                       previous-char char))
        (values layout
                (if previous-glyph
                    (coerce (+ pen (advance previous-glyph previous-char)) 'single-float)
                    0.0f0))))))

(defun font-decorations (font string x y dpi-x dpi-y line-p)
  "The underline, strikethrough and overline of STRING in FONT drawn at
   baseline (X, Y), across the ink box or, when LINE-P, the line box:
   a list of rectangles (LEFT TOP WIDTH HEIGHT) in pixels, y down.  NIL
   when FONT has none."
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
