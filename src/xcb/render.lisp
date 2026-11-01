;;;; render.lisp — text drawing and alpha mask compositing for XCB
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:xcb-truetype)

;; xcb:put-image-chunked is more efficient implementation; in benchmark it is
;; comparable or as fast as clx-truetype's on small images and faster on
;; immediate and big sized images. In every case it conses less due to use of
;; displaced array instead of subseq and it does less round-trips to server due
;; to using maximum allowed chunk size by the server, 256 instead of 32.
(declaim (inline put-image-chunked))
(defun put-image-chunked (conn format drawable gc width height dst-x dst-y left-pad depth data)
  "Upload DATA to DRAWABLE via PUT-IMAGE, chunking by rows if necessary to stay
   well within X11's maximum request length (65535 4-byte words). DATA can be a
   1-D vector or a 2-D array of octets."
  (xcb:put-image-chunked conn format drawable gc width height dst-x dst-y left-pad depth data))

(defun put-alpha-mask (conn pixmap width height data &optional root)
  "Upload DATA (WIDTH*HEIGHT bytes, one byte per pixel, as a 1-D vector or
   a HEIGHT x WIDTH array) to PIXMAP at depth 8.  PUT-IMAGE requires each
   row padded to a 4-byte boundary; DATA is sent as-is when WIDTH already
   is a multiple of 4 and copied into a padded buffer otherwise."
  (let ((gc (mask-gc-for conn pixmap root)))
    (if (zerop (mod width 4))
        (put-image-chunked conn 2 pixmap gc width height 0 0 0 8 data)
        (let* ((padded-row (* 4 (ceiling width 4)))
               (padded     (make-array (* padded-row height)
                                       :element-type '(unsigned-byte 8)
                                       :initial-element 0))
               (flat       (if (= (array-rank data) 1)
                               data
                               (make-array (array-total-size data)
                                           :element-type (array-element-type data)
                                           :displaced-to data))))
          (dotimes (row height)
            (replace padded flat
                     :start1 (* row padded-row)
                     :start2 (* row width)
                     :end2   (* (1+ row) width)))
          (put-image-chunked conn 2 pixmap gc width height 0 0 0 8 padded)))))

(defun composite-alpha-mask (drawable alpha-data min-x max-y width height x y colour)
  "Composite ALPHA-DATA, a WIDTH x HEIGHT alpha mask whose origin is
   (MIN-X, MAX-Y) relative to the baseline, onto DRAWABLE at baseline
   (X, Y) in solid COLOUR.  The per-call mask pixmap and picture are freed
   even if uploading or compositing signals an error."
  (let* ((conn (drawable-connection drawable))
         (a8   (a8-format conn))
         (mask (xcb:generate-id conn))
         (mask-picture (xcb:generate-id conn))
         (picture-created-p nil))
    (flush-deferred-frees conn)
    ;; 1. Upload the alpha mask into a depth-8 pixmap.
    (xcb:create-pixmap conn 8 mask (drawable-id drawable) width height)
    (unwind-protect
         (progn
           (put-alpha-mask conn mask width height alpha-data
                           (xcb:root (drawable-screen drawable)))
           (create-picture conn mask-picture mask a8 0)
           (setf picture-created-p t)
           ;; 2. Composite the solid-fill source through the mask.  The
           ;;    destination and source pictures are cached and outlive
           ;;    this call.
           (composite conn
                      +pict-op-over+
                      (solid-fill-picture conn colour)
                      mask-picture
                      (drawable-picture-for drawable)
                      0 0                           ; src x, y
                      0 0                           ; mask x, y
                      (+ x min-x) (- y max-y)       ; dst x, y
                      width height))
      ;; 3. Free the per-call mask resources.  Freeing a picture that was
      ;;    never created would itself signal BadPicture, hence the flag.
      (when picture-created-p
        (ignore-errors (free-picture conn mask-picture)))
      (ignore-errors (xcb:free-pixmap conn mask)))))

(defun %draw-text (pixarray-function drawable font string x y colour)
  (multiple-value-bind (alpha-data min-x max-y width height)
      (funcall pixarray-function drawable font string)
    (unless (or (null alpha-data) (zerop width) (zerop height))
      (composite-alpha-mask drawable alpha-data min-x max-y width height
                            x y colour))))

(declaim (inline draw-text))
(defun draw-text (drawable font string x y &key (colour #xFFFFFF))
  "Draw STRING in FONT at baseline (X, Y) on DRAWABLE.  Decorations span
   the ink bounding box.  COLOUR is an #x00RRGGBB colour value.
   Antialiasing follows FONT-ANTIALIAS."
  (if *draw-with-glyph-sets*
      (%draw-glyphs drawable font string x y colour nil)
      (%draw-text #'text-pixarray-for-drawable drawable font string x y colour)))

(declaim (inline draw-text-line))
(defun draw-text-line (drawable font string x y &key (colour #xFFFFFF))
  "Draw STRING as a single line in FONT at baseline (X, Y) on DRAWABLE,
   using the advance-width line box (see TEXT-LINE-BOUNDING-BOX).
   COLOUR is an #x00RRGGBB colour value.  Antialiasing follows
   FONT-ANTIALIAS."
  (if *draw-with-glyph-sets*
      (%draw-glyphs drawable font string x y colour t)
      (%draw-text #'text-line-pixarray-for-drawable drawable font string x y colour)))
