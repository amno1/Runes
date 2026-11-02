;;;; glyphs.lisp — text drawn through RENDER glyph sets
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes/xcb)

;;; How text reaches the server.
;;;
;;; Each glyph of a face, at one size and DPI, is rasterized once on the
;;; client (*GLYPH-IMAGES*, shared by every connection) and uploaded once
;;; per connection into a RENDER glyph set.  Drawing a string is then one
;;; CompositeGlyphs16 request carrying a few bytes per character, however
;;; often the string is drawn: the way Xft draws.  Only glyphs a string is
;;; the first to use are uploaded with it.
;;;
;;; A glyph set belongs to a connection, not to a screen, so one serves
;;; every screen.  The server frees a connection's glyph sets when it
;;; closes; a connection keeps at most *MAX-GLYPH-SETS* of them, freeing
;;; the least recently used when it needs another.

(defvar *draw-with-glyph-sets* t
  "When true (the default), DRAW-TEXT and DRAW-TEXT-LINE draw through RENDER
   glyph sets.  When NIL they upload the whole string's alpha mask on every
   call, as clx-truetype did.")

(defvar *max-glyph-sets* 64
  "The most glyph sets -- one per face, size, DPI and antialiasing -- a
   connection keeps.")

(defconstant +glyphs-per-request+ 2000
  "Glyphs sent in one CompositeGlyphs16 or AddGlyphs request at most,
   keeping each well within the X server's request length limit.")

;;; Glyph sets on a connection

(defstruct (glyph-set (:constructor %make-glyph-set (id)))
  (id 0)
  (uploaded (make-hash-table) :type hash-table)
  (last-use 0))

(defvar *glyph-sets* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq)
  "Maps a connection to a hash table of face key -> GLYPH-SET.  Weak on
   the connection, like the other per-connection caches.")

(defvar *glyph-sets-lock* (bt:make-recursive-lock "xcb-truetype glyph sets")
  "Guards *GLYPH-SETS*, and makes deciding which glyphs to upload, uploading
   them and compositing with them one step, so that two threads drawing
   through one connection cannot each upload the same glyph.")

(defvar *glyph-set-clock* 0)

(defun %glyph-set-for (conn key)
  (let* ((sets (or (gethash conn *glyph-sets*)
                   (setf (gethash conn *glyph-sets*) (make-hash-table :test 'equal))))
         (set (gethash key sets)))
    (unless set
      (when (>= (hash-table-count sets) *max-glyph-sets*)
        (let ((oldest-key nil) (oldest nil))
          (maphash (lambda (k s)
                     (when (or (null oldest) (< (glyph-set-last-use s) (glyph-set-last-use oldest)))
                       (setf oldest-key k oldest s)))
                   sets)
          (ignore-errors (xcb-render:free-glyph-set conn (glyph-set-id oldest)))
          (remhash oldest-key sets)))
      (setf set (%make-glyph-set (xcb:generate-id conn)))
      (xcb-render:create-glyph-set conn (glyph-set-id set) (a8-format conn))
      (setf (gethash key sets) set))
    (setf (glyph-set-last-use set) (incf *glyph-set-clock*))
    set))

(defun %upload-glyphs (conn set layout)
  "Upload to SET the glyphs of LAYOUT it does not hold yet."
  (let ((uploaded (glyph-set-uploaded set))
        (pending '()))
    (loop for (index nil image) across layout
          unless (gethash index uploaded)
            do (setf (gethash index uploaded) t)
               (push (cons index image) pending))
    (loop while pending
          do (let ((batch '()))
               (loop repeat +glyphs-per-request+ while pending
                     do (push (pop pending) batch))
               (xcb-render:add-glyphs
                conn (glyph-set-id set)
                (mapcar #'car batch)
                (mapcar (lambda (entry)
                          (let ((image (cdr entry)))
                            (make-instance 'xcb-render:glyphinfo
                                           :width (glyph-image-width image)
                                           :height (glyph-image-height image)
                                           :x (glyph-image-x image)
                                           :y (glyph-image-y image)
                                           :x-off (round (glyph-image-advance image))
                                           :y-off 0)))
                        batch)
                (let ((data (make-array (reduce #'+ batch :key (lambda (e) (length (glyph-image-data (cdr e)))))
                                        :element-type '(unsigned-byte 8)))
                      (at 0))
                  (dolist (entry batch data)
                    (let ((d (glyph-image-data (cdr entry))))
                      (replace data d :start1 at)
                      (incf at (length d))))))))))

(defun %glyph-commands (conn layout start end x y)
  "Encode the glyphs of LAYOUT from START below END as CompositeGlyphs16
   elements, the first glyph's origin at (X + its pen position, Y).  Each
   element is a run of glyphs that each start where the one before ended
   -- its origin plus its rounded advance -- and gives the offset to its
   first glyph from where the previous run ended."
  (let ((out (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (msb (= (xcb:buffer-byte-order (xcb:conn-send-buffer conn)) xcb:+msb+))
        (pen-x 0) (pen-y 0)              ; where the server's pen is
        (run-start nil) (run-length 0))
    (labels ((byte8 (n) (vector-push-extend (ldb (byte 8 0) n) out))
             (card16 (n)
               (let ((n (logand n #xffff)))
                 (if msb
                     (progn (byte8 (ash n -8)) (byte8 n))
                     (progn (byte8 n) (byte8 (ash n -8))))))
             (close-run ()
               (when run-start
                 (setf (aref out run-start) run-length)
                 (loop until (zerop (mod (fill-pointer out) 4)) do (byte8 0))
                 (setf run-start nil))))
      (loop for i from start below end
            for (index pen image) = (aref layout i)
            for gx = (+ x (round pen))
            do (unless (and run-start (= gx pen-x) (= y pen-y) (< run-length 254))
                 (close-run)
                 (setf run-start (fill-pointer out) run-length 0)
                 (byte8 0) (byte8 0) (byte8 0) (byte8 0) ; length, filled in later; pad
                 (card16 (- gx pen-x))
                 (card16 (- y pen-y))
                 (setf pen-x gx pen-y y))
               (card16 index)
               (incf run-length)
               (incf pen-x (round (glyph-image-advance image))))
      (close-run))
    out))

(defun %draw-decorations (drawable font x y colour dpi-x dpi-y line-p string)
  "Draw FONT's underline, strikethrough and overline for STRING drawn at
   baseline (X, Y), where RENDER-ALPHA-MAP draws them."
  (let ((bars (font-decorations font string x y dpi-x dpi-y line-p)))
    (when bars
      (let* ((conn (drawable-connection drawable))
             (src (solid-fill-picture conn colour))
             (dst (drawable-picture-for drawable)))
        (loop for (left top width height) in bars
              do (composite conn +pict-op-over+ src 0 dst
                            0 0 0 0 left top width height))))))

(defun %draw-glyphs (drawable font string x y colour line-p)
  "Draw STRING in FONT at baseline (X, Y) on DRAWABLE through a glyph set."
  (let ((conn (drawable-connection drawable)))
    (flush-deferred-frees conn)
    (multiple-value-bind (dpi-x dpi-y) (screen-dpi (drawable-screen drawable))
      (when (plusp (length string))
        (multiple-value-bind (layout key) (layout-glyphs font string dpi-x dpi-y)
          (bt:with-recursive-lock-held (*glyph-sets-lock*)
            (let ((set (%glyph-set-for conn key))
                  (src (solid-fill-picture conn colour))
                  (dst (drawable-picture-for drawable))
                  (mask-format (a8-format conn)))
              (%upload-glyphs conn set layout)
              ;; A8 as the mask format: the glyphs are added into one
              ;; mask before compositing, so where two glyphs' edges
              ;; overlap they are not drawn twice.
              (loop for start from 0 below (length layout) by +glyphs-per-request+
                    do (xcb-render:composite-glyphs16
                        conn +pict-op-over+ src dst mask-format (glyph-set-id set) 0 0
                        (%glyph-commands conn layout start
                                         (min (length layout) (+ start +glyphs-per-request+))
                                         x y)))))))
      (%draw-decorations drawable font x y colour dpi-x dpi-y line-p string))))
