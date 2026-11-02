;;;; metrics.lisp — font metrics calculations and bounding boxes
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes)

(declaim (ftype (function (t) t)
                font-string-bboxes
                font-string-line-bboxes
                font-underline
                font-overline))

;;; ZPB-TTF font objects cache
(defun font-source (font)
  "Where FONT is read from: its pathname, or (PATHNAME . INDEX) for a font
   in a collection.  NIL if FONT is not in *FONT-CACHE*."
  (bt:with-recursive-lock-held (*font-cache-lock*)
    (let ((subfamilies (gethash (font-family font) *font-cache*)))
      (and subfamilies (gethash (font-subfamily font) subfamilies)))))

(defun get-font-pathname (font)
  "The pathname of the file FONT is read from, or NIL.  See FONT-SOURCE
   for which font of a collection it is."
  (let ((source (font-source font)))
    (if (consp source) (car source) source)))

(defvar *font-loader-cache* (make-hash-table :test 'equal)
  "Caches opened ZPB-TTF font loader objects keyed by FONT-SOURCE.
   Retains one open file handle per font file for the lifetime of the image
   to avoid repeated disk I/O and font header parsing on every text layout.
   Call CLOSE-FONT-LOADERS to explicitly close all open loaders and clear
   the cache.")

(defvar *font-loader-lock*
  (bt:make-recursive-lock "truetype font loaders")
  "Guards *FONT-LOADER-CACHE* and every use of a loader in it.  A
   ZPB-TTF loader reads glyphs from its open file as they are needed, so
   two threads using one loader at once would interleave their reads.
   Recursive: the metric functions nest.")

(defun close-font-loaders ()
  "Close all cached font loaders and flush *FONT-LOADER-CACHE*."
  (bt:with-recursive-lock-held (*font-loader-lock*)
    (maphash (lambda (path loader)
               (declare (ignore path))
               (ignore-errors (zpb-ttf:close-font-loader loader)))
             *font-loader-cache*)
    (clrhash *font-loader-cache*)))

(defmacro with-font-loader ((loader font) &body body)
  "Run BODY with LOADER bound to FONT's cached ZPB-TTF loader, opening it
   the first time, holding *FONT-LOADER-LOCK*."
  (let ((exists-p (gensym))
        (source (gensym)))
    `(bt:with-recursive-lock-held (*font-loader-lock*)
       (let ((,source (font-source ,font)))
         (multiple-value-bind (,loader ,exists-p)
             (gethash ,source *font-loader-cache*)
           (unless ,exists-p
             (setf ,loader (setf (gethash ,source *font-loader-cache*)
                                 (open-font-source ,source))))
           ,@body)))))

(defun open-font-source (source)
  "Open a font loader for SOURCE, as FONT-SOURCE returns it."
  (if (consp source)
      (zpb-ttf:open-font-loader (car source) :collection-index (cdr source))
      (zpb-ttf:open-font-loader source)))

(defun font-cache-fetch (cache dpi string)
  (cacle:cache-fetch (cacle:cache-fetch cache dpi) string))

(defgeneric target-dpi (target)
  (:documentation "Return (VALUES DPI-X DPI-Y) for TARGET.

   TARGET can be an integer DPI or a cons (DPI-X . DPI-Y); a backend adds
   methods for its own drawables and screens.  Anything else is 96 DPI.")
  (:method ((target cons)) (values (car target) (cdr target)))
  (:method ((target integer)) (values target target))
  (:method ((target t)) (values 96 96)))

;;; Font metrics
(defun %font-units/em (font)
  ;; Read from the font file once; the metric functions need it on every
  ;; call, and it would otherwise take the font-loader lock each time.
  (or (slot-value font 'units/em)
      (setf (slot-value font 'units/em)
            (with-font-loader (loader font)
              (zpb-ttf:units/em loader)))))

(defun font-units->pixels-x (dpi-x font)
  "px = funits*coeff. Function returns coeff."
  (/ (* (font-size font) dpi-x) (* 72 (%font-units/em font))))

(defun font-units->pixels-y (dpi-y font)
  "px = funits*coeff. Function returns coeff."
  (/ (* (font-size font) dpi-y) (* 72 (%font-units/em font))))

(defun font-ascent-for-dpi (dpi-y font)
  (with-font-loader (loader font)
    (ceiling (* (font-units->pixels-y dpi-y font)
                (zpb-ttf:ascender loader)))))

(defun font-descent-for-dpi (dpi-y font)
  (with-font-loader (loader font)
    (floor (* (font-units->pixels-y dpi-y font)
              (zpb-ttf:descender loader)))))

(defun font-ascent (drawable font)
  "Returns ascent of FONT.

   DRAWABLE can be a window, a pixmap, a screen, an integer DPI
   or a cons (DPI-X . DPI-Y)."
  (multiple-value-bind (dpi-x dpi-y) (target-dpi drawable)
    (declare (ignore dpi-x))
    (font-ascent-for-dpi dpi-y font)))

(defun font-descent (drawable font)
  "Returns descent of FONT.

   DRAWABLE can be a window, a pixmap, a screen, an integer DPI
   or a cons (DPI-X . DPI-Y)."
  (multiple-value-bind (dpi-x dpi-y) (target-dpi drawable)
    (declare (ignore dpi-x))
    (font-descent-for-dpi dpi-y font)))

(defun font-line-gap (drawable font)
  "Returns line gap of FONT.

   DRAWABLE can be a window, a pixmap, a screen, an integer DPI
   or a cons (DPI-X . DPI-Y)."
  (with-font-loader (loader font)
    (multiple-value-bind (dpi-x dpi-y) (target-dpi drawable)
      (declare (ignore dpi-x))
      (ceiling (* (font-units->pixels-y dpi-y font)
                  (zpb-ttf:line-gap loader))))))

;;; baseline-to-baseline = ascent - descent + line gap
(defun baseline-to-baseline (drawable font)
  "Returns distance between baselines of FONT."
  (+ (font-ascent drawable font) (- (font-descent drawable font))
     (font-line-gap drawable font)))

(defun text-bounding-box-provider (dpi-x dpi-y font string)
  "Internal: cache provider for FONT-STRING-BBOXES."
  (with-font-loader (loader font)
    (let* ((bbox
             (zpb-ttf:string-bounding-box string loader))
           (units->pixels-x (font-units->pixels-x dpi-x font))
           (units->pixels-y (font-units->pixels-y dpi-y font))
           (xmin (zpb-ttf:xmin bbox))
           (ymin (zpb-ttf:ymin bbox))
           (xmax (zpb-ttf:xmax bbox))
           (ymax (zpb-ttf:ymax bbox)))
      (when (font-underline font)
        (setf ymin (min ymin (- (zpb-ttf:underline-position loader)
                                (zpb-ttf:underline-thickness loader)))))
      (when (font-overline font)
        ;; The overline's top: above the ascender by the underline's
        ;; distance below the baseline (UNDERLINE-POSITION is negative),
        ;; plus its thickness.  See RENDER-ALPHA-MAP.
        (setf ymax (max ymax (+ (zpb-ttf:ascender loader)
                                (- (zpb-ttf:underline-position loader))
                                (zpb-ttf:underline-thickness loader)))))
      (vector (floor (* xmin
                        units->pixels-x))
              (floor (* ymin
                        units->pixels-y))
              (ceiling (* xmax
                          units->pixels-x))
              (ceiling (* ymax
                          units->pixels-y))))))

(defun text-bounding-box (drawable font string &key start end)
  "Returns text bounding box.

   DRAWABLE can be a window, a pixmap, a screen, an integer DPI
   or a cons (DPI-X . DPI-Y)."
  (when (or start end)
    (setf string (subseq string (or start 0) end)))
  (multiple-value-bind (dpi-x dpi-y) (target-dpi drawable)
    (font-cache-fetch (font-string-bboxes font)
                      (cons dpi-x dpi-y)
                      string)))

(defun text-width (drawable font string &key start end)
  "Returns width of text bounding box.

   DRAWABLE can be a window, a pixmap, a screen, an integer DPI
   or a cons (DPI-X . DPI-Y)."
  (when (or start end)
    (setf string (subseq string (or start 0) end)))
  (let ((bbox (text-bounding-box drawable font string)))
    (- (xmax bbox) (xmin bbox))))

(defun text-height (drawable font string &key start end)
  "Returns height of text bounding box.

   DRAWABLE can be a window, a pixmap, a screen, an integer DPI
   or a cons (DPI-X . DPI-Y)."
  (when (or start end)
    (setf string (subseq string (or start 0) end)))
  (let ((bbox (text-bounding-box drawable font string)))
    (- (ymax bbox) (ymin bbox))))

(defvar *allow-fixed-pitch-p* t
  "When true (the default), the width of a line in a font marked
   fixed-pitch is the first glyph's advance times the length, without
   looking at the other glyphs.  Faster, but wrong for fonts whose CJK
   glyphs are double width despite the fixed-pitch flag: bind it to NIL
   for those.")

(defun text-line-bounding-box-provider (dpi-x dpi-y font string)
  "Internal: cache provider for FONT-STRING-LINE-BBOXES."
  (with-font-loader (loader font)
    (let* ((units->pixels-x (font-units->pixels-x dpi-x font))
           (xmin 0)
           (ymin (font-descent-for-dpi dpi-y font))
           (ymax (font-ascent-for-dpi dpi-y font))
           (string-length (length string))
           (previous (and (> string-length 0)
                          (zpb-ttf:find-glyph (char string 0) loader)))
           (xmax (if previous (zpb-ttf:advance-width previous) 0)))
      (if (and *allow-fixed-pitch-p* (zpb-ttf:fixed-pitch-p loader))
          (setf xmax (* xmax string-length))
          ;; Keep the previous glyph: KERNING-OFFSET takes glyphs, so each
          ;; character is looked up once rather than twice.
          (loop for i from 1 below string-length
                for glyph = (zpb-ttf:find-glyph (char string i) loader)
                do (incf xmax (+ (zpb-ttf:advance-width glyph)
                                 (zpb-ttf:kerning-offset previous glyph loader)))
                   (setf previous glyph)))
      (vector (floor (* xmin units->pixels-x))
              ymin
              (ceiling (* xmax
                          units->pixels-x))
              ymax))))

(defun text-line-bounding-box (drawable font string &key start end)
  "Returns text line bounding box.

   DRAWABLE can be a window, a pixmap, a screen, an integer DPI
   or a cons (DPI-X . DPI-Y)."
  (when (or start end)
    (setf string (subseq string (or start 0) end)))
  (multiple-value-bind (dpi-x dpi-y) (target-dpi drawable)
    (font-cache-fetch (font-string-line-bboxes font)
                      (cons dpi-x dpi-y)
                      string)))

(defun text-line-width (drawable font string &key start end)
  "Returns width of text line bounding box.

   DRAWABLE can be a window, a pixmap, a screen, an integer DPI
   or a cons (DPI-X . DPI-Y)."
  (when (or start end)
    (setf string (subseq string (or start 0) end)))
  (let ((bbox (text-line-bounding-box drawable font string)))
    (- (xmax bbox) (xmin bbox))))

(defun text-line-height (drawable font string &key start end)
  "Returns height of text line bounding box.

   DRAWABLE can be a window, a pixmap, a screen, an integer DPI
   or a cons (DPI-X . DPI-Y)."
  (when (or start end)
    (setf string (subseq string (or start 0) end)))
  (let ((bbox (text-line-bounding-box drawable font string)))
    (- (ymax bbox) (ymin bbox))))

(defun xmin (bounding-box)
  "Returns left side x of BOUNDING-BOX"
  (if (vectorp bounding-box)
      (elt bounding-box 0)
      (error "xmin: not a bounding box: ~S" bounding-box)))

(defun ymin (bounding-box)
  "Returns bottom side y of BOUNDING-BOX"
  (if (vectorp bounding-box)
      (elt bounding-box 1)
      (error "ymin: not a bounding box: ~S" bounding-box)))

(defun xmax (bounding-box)
  "Returns right side x of BOUNDING-BOX"
  (if (vectorp bounding-box)
      (elt bounding-box 2)
      (error "xmax: not a bounding box: ~S" bounding-box)))

(defun ymax (bounding-box)
  "Returns top side y of BOUNDING-BOX"
  (if (vectorp bounding-box)
      (elt bounding-box 3)
      (error "ymax: not a bounding box: ~S" bounding-box)))

(defun font-lines-height (drawable font lines-count)
  "Returns text lines height in pixels.

   For one line height is ascender+descender. For more than one
   line height is ascender+descender+linegap."
  (if (> lines-count 0)
      (+ (+ (font-ascent drawable font)
            (- (font-descent drawable font)))
         (* (1- lines-count) (+ (font-ascent drawable font)
                                (- (font-descent drawable font))
                                (font-line-gap drawable font))))
      0))


(defun fit-font-size (drawable family subfamily string box-width box-height
                      &key (min-size 8) (probe-size 100))
  "Return the pixel size at which STRING's text-line bounding box in
   FAMILY/SUBFAMILY fits inside BOX-WIDTH x BOX-HEIGHT, as large as
   possible without exceeding either dimension, and at least MIN-SIZE.
   Like FIT-FONT, but returns just the size, for a caller that already
   has a font to resize (a redraw loop, say) and would otherwise make
   and discard one per call."
  (let* ((probe (make-instance 'font
                               :family family
                               :subfamily subfamily
                               :size probe-size
                               ;; Measured once and discarded: don't pay
                               ;; for full-size caches.
                               :dpi-cache-size 1
                               :string-cache-size 1))
         (pw (max 1 (text-line-width  drawable probe string)))
         (ph (max 1 (text-line-height drawable probe string)))
         (by-width  (floor (* probe-size box-width)  pw))
         (by-height (floor (* probe-size box-height) ph)))
    (max min-size (min by-width by-height))))

(defun fit-font (drawable family subfamily string box-width box-height
                 &key (min-size 8) (probe-size 100))
  "Return a FONT of FAMILY/SUBFAMILY sized so that STRING's text-line
   bounding box fits inside BOX-WIDTH x BOX-HEIGHT, as large as
   possible without exceeding either dimension.  DRAWABLE supplies the
   DPI the measurement is taken against, so the result matches what
   DRAW-TEXT-LINE will compute.  Returns a font of at least MIN-SIZE.
   See FIT-FONT-SIZE to get the size without a new font."
  (make-instance 'font :family family :subfamily subfamily
                       :size (fit-font-size drawable family subfamily string
                                            box-width box-height
                                            :min-size min-size
                                            :probe-size probe-size)))

(defun text-center (drawable font string box-width box-height)
  "Return (VALUES BASELINE-X BASELINE-Y) that horizontally and
   vertically centre STRING when rendered in FONT in a BOX-WIDTH x
   BOX-HEIGHT box.  Pass the values directly to DRAW-TEXT-LINE's X and
   Y arguments.

   The line box runs from (baseline - ascent) to (baseline - descent)
   in screen coordinates; descent is negative in TrueType convention,
   so the box height is (ascent - descent) and the baseline that
   centres it is (box-height - line-height)/2 + ascent."
  (let* ((tw      (text-line-width  drawable font string))
         (ascent  (font-ascent  drawable font))
         (descent (font-descent drawable font))
         (line-h  (- ascent descent)))
    (values (round (- box-width  tw)     2)
            (+ (round (- box-height line-h) 2) ascent))))


(defun line-box (font dpi &key (multiplier 1.0) explicit-height)
  "Return (VALUES ASCENT DESCENT HEIGHT LEADING) in pixels for FONT at DPI.
   ASCENT and DESCENT are positive pixel values. HEIGHT applies MULTIPLIER
   or EXPLICIT-HEIGHT."
  (multiple-value-bind (dpi-x dpi-y) (target-dpi dpi)
    (declare (ignore dpi-x))
    (let* ((ascent (coerce (font-ascent-for-dpi dpi-y font) 'single-float))
           (descent (abs (coerce (font-descent-for-dpi dpi-y font) 'single-float)))
           (gap (coerce (or (ignore-errors (font-line-gap dpi font)) 0) 'single-float))
           (natural-height (+ ascent descent))
           (lh (if explicit-height
                   (coerce explicit-height 'single-float)
                   (* natural-height (coerce multiplier 'single-float)))))
      (values ascent descent lh gap))))

(defun lookup-custom-kerning (table char1 char2)
  "Lookup kerning offset between CHAR1 and CHAR2 in custom kerning TABLE."
  (when table
    (cond
      ((hash-table-p table)
       (or (gethash (cons char1 char2) table)
           (gethash (coerce (vector char1 char2) 'string) table)))
      ((consp table)
       (let ((pair (cons char1 char2))
             (str (coerce (vector char1 char2) 'string)))
         (or (cdr (assoc pair table :test #'equal))
             (cdr (assoc str table :test #'string=))))))))

(defun kern-offset (font char1 char2 &key custom-table (dpi-x 96))
  "Calculate kerning offset in fractional pixels between CHAR1 and CHAR2 for FONT.
   Consults CUSTOM-TABLE if present; otherwise queries the font loader's kern table via zpb-ttf."
  (when custom-table
    (let ((val (lookup-custom-kerning custom-table char1 char2)))
      (when val
        (return-from kern-offset (coerce val 'single-float)))))
  (with-font-loader (loader font)
    (let ((g1 (zpb-ttf:find-glyph char1 loader))
          (g2 (zpb-ttf:find-glyph char2 loader)))
      (if (and g1 g2)
          (let ((offset (zpb-ttf:kerning-offset g1 g2 loader)))
            (if (and offset (not (zerop offset)))
                (* (coerce offset 'single-float)
                   (coerce (font-units->pixels-x dpi-x font) 'single-float))
                0.0f0))
          0.0f0))))
