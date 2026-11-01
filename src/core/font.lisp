;;;; raster.lisp — font class and TrueType rasterization via cl-vectors and cl-aa
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:truetype)

(defclass font ()
  ((family    :type string
              :initarg :family
              :accessor font-family
              :documentation "Font family.")
   (subfamily :type string
              :initarg :subfamily
              :accessor font-subfamily
              :documentation "Font subfamily (e.g. Regular, Bold).")
   (size      :type real
              :initarg :size
              :accessor font-size
              :documentation "Font size in pixels.")
   (underline :type boolean
              :initarg :underline
              :initform nil
              :accessor font-underline
              :documentation "Draw underline under text string.")
   (strikethrough :type boolean
                  :initarg :strikethrough
                  :initform nil
                  :accessor font-strikethrough
                  :documentation "Draw strike through text string.")
   (overline   :type boolean
               :initarg :overline
               :initform nil :accessor font-overline
               :documentation "Draw line over text string.")
   (background :initarg :background
               :initform nil
               :accessor font-background
               :documentation "Background color.")
   (foreground :initarg :foreground
               :initform nil
               :accessor font-foreground
               :documentation "Foreground color.")
   (overwrite-gcontext :type boolean
                       :initarg :overwrite-gcontext
                       :initform nil 
                       :accessor font-overwrite-gcontext
                       :documentation "Use font values for background and foreground colors.")
   (antialias     :type boolean
                  :initarg :antialias
                  :initform t
                  :accessor font-antialias
                  :documentation "Antialias text string.")
   (units/em  :initform nil
              :documentation "The font file's units per em, read on first use
              by FONT-UNITS->PIXELS-X and -Y, and forgotten when the font's
              caches are flushed.")
   (string-bboxes :type cacle:cache
                  :accessor font-string-bboxes
                  :documentation "Cache for text bboxes")
   (string-line-bboxes     :type cacle:cache
                           :accessor font-string-line-bboxes
                           :documentation "Cache for text line bboxes")
   (string-alpha-maps      :type cacle:cache
                           :accessor font-string-alpha-maps
                           :documentation "Cache for text alpha maps")
   (string-line-alpha-maps :type cacle:cache
                           :accessor font-string-line-alpha-maps
                           :documentation "Cache for text line alpha maps"))
  (:documentation "Class for representing font information."))

(defun check-valid-font-families (family subfamily)
  (unless (plusp (hash-table-count *font-cache*))
    (cache-fonts))
  (unless (gethash family *font-cache*)
    (error "Font family not found: ~S" family))
  (unless (gethash subfamily (gethash family *font-cache*))
    (error "Font subfamily not found: ~S in family ~S (available: ~{~S~^, ~})"
           subfamily family (get-font-subfamilies family))))

(defmethod initialize-instance :around
    ((instance font) &rest initargs &key family subfamily &allow-other-keys)
  ;; A missing or NIL :SUBFAMILY picks Regular/Book/first available, so
  ;; callers such as FIT-FONT can pass NIL through to mean "default".
  (unless (plusp (hash-table-count *font-cache*))
    (cache-fonts))
  (unless (gethash family *font-cache*)
    (error "Font family not found: ~S" family))
  (let* ((effective-subfamily
           (if (null subfamily)
               (let ((subs (get-font-subfamilies family)))
                 (or (find "Regular" subs :test #'string-equal)
                     (find "Book"    subs :test #'string-equal)
                     (first subs)))
               subfamily))
         (stripped (loop for (k v) on initargs by #'cddr
                         unless (eq k :subfamily)
                           append (list k v))))
    (apply #'call-next-method instance :subfamily effective-subfamily stripped)))

(defmethod initialize-instance :before 
    ((instance font) &rest initargs &key family subfamily &allow-other-keys)
  (declare (ignorable initargs))
  (check-valid-font-families family subfamily))

(defun make-font-cache (font dpi-cache-size string-cache-size inner-provider)
  (flet ((outer-provider (dpi-cons)
           (values
            (cacle:make-cache
             string-cache-size
             (lambda (string)
               (values
                (funcall inner-provider (car dpi-cons) (cdr dpi-cons) font string)
                (length string)))
             :test #'equal
             :policy :lfu)
            1)))
    (cacle:make-cache
     dpi-cache-size
     #'outer-provider
     :test #'equal
     :policy :lfu)))

(defmethod initialize-instance :after
    ((font font) &key (dpi-cache-size 10) (string-cache-size 1000) &allow-other-keys)
  (setf
   (font-string-bboxes font)
   (make-font-cache font dpi-cache-size string-cache-size 'text-bounding-box-provider)
   (font-string-line-bboxes font)
   (make-font-cache font dpi-cache-size string-cache-size 'text-line-bounding-box-provider)
   (font-string-alpha-maps font)
   (make-font-cache font dpi-cache-size string-cache-size 'text-pixarray-provider)
   (font-string-line-alpha-maps font)
   (make-font-cache font dpi-cache-size string-cache-size 'text-line-pixarray-provider)))

(defmethod (setf font-family) :before
  (family (instance font))
  (check-valid-font-families family (font-subfamily instance)))

(defmethod (setf font-subfamily) :before
  (subfamily (instance font))
  (check-valid-font-families (font-family instance) subfamily))

(defun flush-font-caches (font)
  "Flush all cached bounding boxes and alpha maps for FONT."
  (setf (slot-value font 'units/em) nil)
  (dolist (accessor (list #'font-string-bboxes
                          #'font-string-line-bboxes
                          #'font-string-alpha-maps
                          #'font-string-line-alpha-maps))
    (cacle:cache-flush (funcall accessor font))))

(defmethod (setf font-family) :after (family (font font))
  (flush-font-caches font))

(defmethod (setf font-subfamily) :after (subfamily (font font))
  (flush-font-caches font))

(defmethod (setf font-size) :after (value (font font))
  (flush-font-caches font))

(defmethod (setf font-underline) :after (value (font font))
  (flush-font-caches font))

(defmethod (setf font-overline) :after (value (font font))
  (flush-font-caches font))

(defmethod (setf font-strikethrough) :after (value (font font))
  (flush-font-caches font))

(defmethod (setf font-antialias) :after (value (font font))
  (flush-font-caches font))

(defgeneric font-equal (font1 font2)
  (:documentation "Returns t if two font objects are equal, else returns nil.")
  (:method ((font1 font) (font2 font))
    (and (string-equal (font-family font1)
                       (font-family font2))
         (string-equal (font-subfamily font1)
                       (font-subfamily font2))
         (= (font-size font1) (font-size font2))
         (eql (font-underline font1) (font-underline font2))
         (eql (font-strikethrough font1) (font-strikethrough font2))
         (eql (font-overline font1) (font-overline font2))
         (equal (font-background font1) (font-background font2))
         (equal (font-foreground font1) (font-foreground font2))
         (eql (font-overwrite-gcontext font1) (font-overwrite-gcontext font2))
         (eql (font-antialias font1) (font-antialias font2)))))

(defmethod print-object ((instance font) stream)
  "Pretty printing font object"
  (with-slots (family subfamily size underline strikethrough
                   overline background foreground overwrite-gcontext
                   antialias)
      instance
    (if *print-readably*
        (format stream
                "#.(~S '~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S)"
                'cl:make-instance 'font
                :family family :subfamily subfamily :size size :underline underline 
                :strikethrough strikethrough
                :overline overline :background background :foreground foreground 
                :overwrite-gcontext overwrite-gcontext
                :antialias antialias)
        (format stream
                "#<'~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S>"
                'font
                :family family :subfamily subfamily :size size :underline underline 
                :strikethrough strikethrough
                :overline overline :background background :foreground foreground 
                :overwrite-gcontext overwrite-gcontext
                :antialias antialias))))

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
