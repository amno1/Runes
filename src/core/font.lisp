;;;; font.lisp — the font class: a face, a size and a text style
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes)

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
                           :documentation "Cache for text line bboxes"))
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
   (make-font-cache font dpi-cache-size string-cache-size 'text-line-bounding-box-provider)))

(defmethod (setf font-family) :before
  (family (instance font))
  (check-valid-font-families family (font-subfamily instance)))

(defmethod (setf font-subfamily) :before
  (subfamily (instance font))
  (check-valid-font-families (font-family instance) subfamily))

(defgeneric flush-font-caches (font)
  (:documentation "Flush everything cached for FONT.  Runes flushes its
   bounding boxes; a renderer that caches images per font adds an :AFTER
   method to flush those too.")
  (:method ((font font))
    (setf (slot-value font 'units/em) nil)
    (cacle:cache-flush (font-string-bboxes font))
    (cacle:cache-flush (font-string-line-bboxes font))))

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
