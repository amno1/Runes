;;;; glyph.lisp — Glyph objects, metrics, and contour extraction
;;;; Copyright (c) 2006 Zachary Beane, 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

(in-package #:zpb-ttf)

(defclass control-point ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (on-curve-p :initarg :on-curve-p :reader on-curve-p)))

(defun make-control-point (x y on-curve-p)
  (make-instance 'control-point
                 :x x
                 :y y
                 :on-curve-p on-curve-p))

(defmethod print-object ((control-point control-point) stream)
  (print-unreadable-object (control-point stream :type t)
    (format stream "~D,~D~:[~;*~]"
            (x control-point) (y control-point) (on-curve-p control-point))))

;;; Contour iteration macros

(defmacro do-contour-segments* ((p1 p2) contour &body body)
  (let ((length (gensym))
        (i (gensym))
        (stack (gensym))
        (next (gensym))
        (next-point (gensym "NEXT-POINT"))
        (midpoint (gensym "MIDPOINT"))
        (contour* (gensym))
        (loop (gensym "LOOP"))
        (body-tag (gensym "BODY"))
        (done-tag (gensym "DONE"))
        (mid p1)
        (end p2))
    `(let* ((,i 1)
            (,contour* ,contour)
            (,length (length ,contour*))
            ,stack ,next ,mid ,end)
       (unless (zerop ,length)
         (unless (on-curve-p (aref ,contour* 0))
           (setf ,stack (aref ,contour* 0)))
         (flet ((,next-point ()
                  (when (< ,i ,length)
                    (prog1 (aref ,contour* ,i) (incf ,i))))
                (,midpoint (p0 p1)
                  (make-control-point (/ (+ (x p0) (x p1)) 2)
                                      (/ (+ (y p0) (y p1)) 2)
                                      t)))
           (tagbody
              ,loop
              (setf ,mid nil
                    ,next (,next-point))
              (unless ,next
                (setf ,mid ,stack
                      ,end (aref ,contour* 0))
                (cond
                  ((on-curve-p ,end)
                   (go ,body-tag))
                  (,stack
                   (setf ,mid ,stack
                         ,end (,midpoint ,stack ,end))
                   (go ,body-tag))
                  (t (go ,done-tag))))
              (if (on-curve-p ,next)
                  (setf ,end ,next
                        ,mid ,stack
                        ,stack nil)
                  (cond (,stack
                         (setf ,mid ,stack
                               ,end (,midpoint ,stack ,next)
                               ,stack ,next))
                        (t
                         (setf ,stack ,next)
                         (go ,loop))))
              ,body-tag
              ,@body
              (when ,next
                (go ,loop))
              ,done-tag))))))

(defun start-of-contour (contour)
  (let ((first (aref contour 0)))
   (if (on-curve-p first)
       first
       (let ((last (aref contour (1- (length contour)))))
         (if (on-curve-p last)
             last
             (make-control-point (/ (+ (x first) (x last)) 2)
                                 (/ (+ (y first) (y last)) 2)
                                 t))))))

(defmacro do-contour-segments ((p0 p1 p2) contour &body body)
  "A contour is made up of segments. A segment may be a straight line
or a curve. For each segment, bind the P0 and P2 variables to the
start and end points of the segment. If the segment is a curve, set P1
to the control point of the curve, otherwise set P1 to NIL."
  (let ((start p0)
        (contour* (gensym "CONTOUR")))
    `(let ((,contour* ,contour))
       (when (plusp (length ,contour*))
         (let ((,start (start-of-contour ,contour*)))
           (do-contour-segments* (,p1 ,p2)
               ,contour*
             (progn ,@body)
             (setf ,start ,p2)))))))

(defun explicit-contour-points (contour)
  (let ((new-contour (make-array (length contour)
                                 :adjustable t
                                 :fill-pointer 0)))
    (when (and (plusp (length contour))
               (on-curve-p (aref contour 0)))
      (vector-push-extend (aref contour 0) new-contour))
    (do-contour-segments* (p1 p2)
        contour
      (when p1
        (vector-push-extend p1 new-contour))
      (unless (eql p2 (aref contour 0))
        (vector-push-extend p2 new-contour)))
    new-contour))

(defun explicit-contours (glyph)
  (map 'vector #'explicit-contour-points (contours glyph)))

(defparameter *empty-contours*
  (make-array 0 :element-type t))

(defparameter *empty-bounding-box*
  (make-array 4 :initial-element 0 :element-type '(signed-byte 16)))

(defun empty-bounding-box ()
  (copy-seq *empty-bounding-box*))

(defun empty-contours ()
  (copy-seq *empty-contours*))

;;; Maximum component nesting depth (cf. maxComponentDepth in 'maxp').
(defparameter *max-compound-glyph-depth* 16)

(defvar *compound-glyph-stack* '()
  "Glyph indices of the compound glyphs whose contours are being read.")

;;; The Glyph class

(defclass glyph ()
  ((font-loader
    :initarg :font-loader
    :reader font-loader)
   (font-index
    :initarg :font-index
    :accessor font-index)
   (code-point
    :initarg :code-point
    :accessor code-point)
   (contours
    :initarg :contours
    :accessor contours)
   ;; Exact outline of a CFF glyph, from which CONTOURS are approximated.
   (cubic-contours
    :initarg :cubic-contours
    :accessor %cubic-contours)
   ;; Phantom points of a TrueType glyph at the loader's instance.
   (phantom-points
    :initarg :phantom-points
    :accessor %phantom-points)
   (bounding-box
    :initarg :bounding-box
    :accessor bounding-box)))

(defmethod print-object ((glyph glyph) stream)
  (print-unreadable-object (glyph stream :type t :identity nil)
    (format stream "GID=~D U+~4,'0X"
            (font-index glyph)
            (code-point glyph))))

;;; Glyph metrics

(defun varied-phantom-points (glyph)
  "GLYPH's phantom points at its loader's instance, or NIL when the font has
no TrueType glyph variations."
  (when (table-info "gvar" (font-loader glyph))
    (contours glyph)
    (%phantom-points glyph)))

(defgeneric left-side-bearing (object)
  (:method ((glyph glyph))
    (let* ((loader (font-loader glyph))
           (gid (font-index glyph))
           (lsb (bounded-aref (left-side-bearings loader) gid)))
      (if (null (normalized-coordinates loader))
          lsb
          (let ((delta (lsb-delta loader gid))
                (phantom (varied-phantom-points glyph)))
            (cond (delta (+ lsb delta))
                  ;; The varied outline's left edge from the first phantom point.
                  (phantom (- (xmin glyph) (car (aref phantom 0))))
                  ;; A CFF2 glyph's side bearing is its left edge.
                  ((table-info "CFF2" loader) (xmin glyph))
                  (t lsb)))))))

(defgeneric advance-width (object)
  (:method ((glyph glyph))
    (let ((loader (font-loader glyph)))
      (cond
        ((null (normalized-coordinates loader))
         (bounded-aref (advance-widths loader) (font-index glyph)))
        ;; Without HVAR, a TrueType instance takes its advance from the
        ;; varied phantom points.
        ((and (table-info "gvar" loader) (not (table-info "HVAR" loader)))
         (let ((phantom (varied-phantom-points glyph)))
           (- (car (aref phantom 1)) (car (aref phantom 0)))))
        (t
         (+ (bounded-aref (advance-widths loader) (font-index glyph))
            (advance-width-delta loader (font-index glyph))))))))

(defgeneric top-side-bearing (object)
  (:method ((glyph glyph))
    (let ((loader (font-loader glyph))
          (gid (font-index glyph)))
      (if (vmtx-missing-p loader)
          (- (ascender loader) (ymax glyph))
          (let ((tsb (bounded-aref (top-side-bearings loader) gid)))
            (if (null (normalized-coordinates loader))
                tsb
                (let ((delta (tsb-delta loader gid))
                      (phantom (varied-phantom-points glyph)))
                  (cond (delta (+ tsb delta))
                        (phantom (- (cdr (aref phantom 2)) (ymax glyph)))
                        (t tsb)))))))))

(defgeneric advance-height (object)
  (:method ((glyph glyph))
    (let* ((loader (font-loader glyph))
           (gid (font-index glyph))
           (advance (bounded-aref (advance-heights loader) gid)))
      (cond
        ((or (null (normalized-coordinates loader)) (vmtx-missing-p loader))
         advance)
        ((and (table-info "gvar" loader) (not (table-info "VVAR" loader)))
         (let ((phantom (varied-phantom-points glyph)))
           (- (cdr (aref phantom 2)) (cdr (aref phantom 3)))))
        (t
         (+ advance (advance-height-delta loader gid)))))))

(defgeneric right-side-bearing (object)
  (:method ((glyph glyph))
    (- (advance-width glyph)
       (- (+ (left-side-bearing glyph) (xmax glyph))
          (xmin glyph)))))

(defgeneric kerning-offset (left right loader))

(defmethod kerning-offset ((left-glyph glyph) (right-glyph glyph)
                           (font-loader font-loader))
  (let ((key (logior (ash (font-index left-glyph) 16)
                     (font-index right-glyph))))
    (gethash key (kerning-table font-loader) 0)))

(defmethod kerning-offset ((left character) (right character)
                           (font-loader font-loader))
  (kerning-offset (find-glyph left font-loader)
                  (find-glyph right font-loader)
                  font-loader))

(defmethod kerning-offset ((left null) right font-loader)
  (declare (ignore left right font-loader))
  0)

(defmethod kerning-offset (left (right null) font-loader)
  (declare (ignore left right font-loader))
  0)

(defgeneric kerned-advance-width (object next)
  (:method ((object glyph) next)
    (+ (advance-width object)
       (kerning-offset object next (font-loader object)))))

(defgeneric contour-count (object)
  (:method (object)
    (length (contours object))))

(defgeneric contour (object index)
  (:method (object index)
    (aref (contours object) index)))

(defmacro do-contours ((contour object &optional result) &body body)
  (let ((i (gensym))
        (obj (gensym)))
    `(let ((,obj ,object))
       (dotimes (,i (contour-count ,obj) ,result)
         (let ((,contour (contour ,obj ,i)))
           ,@body)))))

(defgeneric string-bounding-box (string loader &key kerning)
  (:method (string (loader font-loader) &key (kerning t))
    (if (zerop (length string))
        (empty-bounding-box)
        (let* ((g1 (find-glyph (char string 0) loader))
               (bbox (copy-seq (bounding-box g1)))
               (x 0))
          (loop for i from 1 below (length string)
                for g2 = (find-glyph (char string i) loader)
                do (incf x (advance-width g1))
                   (when kerning
                     (incf x (kerning-offset g1 g2 loader)))
                   (let ((gb (bounding-box g2)))
                     (setf (xmin bbox) (min (xmin bbox) (+ x (xmin gb)))
                           (ymin bbox) (min (ymin bbox) (ymin gb))
                           (xmax bbox) (max (xmax bbox) (+ x (xmax gb)))
                           (ymax bbox) (max (ymax bbox) (ymax gb))))
                   (setf g1 g2))
          bbox))))

;;; Zero-copy TrueType contour extraction

(defun glyph-loca-info (loader gid)
  (let ((loca (table-info "loca" loader)))
    (if (null loca)
        (values 0 0)
        (let ((fd (font-data loader))
              (loc-off (offset loca))
              (is-short (eql (loca-offset-format loader) :short)))
          (if is-short
              (let ((off1 (* (zpb-otf:font-data-read-u16 fd (+ loc-off (* gid 2))) 2))
                    (off2 (* (zpb-otf:font-data-read-u16 fd (+ loc-off (* (1+ gid) 2))) 2)))
                (values off1 (- off2 off1)))
              (let ((off1 (zpb-otf:font-data-read-u32 fd (+ loc-off (* gid 4))))
                    (off2 (zpb-otf:font-data-read-u32 fd (+ loc-off (* (1+ gid) 4)))))
                (values off1 (- off2 off1))))))))

(defmethod initialize-bounding-box ((glyph glyph))
  (when (cff-outlines-p (font-loader glyph))
    (return-from initialize-bounding-box (initialize-cff-glyph glyph)))
  ;; At an instance, the outline decides; glyf records the default's bounds.
  (when (loader-gvar-coords (font-loader glyph))
    (return-from initialize-bounding-box
      (setf (bounding-box glyph) (contours-bounding-box (contours glyph)))))
  (let ((loader (font-loader glyph))
        (gid (font-index glyph)))
    (multiple-value-bind (g-off len) (glyph-loca-info loader gid)
      (if (or (zerop len) (null (table-info "glyf" loader)))
          (setf (bounding-box glyph) (empty-bounding-box))
          (let* ((fd (font-data loader))
                 (pos (+ (offset (table-info "glyf" loader)) g-off))
                 (xmin (zpb-otf:font-data-read-i16 fd (+ pos 2)))
                 (ymin (zpb-otf:font-data-read-i16 fd (+ pos 4)))
                 (xmax (zpb-otf:font-data-read-i16 fd (+ pos 6)))
                 (ymax (zpb-otf:font-data-read-i16 fd (+ pos 8))))
            (setf (bounding-box glyph) (vector xmin ymin xmax ymax)))))))

(defmethod initialize-contours ((glyph glyph))
  (when (cff-outlines-p (font-loader glyph))
    (return-from initialize-contours (initialize-cff-glyph glyph)))
  (multiple-value-bind (points phantom) (glyph-outline-points (font-loader glyph) (font-index glyph))
    (setf (contours glyph) (outline-points->contours points)
          (%phantom-points glyph) phantom)))

(defmethod bounding-box :before ((glyph glyph))
  (unless (slot-boundp glyph 'bounding-box)
    (initialize-bounding-box glyph)))

(defmethod code-point :before ((glyph glyph))
  (unless (slot-boundp glyph 'code-point)
    (setf (code-point glyph)
          (font-index-code-point (font-index glyph) (font-loader glyph)))))

(defmethod contours :before ((glyph glyph))
  (unless (slot-boundp glyph 'contours)
    (initialize-contours glyph)))
