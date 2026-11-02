;;;; gvar.lisp — Glyph point deltas from the 'gvar' table
;;;; Copyright (c) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

;;; A port of Fontations' read-fonts: the glyph variation data of
;;; src/tables/gvar.rs, the tuple scalars, packed point numbers and packed
;;; deltas of src/tables/variations.rs, and the delta accumulation and IUP
;;; inference of src/tables/gvar/deltas.rs, which follow FreeType.
;;;
;;; Deltas are 16.16 fixed point values. COORDS are normalized coordinates
;;; as raw 2.14 integers, one per axis.

(in-package #:zpb-ttf)

(defun f+ (a b) (runes/otf/charstring::fixed+ a b))
(defun f- (a b) (runes/otf/charstring::fixed- a b))
(defun f* (a b) (runes/otf/charstring::fixed* a b))
(defun f/ (a b) (runes/otf/charstring::fixed/ a b))
(defun fixed-from-int (i) (runes/otf/charstring:fixed-from-int i))
(defun fixed-round (f) (ash (+ f #x8000) -16))

(define-condition gvar-data-error (error) ()
  (:documentation "Malformed glyph variation data; the glyph gets no deltas."))

(defun gvar-fail () (error 'gvar-data-error))

;;; Reading bytes

(defstruct (gvar-cursor (:conc-name gc-)) fd pos end)

(defun gc-u8 (c)
  (when (>= (gc-pos c) (gc-end c)) (gvar-fail))
  (prog1 (runes/otf:font-data-read-u8 (gc-fd c) (gc-pos c)) (incf (gc-pos c))))

(defun gc-int (c bytes signed)
  (when (> (+ (gc-pos c) bytes) (gc-end c)) (gvar-fail))
  (let ((v 0))
    (dotimes (i bytes)
      (setf v (logior (ash v 8) (runes/otf:font-data-read-u8 (gc-fd c) (+ (gc-pos c) i)))))
    (incf (gc-pos c) bytes)
    (if (and signed (logbitp (1- (* 8 bytes)) v)) (- v (ash 1 (* 8 bytes))) v)))

;;; Glyph variation data (gvar.rs)

(defstruct glyph-var-data
  fd axis-count
  shared-tuples shared-tuple-count       ; position and count of the shared tuple records
  tuple-count
  headers                                ; position of the first tuple variation header
  serialized serialized-end              ; tuple data, after any shared point numbers
  shared-points)                         ; :all, a vector of point numbers, or NIL

(defun gvar-glyph-data (loader gid)
  "The variation data for GID, or NIL if the glyph has none."
  (let ((info (table-info "gvar" loader)))
    (when info
      (let* ((fd (font-data loader))
             (gvar (runes/otf:read-gvar fd (offset info) (size info))))
        (when (< gid (runes/otf:gvar-glyph-count gvar))
          (flet ((data-position (i)
                   (+ (offset info)
                      (runes/otf:gvar-glyph-variation-data-array-offset gvar)
                      (runes/otf:gvar-glyph-variation-data-offset-ref gvar i))))
            (let ((start (data-position gid))
                  (stop (data-position (1+ gid))))
              (when (and (< start stop) (<= stop (+ (offset info) (size info))))
                (read-glyph-var-data fd gvar start stop)))))))))

(defun read-glyph-var-data (fd gvar start stop)
  "The GlyphVariationData table from START to STOP of the 'gvar' table GVAR."
  (let* ((count (runes/otf:font-data-read-u16 fd start))
         (serialized (+ start (runes/otf:font-data-read-u16 fd (+ start 2))))
         (data (make-glyph-var-data
                :fd fd
                :axis-count (runes/otf:gvar-axis-count gvar)
                :shared-tuples (+ (runes/otf::table-ref-offset gvar)
                                  (runes/otf:gvar-shared-tuples-offset-offset gvar))
                :shared-tuple-count (runes/otf:gvar-shared-tuple-count gvar)
                :tuple-count (logand count #x0FFF)
                :headers (+ start 4)
                :serialized serialized :serialized-end stop)))
    ;; Shared point numbers come first in the serialized data.
    (when (logbitp 15 count)
      (let ((c (make-gvar-cursor :fd fd :pos serialized :end stop)))
        (setf (glyph-var-data-shared-points data) (read-packed-points c)
              (glyph-var-data-serialized data) (gc-pos c))))
    data))

;;; Packed point numbers and deltas (variations.rs)

(defun read-packed-points (c)
  "Point numbers at cursor C: :ALL, or a vector. C ends after them."
  (let* ((b0 (gc-u8 c))
         (count (cond ((zerop b0) 0)
                      ((< b0 128) b0)
                      (t (logand (logior (ash b0 8) (gc-u8 c))
                                 #x7FFF)))))
    (if (zerop count)
        :all
        (loop with last = 0
              with seen = 0
              with points = (make-array count :fill-pointer 0)
              while (< seen count)
              ;; One run: a control byte, then RUN values.  Whole runs are
              ;; consumed, even one that runs past the count.
              do (loop with control = (gc-u8 c)
                       with size = (if (logbitp 7 control) 2 1)
                       with run = (1+ (logand control #x7F))
                       for i below run
                       for v = (gc-int c size nil)
                       when (< (fill-pointer points) count)
                         do (setf last (ldb (byte 16 0) (+ last v)))
                            (vector-push last points)
                       finally (incf seen run))
              finally (return points)))))

(defun read-delta-run (c)
  "The next run at C, as two values: the count and the byte width (0 for zeros)."
  (let ((control (gc-u8 c)))
    (values (1+ (logand control #x3F))
            (ecase (ash control -6) (0 1) (1 2) (2 0) (3 4)))))

(defun read-run-value (c width)
  (if (zerop width) 0 (gc-int c width t)))

(defun read-dense (c count fn)
  "Call FN with each index below COUNT and its delta; runs must fit exactly."
  (let ((cur 0))
    (loop while (< cur count)
          do (multiple-value-bind (run width) (read-delta-run c)
               (when (> (+ cur run) count) (gvar-fail))
               (dotimes (i run)
                 (funcall fn (+ cur i) (read-run-value c width)))
               (incf cur run)))))

(defun read-sparse (c points count fn)
  "Call FN with each of the COUNT POINTS and its delta, as read_sparse_deltas does."
  (let ((cur 0) (next 0))
    (loop while (< cur count)
          do (multiple-value-bind (run width) (read-delta-run c)
               (dotimes (i run)
                 (cond ((< next (length points))
                        (funcall fn (aref points next) (read-run-value c width))
                        (incf next))
                       ;; A zero run needs a point for every value.
                       ((zerop width) (gvar-fail))))
               (incf cur run)))))

;;; Tuples

(defun read-tuple (fd pos axis-count)
  "The AXIS-COUNT 2.14 values at POS, as a vector."
  (let ((tuple (make-array axis-count)))
    (dotimes (i axis-count tuple)
      (setf (svref tuple i) (runes/otf:font-data-read-i16 fd (+ pos (* 2 i)))))))

(defun gvar-tuple-scalar (peak intermediate coords)
  "Scalar of a tuple at COORDS, or NIL when it does not apply (scalar_for).
Each axis multiplies the scalar by how far COORDS lies towards its peak."
  (flet ((coord (i) (if (< i (length coords)) (aref coords i) 0))
         (scale (scalar numerator denominator)
           ;; From 2.14 to 16.16 for the division.
           (runes/otf/charstring::fixed-mul-div scalar (ash numerator 2) (ash denominator 2))))
    (loop with scalar = #x10000
          for i below (length peak)
          for p = (aref peak i)
          for coord = (coord i)
          do (cond
               ;; No peak on this axis, or exactly at it: a factor of one.
               ((or (zerop p) (= p coord)))
               ((zerop coord) (return nil))
               ((null intermediate)
                (if (<= (min p 0) coord (max p 0))
                    (setf scalar (scale scalar coord p))
                    (return nil)))
               (t
                (let ((start (aref (car intermediate) i))
                      (end (aref (cdr intermediate) i)))
                  (cond
                    ;; An invalid region contributes a factor of one.
                    ((or (> start p) (> p end) (and (minusp start) (plusp end))))
                    ((or (<= coord start) (>= coord end)) (return nil))
                    ((< coord p) (setf scalar (scale scalar (- coord start) (- p start))))
                    (t (setf scalar (scale scalar (- end coord) (- end p))))))))
          finally (return (unless (zerop scalar) scalar)))))

(defun map-active-tuples (data coords fn)
  "Call FN with (scalar points data-cursor) for each tuple that applies at
COORDS; POINTS is :ALL or a vector, and the cursor is at the tuple's deltas."
  (let* ((fd (glyph-var-data-fd data))
         (axis-count (glyph-var-data-axis-count data))
         (hpos (glyph-var-data-headers data))
         (dpos (glyph-var-data-serialized data))
         (dend (glyph-var-data-serialized-end data)))
    (dotimes (i (glyph-var-data-tuple-count data))
      (let* ((size (runes/otf:font-data-read-u16 fd hpos))
             (index (runes/otf:font-data-read-u16 fd (+ hpos 2)))
             (p (+ hpos 4))
             (peak (if (logbitp 15 index)
                       (prog1 (read-tuple fd p axis-count) (incf p (* 2 axis-count)))
                       (let ((shared (logand index #x0FFF)))
                         (when (< shared (glyph-var-data-shared-tuple-count data))
                           (read-tuple fd (+ (glyph-var-data-shared-tuples data) (* shared 2 axis-count))
                                       axis-count)))))
             (intermediate (when (logbitp 14 index)
                             (prog1 (cons (read-tuple fd p axis-count)
                                          (read-tuple fd (+ p (* 2 axis-count)) axis-count))
                               (incf p (* 4 axis-count)))))
             (start dpos))
        (setf hpos p
              dpos (+ dpos size))
        ;; Tuple data past the end of the glyph's data ends the iteration.
        (when (> dpos dend) (return))
        (let ((scalar (and peak (gvar-tuple-scalar peak intermediate coords))))
          (when scalar
            (let ((c (make-gvar-cursor :fd fd :pos start :end dpos)))
              (funcall fn scalar
                       (if (logbitp 13 index)
                           (read-packed-points c)
                           (or (glyph-var-data-shared-points data) (gvar-fail)))
                       c))))))))

(defun scaled-delta (delta scalar)
  (if (= scalar #x10000)
      (fixed-from-int delta)
      (f* (fixed-from-int delta) scalar)))

;;; Simple glyphs (gvar/deltas.rs)

(defun interpolate-untouched (xs ys out-x out-y touched start end)
  "Infer deltas for the untouched points of the contour START..END (IUP)."
  (let ((referenced (loop for i from start to end when (aref touched i) collect i)))
    (when referenced
      (labels ((interp (from to ref1 ref2)
                 (when (< from to)
                   (interp-coord xs out-x from to ref1 ref2)
                   (interp-coord ys out-y from to ref1 ref2)))
               (interp-coord (in out from to ref1 ref2)
                 (multiple-value-bind (lo hi)
                     (if (> (aref in ref1) (aref in ref2)) (values ref2 ref1) (values ref1 ref2))
                   (let ((in1 (aref in lo)) (in2 (aref in hi))
                         (out1 (aref out lo)) (out2 (aref out hi)))
                     ;; References sharing a coordinate but moved apart leave it alone.
                     (when (or (/= in1 in2) (= out1 out2))
                       (let ((scale (if (/= in1 in2) (f/ (f- out2 out1) (f- in2 in1)) 0))
                             (d1 (f- out1 in1))
                             (d2 (f- out2 in2)))
                         (loop for i from from below to
                               do (let ((coord (aref in i)))
                                    (setf (aref out i)
                                          (cond ((<= coord in1) (f+ coord d1))
                                                ((>= coord in2) (f+ coord d2))
                                                (t (f+ out1 (f* (f- coord in1) scale)))))))))))))
        (let ((first (first referenced)) (last (first referenced)))
          (dolist (next (rest referenced))
            (interp (1+ last) next last next)
            (setf last next))
          (if (= last first)
              ;; A single reference carries the whole contour with it.
              (let ((dx (f- (aref out-x first) (aref xs first)))
                    (dy (f- (aref out-y first) (aref ys first))))
                (unless (and (zerop dx) (zerop dy))
                  (loop for i from start to end
                        unless (= i first)
                          do (setf (aref out-x i) (f+ (aref out-x i) dx)
                                   (aref out-y i) (f+ (aref out-y i) dy)))))
              (progn
                (interp (1+ last) (1+ end) last first)
                (interp start first last first))))))))

(defun simple-glyph-deltas (data coords xs ys ends)
  "16.16 deltas for every point of a simple glyph, phantom points included,
as two vectors; XS and YS are the font unit coordinates of all its points
and ENDS its contour end points. NIL if the data is malformed."
  (let* ((n (length xs))
         (dx (make-array n :initial-element 0))
         (dy (make-array n :initial-element 0))
         (fx (map 'vector #'fixed-from-int xs))
         (fy (map 'vector #'fixed-from-int ys)))
    (handler-case
        (progn
          (map-active-tuples
           data coords
           (lambda (scalar points c)
             (if (eq points :all)
                 (progn
                   (read-dense c n (lambda (i d) (setf (aref dx i) (f+ (aref dx i) (scaled-delta d scalar)))))
                   (read-dense c n (lambda (i d) (setf (aref dy i) (f+ (aref dy i) (scaled-delta d scalar))))))
                 (let ((out-x (copy-seq fx)) (out-y (copy-seq fy))
                       (touched (make-array n :initial-element nil))
                       (count (length points)))
                   (read-sparse c points count
                                (lambda (i d)
                                  (when (< i n)
                                    (setf (aref out-x i) (f+ (aref out-x i) (scaled-delta d scalar))
                                          (aref touched i) t))))
                   (read-sparse c points count
                                (lambda (i d)
                                  (when (< i n)
                                    (setf (aref out-y i) (f+ (aref out-y i) (scaled-delta d scalar))))))
                   (let ((start 0))
                     (dolist (end ends)
                       ;; A contour ending before the previous one is skipped.
                       (when (>= end start)
                         (when (>= end n) (gvar-fail))
                         (interpolate-untouched fx fy out-x out-y touched start end)
                         (setf start (1+ end)))))
                   (dotimes (i n)
                     (setf (aref dx i) (f+ (aref dx i) (f- (aref out-x i) (aref fx i)))
                           (aref dy i) (f+ (aref dy i) (f- (aref out-y i) (aref fy i)))))))))
          (values dx dy))
      (gvar-data-error () nil))))

;;; Composite glyphs

(defun composite-glyph-deltas (data coords count)
  "16.16 deltas for the COUNT components and phantom points of a composite
glyph, as two vectors; NIL if the data is malformed."
  (let ((dx (make-array count :initial-element 0))
        (dy (make-array count :initial-element 0)))
    (handler-case
        (progn
          (map-active-tuples
           data coords
           (lambda (scalar points c)
             (multiple-value-bind (positions values)
                 (if (eq points :all)
                     ;; Every delta in the data: x deltas, then as many y deltas.
                     (let ((all (read-all-deltas c)))
                       (values (loop for i below (floor (length all) 2) collect i) all))
                     (values (coerce points 'list) (read-n-deltas c (* 2 (length points)))))
               (let ((half (length positions)))
                 (loop for position in positions
                       for i from 0
                       when (and (< position count) (< (+ half i) (length values)))
                         do (setf (aref dx position) (f+ (aref dx position) (f* (fixed-from-int (aref values i)) scalar))
                                  (aref dy position) (f+ (aref dy position)
                                                         (f* (fixed-from-int (aref values (+ half i))) scalar))))))))
          (values dx dy))
      (gvar-data-error () nil))))

(defun read-n-deltas (c limit)
  "Up to LIMIT deltas from C, fewer if the data ends."
  (let ((out (make-array limit :fill-pointer 0)))
    (handler-case
        (loop while (< (fill-pointer out) limit)
              do (multiple-value-bind (run width) (read-delta-run c)
                   (dotimes (i run)
                     (let ((v (read-run-value c width)))
                       (when (< (fill-pointer out) limit) (vector-push v out))))))
      (gvar-data-error () nil))
    out))

(defun read-all-deltas (c)
  (let ((out (make-array 16 :adjustable t :fill-pointer 0)))
    (handler-case
        (loop while (< (gc-pos c) (gc-end c))
              do (multiple-value-bind (run width) (read-delta-run c)
                   (dotimes (i run) (vector-push-extend (read-run-value c width) out))))
      (gvar-data-error () nil))
    out))
