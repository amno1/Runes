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

(deftype fixed-array ()
  "16.16 values, each wrapped to 32 bits as Rust's i32 does."
  '(simple-array (signed-byte 32) (*)))

(defstruct (gvar-scratch (:conc-name gs-))
  "Work arrays for the deltas of one glyph, kept to be used again: the points'
16.16 coordinates FX and FY, their deltas DX and DY, the moved points OUT-X
and OUT-Y of one tuple, which of them are TOUCHED, and the REFS of a contour."
  (fx (make-array 0 :element-type '(signed-byte 32)) :type fixed-array)
  (fy (make-array 0 :element-type '(signed-byte 32)) :type fixed-array)
  (dx (make-array 0 :element-type '(signed-byte 32)) :type fixed-array)
  (dy (make-array 0 :element-type '(signed-byte 32)) :type fixed-array)
  (out-x (make-array 0 :element-type '(signed-byte 32)) :type fixed-array)
  (out-y (make-array 0 :element-type '(signed-byte 32)) :type fixed-array)
  (touched (make-array 0 :element-type 'bit) :type simple-bit-vector)
  (refs (make-array 0 :element-type '(unsigned-byte 32))
   :type (simple-array (unsigned-byte 32) (*))))

;; The charstring package's fixed-point operations, inline and typed: the
;; same results.
(declaim (inline wrap32 f+ f- f* f/ fixed-from-int fixed-round))
(defun wrap32 (x)
  (declare (integer x))
  (sb-c::mask-signed-field 32 (ldb (byte 32 0) x)))
(defun f+ (a b) (declare (type (signed-byte 32) a b)) (wrap32 (+ a b)))
(defun f- (a b) (declare (type (signed-byte 32) a b)) (wrap32 (- a b)))
(defun f* (a b)
  "Rounded like FreeType's FT_MulFix."
  (declare (type (signed-byte 32) a b))
  (let ((ab (* a b)))
    (wrap32 (ash (+ ab #x8000 (ash ab -63)) -16))))
(defun f/ (a b) (runes/otf/charstring::fixed/ a b))
(defun fixed-from-int (i)
  "Shifted, wrapping like Rust's i << 16."
  (declare (integer i))
  (wrap32 (ash (ldb (byte 16 0) i) 16)))
(defun fixed-round (f) (ash (+ f #x8000) -16))

(define-condition gvar-data-error (error) ()
  (:documentation "Malformed glyph variation data; the glyph gets no deltas."))

(defun gvar-fail () (error 'gvar-data-error))

;;; Reading bytes

(defstruct (gvar-cursor (:conc-name gc-))
  (fd nil :type runes/otf:font-data)
  (pos 0 :type fixnum)
  (end 0 :type fixnum))

(declaim (inline gc-u8 gc-int))

(defun gc-u8 (c)
  (declare (gvar-cursor c))
  (when (>= (gc-pos c) (gc-end c)) (gvar-fail))
  (prog1 (runes/otf:font-data-read-u8 (gc-fd c) (gc-pos c)) (incf (gc-pos c))))

(defun gc-int (c bytes signed)
  (declare (gvar-cursor c) (type (integer 1 4) bytes))
  (when (> (+ (gc-pos c) bytes) (gc-end c)) (gvar-fail))
  (let ((v 0))
    (declare (type (unsigned-byte 32) v))
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
  "The variation data for GID, or NIL if the glyph has none.  What is read is
kept for the glyph, as the font does not change."
  (let ((cache (or (%gvar-cache loader)
                   (setf (%gvar-cache loader)
                         (let ((info (table-info "gvar" loader)))
                           (if info
                               (let ((gvar (runes/otf:read-gvar (font-data loader)
                                                                (offset info) (size info))))
                                 (vector info gvar
                                         (make-array (runes/otf:gvar-glyph-count gvar)
                                                     :initial-element nil)))
                               :none))))))
    (unless (eq cache :none)
      (let* ((info (svref cache 0))
             (fd (font-data loader))
             (gvar (svref cache 1))
             (known (svref cache 2)))
        (declare (simple-vector known))
        (when (< gid (length known))
          (let ((entry (svref known gid)))
            (cond
              ((null entry)
               (flet ((data-position (i)
                        (+ (offset info)
                           (runes/otf:gvar-glyph-variation-data-array-offset gvar)
                           (runes/otf:gvar-glyph-variation-data-offset-ref gvar i))))
                 (let* ((start (data-position gid))
                        (stop (data-position (1+ gid)))
                        (data (and (< start stop) (<= stop (+ (offset info) (size info)))
                                   (read-glyph-var-data fd gvar start stop))))
                   (setf (svref known gid) (or data :none))
                   data)))
              ((eq entry :none) nil)
              (t entry))))))))

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
        (let ((points (make-array count :element-type '(unsigned-byte 16)))
              (filled 0)
              (last 0)
              (seen 0))
          (declare (fixnum filled seen) (type (unsigned-byte 16) last))
          ;; One run: a control byte, then RUN values.  Whole runs are
          ;; consumed, even one that runs past the count.
          (loop while (< seen count)
                do (let* ((control (gc-u8 c))
                          (size (if (logbitp 7 control) 2 1))
                          (run (1+ (logand control #x7F))))
                     (dotimes (i run)
                       (let ((v (gc-int c size nil)))
                         (when (< filled count)
                           (setf last (ldb (byte 16 0) (+ last v)))
                           (setf (aref points filled) last)
                           (incf filled))))
                     (incf seen run)))
          points))))

(declaim (inline read-delta-run read-run-value scaled-delta add-scaled-delta))

(defun read-delta-run (c)
  "The next run at C, as two values: the count and the byte width (0 for zeros)."
  (declare (gvar-cursor c))
  (let ((control (gc-u8 c)))
    (values (1+ (logand control #x3F))
            (ecase (ash control -6) (0 1) (1 2) (2 0) (3 4)))))

(defun read-run-value (c width)
  (declare (gvar-cursor c) (type (member 0 1 2 4) width))
  (if (zerop width) 0 (gc-int c width t)))

(declaim (inline gc-signed-unchecked))
(defun gc-signed-unchecked (c bytes)
  "The signed BYTES-byte value at C's position, which is known to be within the
data; moves on."
  (declare (gvar-cursor c) (type (member 1 2 4) bytes) (optimize (speed 3) (safety 0)))
  (let ((fd (gc-fd c)) (pos (gc-pos c)) (v 0))
    (declare (type (unsigned-byte 32) v) (fixnum pos))
    (dotimes (i bytes)
      (setf v (logior (ash v 8) (runes/otf:font-data-read-u8 fd (+ pos i)))))
    (setf (gc-pos c) (+ pos bytes))
    (if (logbitp (1- (* 8 bytes)) v) (- v (ash 1 (* 8 bytes))) v)))

(defmacro do-delta-runs ((value width) c continue &body body)
  "While CONTINUE is true, read a run of deltas at cursor C and run BODY for
each of its values, with VALUE bound to the delta and WIDTH to the run's
byte width (0 for a run of zeros).  Runs are always read whole.  A run that
fits in the data is read without a check for each value; one that does not
is read value by value, so that the values before the end are used."
  (let ((run (gensym "RUN")) (i (gensym "I")) (cursor (gensym "C")) (w (gensym "W")))
    `(let ((,cursor ,c))
       (loop while ,continue
             do (multiple-value-bind (,run ,w) (read-delta-run ,cursor)
                  (declare (fixnum ,run) (type (member 0 1 2 4) ,w))
                  (let ((,width ,w))
                    (declare (ignorable ,width))
                    (macrolet ((run-of (bytes)
                                 `(dotimes (,',i ,',run)
                                    (let ((,',value (gc-signed-unchecked ,',cursor ,bytes)))
                                      ,@',body))))
                      (if (and (plusp ,w)
                               (<= (+ (gc-pos ,cursor) (* ,w ,run)) (gc-end ,cursor)))
                          (ecase ,w
                            (1 (run-of 1))
                            (2 (run-of 2))
                            (4 (run-of 4)))
                          (dotimes (,i ,run)
                            (let ((,value (if (zerop ,w) 0 (gc-int ,cursor ,w t))))
                              ,@body))))))))))

(defmacro do-dense-deltas ((index delta) c count &body body)
  "Run BODY for each INDEX below COUNT with its DELTA, read at cursor C; runs
must fit exactly.  READ-DENSE without the closure."
  `(let ((cur 0))
     (declare (fixnum cur))
     (do-delta-runs (,delta width) ,c (< cur ,count)
       (when (>= cur ,count) (gvar-fail))   ; a run past COUNT
       (let ((,index cur)) ,@body)
       (incf cur))))

(defmacro do-sparse-deltas ((point delta) c points count &body body)
  "Run BODY for each of the COUNT POINTS with its DELTA, read at cursor C, as
READ-SPARSE does but without the closure."
  `(let ((cur 0))
     (declare (fixnum cur))
     (do-delta-runs (,delta width) ,c (< cur ,count)
       (cond ((< cur (length ,points))
              (let ((,point (aref ,points cur))) ,@body))
             ;; A zero run needs a point for every value.
             ((zerop width) (gvar-fail)))
       (incf cur))))

(defun read-dense (c count fn)
  "Call FN with each index below COUNT and its delta; runs must fit exactly."
  (let ((cur 0))
    (do-delta-runs (delta width) c (< cur count)
      (when (>= cur count) (gvar-fail))   ; a run past COUNT
      (funcall fn cur delta)
      (incf cur))))

(defun read-sparse (c points count fn)
  "Call FN with each of the COUNT POINTS and its delta, as read_sparse_deltas does."
  (let ((cur 0))
    (do-delta-runs (delta width) c (< cur count)
      (cond ((< cur (length points)) (funcall fn (aref points cur) delta))
            ;; A zero run needs a point for every value.
            ((zerop width) (gvar-fail)))
      (incf cur))))

;;; Tuples

(defun read-tuple (fd pos axis-count)
  "The AXIS-COUNT 2.14 values at POS, as a vector."
  (let ((tuple (make-array axis-count)))
    (dotimes (i axis-count tuple)
      (setf (svref tuple i) (runes/otf:font-data-read-i16 fd (+ pos (* 2 i)))))))

(defun gvar-tuple-scalar (peak intermediate coords)
  "Scalar of a tuple at COORDS, or NIL when it does not apply (scalar_for).
Each axis multiplies the scalar by how far COORDS lies towards its peak."
  (declare (simple-vector peak coords) (type (or null cons) intermediate))
  (flet ((coord (i) (if (< i (length coords)) (svref coords i) 0))
         (scale (scalar numerator denominator)
           ;; From 2.14 to 16.16 for the division.
           (runes/otf/charstring::fixed-mul-div scalar (ash numerator 2) (ash denominator 2))))
    ;; Coordinates and tuples are 2.14 values; the scalar is 16.16.
    (loop with scalar of-type (signed-byte 32) = #x10000
          for i of-type fixnum below (length peak)
          for p of-type (signed-byte 16) = (svref peak i)
          for coord of-type (signed-byte 16) = (coord i)
          do (cond
               ;; No peak on this axis, or exactly at it: a factor of one.
               ((or (zerop p) (= p coord)))
               ((zerop coord) (return nil))
               ((null intermediate)
                (if (<= (min p 0) coord (max p 0))
                    (setf scalar (scale scalar coord p))
                    (return nil)))
               (t
                (let ((start (svref (car intermediate) i))
                      (end (svref (cdr intermediate) i)))
                  (declare (type (signed-byte 16) start end))
                  (cond
                    ;; An invalid region contributes a factor of one.
                    ((or (> start p) (> p end) (and (minusp start) (plusp end))))
                    ((or (<= coord start) (>= coord end)) (return nil))
                    ((< coord p) (setf scalar (scale scalar (- coord start) (- p start))))
                    (t (setf scalar (scale scalar (- end coord) (- end p))))))))
          finally (return (unless (zerop scalar) scalar)))))

(defun read-tuple-header (data pos)
  "The tuple variation header at POS: its data size, its tuple index field,
its peak tuple (NIL if it names a shared tuple that does not exist), its
intermediate region as (start . end) or NIL, and the position after it."
  (let* ((fd (glyph-var-data-fd data))
         (axis-count (glyph-var-data-axis-count data))
         (tuple-size (* 2 axis-count))
         (size (runes/otf:font-data-read-u16 fd pos))
         (index (runes/otf:font-data-read-u16 fd (+ pos 2)))
         (p (+ pos 4)))
    (flet ((next-tuple ()
             (prog1 (read-tuple fd p axis-count) (incf p tuple-size))))
      (let* ((peak (if (logbitp 15 index)
                       (next-tuple)
                       (let ((shared (logand index #x0FFF)))
                         (when (< shared (glyph-var-data-shared-tuple-count data))
                           (read-tuple fd (+ (glyph-var-data-shared-tuples data) (* shared tuple-size))
                                       axis-count)))))
             ;; Start, then end: arguments are evaluated left to right.
             (intermediate (when (logbitp 14 index)
                             (cons (next-tuple) (next-tuple)))))
        (values size index peak intermediate p)))))

(defun header-scalar (data pos index coords)
  "The scalar of the tuple whose header is at POS, with tuple index field
INDEX, at COORDS, read straight from the font data: the same as
GVAR-TUPLE-SCALAR on READ-TUPLE-HEADER's tuples, without making them.  NIL if
the tuple does not apply, or names a shared tuple that does not exist."
  (declare (type glyph-var-data data) (fixnum pos index) (simple-vector coords))
  (let* ((fd (glyph-var-data-fd data))
         (axis-count (glyph-var-data-axis-count data))
         (tuple-size (* 2 axis-count))
         (own (logbitp 15 index))
         (peak-pos (if own
                       (+ pos 4)
                       (let ((shared (logand index #x0FFF)))
                         (and (< shared (glyph-var-data-shared-tuple-count data))
                              (+ (glyph-var-data-shared-tuples data) (* shared tuple-size))))))
         (inter-start (and (logbitp 14 index) (+ pos 4 (if own tuple-size 0))))
         (inter-end (and inter-start (+ inter-start tuple-size))))
    (declare (fixnum axis-count tuple-size))
    (when peak-pos
      (flet ((coord (i) (if (< i (length coords)) (svref coords i) 0))
             (scale (scalar numerator denominator)
               (runes/otf/charstring::fixed-mul-div scalar (ash numerator 2) (ash denominator 2))))
        (declare (inline coord scale))
        (loop with scalar of-type (signed-byte 32) = #x10000
              for i of-type fixnum below axis-count
              for p of-type (signed-byte 16) = (runes/otf:font-data-read-i16 fd (+ peak-pos (* 2 i)))
              for coord of-type (signed-byte 16) = (coord i)
              do (cond
                   ((or (zerop p) (= p coord)))
                   ((zerop coord) (return nil))
                   ((null inter-start)
                    (if (<= (min p 0) coord (max p 0))
                        (setf scalar (scale scalar coord p))
                        (return nil)))
                   (t
                    (let ((start (runes/otf:font-data-read-i16 fd (+ inter-start (* 2 i))))
                          (end (runes/otf:font-data-read-i16 fd (+ inter-end (* 2 i)))))
                      (declare (type (signed-byte 16) start end))
                      (cond
                        ((or (> start p) (> p end) (and (minusp start) (plusp end))))
                        ((or (<= coord start) (>= coord end)) (return nil))
                        ((< coord p) (setf scalar (scale scalar (- coord start) (- p start))))
                        (t (setf scalar (scale scalar (- end coord) (- end p))))))))
              finally (return (unless (zerop scalar) scalar)))))))

(defun map-active-tuples (data coords fn)
  "Call FN with (scalar points data-cursor) for each tuple that applies at
COORDS; POINTS is :ALL or a vector, and the cursor is at the tuple's deltas."
  (declare (function fn) (type glyph-var-data data) (simple-vector coords))
  ;; The headers come one after another, and so do the tuples' data.  One
  ;; cursor does for all of them: FN is done with it when it returns.
  (let* ((fd (glyph-var-data-fd data))
         (cursor (make-gvar-cursor :fd fd))
         (header (glyph-var-data-headers data))
         (start (glyph-var-data-serialized data))
         (tuple-size (* 2 (glyph-var-data-axis-count data))))
    (declare (fixnum header start tuple-size))
    (dotimes (i (glyph-var-data-tuple-count data))
      (let* ((size (runes/otf:font-data-read-u16 fd header))
             (index (runes/otf:font-data-read-u16 fd (+ header 2)))
             (end (+ start size)))
        (declare (fixnum size index end))
        ;; Tuple data past the end of the glyph's data ends the iteration.
        (when (> end (glyph-var-data-serialized-end data))
          (return))
        (let ((scalar (header-scalar data header index coords)))
          (when scalar
            (setf (gc-pos cursor) start
                  (gc-end cursor) end)
            (funcall fn scalar
                     (if (logbitp 13 index)
                         (read-packed-points cursor)
                         (or (glyph-var-data-shared-points data) (gvar-fail)))
                     cursor)))
        (setf header (+ header 4 (* tuple-size (+ (if (logbitp 15 index) 1 0)
                                                  (if (logbitp 14 index) 2 0))))
              start end)))))

(defun scaled-delta (delta scalar)
  (declare (type (signed-byte 32) delta scalar))
  (if (= scalar #x10000)
      (fixed-from-int delta)
      (f* (fixed-from-int delta) scalar)))

;;; Simple glyphs (gvar/deltas.rs)

(defun interpolate-untouched (xs ys out-x out-y touched refs start end)
  "Infer deltas for the untouched points of the contour START..END (IUP)."
  (declare (type fixed-array xs ys out-x out-y) (simple-bit-vector touched)
           (type (simple-array (unsigned-byte 32) (*)) refs) (fixnum start end))
  (labels ((interp-coord (in out from to ref1 ref2)
             ;; Points FROM below TO, between references REF1 and REF2 along
             ;; the contour, in one coordinate: IN the original values, OUT
             ;; the moved ones, all 16.16.
             (declare (type fixed-array in out) (fixnum from to ref1 ref2))
             (loop with swap = (> (aref in ref1) (aref in ref2))
                   with in1 of-type (signed-byte 32) = (aref in (if swap ref2 ref1))
                   with in2 of-type (signed-byte 32) = (aref in (if swap ref1 ref2))
                   with out1 of-type (signed-byte 32) = (aref out (if swap ref2 ref1))
                   with out2 of-type (signed-byte 32) = (aref out (if swap ref1 ref2))
                   with scale of-type (signed-byte 32) = (if (/= in1 in2) (f/ (f- out2 out1) (f- in2 in1)) 0)
                   ;; References sharing a coordinate but moved apart leave
                   ;; the points alone: an empty range.
                   with stop = (if (or (/= in1 in2) (= out1 out2)) to from)
                   for i of-type fixnum from from below stop
                   for coord of-type (signed-byte 32) = (aref in i)
                   do (setf (aref out i)
                            (cond ((<= coord in1) (f+ coord (f- out1 in1)))
                                  ((>= coord in2) (f+ coord (f- out2 in2)))
                                  (t (f+ out1 (f* (f- coord in1) scale)))))))
           (interp (from to ref1 ref2)
             (interp-coord xs out-x from to ref1 ref2)
             (interp-coord ys out-y from to ref1 ref2)))
    ;; The touched points of the contour, in order, in REFS.
    (let ((count 0))
      (declare (fixnum count))
      (loop for i of-type fixnum from start to end
            when (= 1 (sbit touched i))
              do (setf (aref refs count) i)
                 (incf count))
      (cond
        ((zerop count))
        ;; A single reference carries the whole contour with it.
        ((= count 1)
         (loop with ref = (aref refs 0)
               with dx = (f- (aref out-x ref) (aref xs ref))
               with dy = (f- (aref out-y ref) (aref ys ref))
               for i from start to end
               unless (= i ref)
                 do (setf (aref out-x i) (f+ (aref out-x i) dx)
                          (aref out-y i) (f+ (aref out-y i) dy))))
        (t
         ;; Between each two references, then around the end of the contour:
         ;; after the last one to the end, and from the start to the first.
         (dotimes (k (1- count))
           (let ((ref (aref refs k)) (next (aref refs (1+ k))))
             (interp (1+ ref) next ref next)))
         (let ((first (aref refs 0))
               (final (aref refs (1- count))))
           (interp (1+ final) (1+ end) final first)
           (interp start first final first)))))))

(defun add-scaled-delta (vector i delta scalar)
  (declare (simple-vector vector) (fixnum i))
  (setf (aref vector i) (f+ (aref vector i) (scaled-delta delta scalar))))

(declaim (inline add-fixed-delta))
(defun add-fixed-delta (array i delta scalar)
  "ARRAY's element I plus DELTA scaled by SCALAR, as ADD-SCALED-DELTA, for
the typed arrays of the glyph path."
  (declare (type fixed-array array) (fixnum i) (type (signed-byte 32) delta scalar))
  (setf (aref array i) (f+ (aref array i) (scaled-delta delta scalar))))

(defun add-dense-deltas (c scalar dx dy n)
  "Add a tuple's deltas for the N points, read at cursor C, to DX and DY."
  (declare (gvar-cursor c) (type (signed-byte 32) scalar) (type fixed-array dx dy)
           (fixnum n))
  (progn
    (do-dense-deltas (i d) c n (add-fixed-delta dx i d scalar))
    (do-dense-deltas (i d) c n (add-fixed-delta dy i d scalar))))

(defun interpolate-contours (fx fy out-x out-y touched refs ends n-ends n)
  "Infer the deltas of the untouched points of every contour (IUP); ENDS holds
the N-ENDS contour end points, and there are N points."
  (declare (type fixed-array fx fy out-x out-y) (simple-bit-vector touched)
           (type (simple-array (unsigned-byte 32) (*)) refs)
           (type (simple-array (unsigned-byte 16) (*)) ends) (fixnum n-ends n))
  (let ((start 0))
    (declare (fixnum start))
    (dotimes (c n-ends)
      (let ((end (aref ends c)))
        ;; A contour ending before the previous one is skipped.
        (when (>= end start)
          (when (>= end n) (gvar-fail))
          (interpolate-untouched fx fy out-x out-y touched refs start end)
          (setf start (1+ end)))))))

(defun add-sparse-deltas (c scalar points sc n ends n-ends)
  "Add a tuple's deltas for POINTS, read at cursor C, to the deltas of the
scratch SC, with those of the other points inferred from them (IUP).  There
are N points."
  (declare (gvar-cursor c) (type (signed-byte 32) scalar)
           (type (simple-array (unsigned-byte 16) (*)) points)
           (type gvar-scratch sc) (fixnum n n-ends)
           (type (simple-array (unsigned-byte 16) (*)) ends))
  (let ((fx (gs-fx sc)) (fy (gs-fy sc)) (dx (gs-dx sc)) (dy (gs-dy sc))
        (out-x (gs-out-x sc)) (out-y (gs-out-y sc)) (touched (gs-touched sc)))
    (dotimes (i n)
      (setf (aref out-x i) (aref fx i)
            (aref out-y i) (aref fy i)
            (sbit touched i) 0))
    ;; Point numbers past the glyph's points are skipped.
    (do-sparse-deltas (i d) c points (length points)
      (when (< i n)
        (add-fixed-delta out-x i d scalar)
        (setf (sbit touched i) 1)))
    (do-sparse-deltas (i d) c points (length points)
      (when (< i n)
        (add-fixed-delta out-y i d scalar)))
    (interpolate-contours fx fy out-x out-y touched (gs-refs sc) ends n-ends n)
    (dotimes (i n)
      (setf (aref dx i) (f+ (aref dx i) (f- (aref out-x i) (aref fx i)))
            (aref dy i) (f+ (aref dy i) (f- (aref out-y i) (aref fy i)))))))

(defun gvar-scratch-for (sc count)
  "SC, or a new scratch if it is NIL, with room for COUNT points; its deltas
for them are zero."
  (declare (fixnum count))
  (let ((sc (or sc (make-gvar-scratch))))
    (when (< (length (gs-fx sc)) count)
      (let ((cap (max count 64 (* 2 (length (gs-fx sc))))))
        (flet ((fixed () (make-array cap :element-type '(signed-byte 32))))
          (setf (gs-fx sc) (fixed) (gs-fy sc) (fixed) (gs-dx sc) (fixed) (gs-dy sc) (fixed)
                (gs-out-x sc) (fixed) (gs-out-y sc) (fixed)
                (gs-touched sc) (make-array cap :element-type 'bit)
                (gs-refs sc) (make-array cap :element-type '(unsigned-byte 32))))))
    (let ((dx (gs-dx sc)) (dy (gs-dy sc)))
      (dotimes (i count)
        (setf (aref dx i) 0
              (aref dy i) 0)))
    sc))

(defun scratch-glyph-deltas (data coords sc count ends n-ends)
  "Fill the deltas in SC, whose FX and FY hold the 16.16 coordinates of COUNT
points; ENDS holds the N-ENDS contour end points.  True, or NIL if the data
is malformed."
  (declare (type gvar-scratch sc) (fixnum count n-ends)
           (type (simple-array (unsigned-byte 16) (*)) ends))
  (let ((dx (gs-dx sc)) (dy (gs-dy sc)))
    (handler-case
        (progn
          (map-active-tuples data coords
                             (lambda (scalar points c)
                               (if (eq points :all)
                                   (add-dense-deltas c scalar dx dy count)
                                   (add-sparse-deltas c scalar points sc count ends n-ends))))
          t)
      (gvar-data-error () nil))))

(defun simple-glyph-deltas (data coords xs ys ends)
  "16.16 deltas for every point of a simple glyph, phantom points included,
as two arrays; XS and YS are the font unit coordinates of all its points
and ENDS (a list) its contour end points. NIL if the data is malformed."
  (declare (simple-vector xs ys))
  (let* ((n (length xs))
         (sc (gvar-scratch-for (make-gvar-scratch) n))
         (ends (coerce ends '(simple-array (unsigned-byte 16) (*)))))
    (let ((fx (gs-fx sc)) (fy (gs-fy sc)))
      (dotimes (i n)
        (setf (aref fx i) (fixed-from-int (svref xs i))
              (aref fy i) (fixed-from-int (svref ys i)))))
    (when (scratch-glyph-deltas data coords sc n ends (length ends))
      ;; Exact-size arrays, as the callers expect.
      (values (subseq (gs-dx sc) 0 n) (subseq (gs-dy sc) 0 n)))))

(defun zone-glyph-deltas (data coords ux uy count ends n-ends sc)
  "Like SIMPLE-GLYPH-DELTAS for the COUNT points (phantom points included) in
the first elements of the arrays UX and UY of font unit coordinates, and the
N-ENDS contour end points in ENDS, with the work arrays SC (from
GVAR-SCRATCH-FOR).  The deltas are in SC's DX and DY: true, or NIL if the data
is malformed."
  (declare (type fixed-array ux uy) (fixnum count n-ends)
           (type (simple-array (unsigned-byte 16) (*)) ends) (type gvar-scratch sc))
  (let ((fx (gs-fx sc)) (fy (gs-fy sc)))
    (dotimes (i count)
      (setf (aref fx i) (fixed-from-int (aref ux i))
            (aref fy i) (fixed-from-int (aref uy i)))))
  (scratch-glyph-deltas data coords sc count ends n-ends))

;;; Composite glyphs

(defun add-component-deltas (points half deltas count scalar dx dy)
  "Add the scaled DELTAS (the first COUNT of the array) of the positions
POINTS, or 0.. when it is NIL, to DX and DY: HALF x deltas, then as many y
deltas.  Positions past the components, or without a y delta, are skipped."
  (declare (type (or null (simple-array (unsigned-byte 16) (*))) points)
           (fixnum half count) (type fixed-array deltas dx dy)
           (type (signed-byte 32) scalar))
  (dotimes (i half)
    (let ((position (if points (aref points i) i)))
      (when (and (< position (length dx)) (< (+ half i) count))
        (add-fixed-delta dx position (aref deltas i) scalar)
        (add-fixed-delta dy position (aref deltas (+ half i)) scalar)))))

(defun read-n-deltas (c limit)
  "Up to LIMIT deltas from C, fewer if the data ends: an array, and how many."
  (declare (fixnum limit))
  (let ((out (make-array limit :element-type '(signed-byte 32)))
        (n 0))
    (declare (fixnum n))
    (handler-case
        (do-delta-runs (delta width) c (< n limit)
          (when (< n limit)
            (setf (aref out n) delta)
            (incf n)))
      (gvar-data-error () nil))
    (values out n)))

(defun read-all-deltas (c)
  "Every delta from C to the end of its data: an array, and how many."
  (let ((out (make-array 32 :element-type '(signed-byte 32)))
        (n 0))
    (declare (type fixed-array out) (fixnum n))
    (handler-case
        (do-delta-runs (delta width) c (< (gc-pos c) (gc-end c))
          (when (= n (length out))
            (let ((bigger (make-array (* 2 n) :element-type '(signed-byte 32))))
              (replace bigger out)
              (setf out bigger)))
          (setf (aref out n) delta)
          (incf n))
      (gvar-data-error () nil))
    (values out n)))

(defun composite-glyph-deltas (data coords count)
  "16.16 deltas for the COUNT components and phantom points of a composite
glyph, as two vectors; NIL if the data is malformed."
  (let ((dx (make-array count :element-type '(signed-byte 32) :initial-element 0))
        (dy (make-array count :element-type '(signed-byte 32) :initial-element 0)))
    (handler-case
        (progn
          (map-active-tuples
           data coords
           (lambda (scalar points c)
             (if (eq points :all)
                 ;; Every delta in the data, half of them x and half y.
                 (multiple-value-bind (all n) (read-all-deltas c)
                   (add-component-deltas nil (floor n 2) all n scalar dx dy))
                 (multiple-value-bind (all n) (read-n-deltas c (* 2 (length points)))
                   (add-component-deltas points (length points) all n scalar dx dy)))))
          (values dx dy))
      (gvar-data-error () nil))))
