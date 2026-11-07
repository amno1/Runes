;;;; glyf-outlines.lisp — TrueType glyph outlines, with variations
;;;; Copyright (c) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

;;; Loads a TrueType glyph's points in font units as Fontations' unscaled
;;; outline loader does (read-fonts src/tables/glyf/outline: load.rs and the
;;; Unscaled scaler), which matches FreeType: points are read with four
;;; phantom points for the metrics, 'gvar' deltas are added at the loader's
;;; instance and rounded, and composite components are transformed in 16.16
;;; fixed point, offset (with their own deltas) or aligned by point.

(in-package #:zpb-ttf)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(deftype growable-vector ()
  "An adjustable vector with a fill pointer, as MAKE-ARRAY makes."
  '(and (vector t) (not simple-array)))

(defstruct (outline-points (:conc-name op-))
  (xs (make-array 32 :adjustable t :fill-pointer 0) :type growable-vector)
  (ys (make-array 32 :adjustable t :fill-pointer 0) :type growable-vector)
  (on (make-array 32 :adjustable t :fill-pointer 0) :type growable-vector)
  ;; End point of each contour, as indices into the arrays above.
  (ends (make-array 8 :adjustable t :fill-pointer 0) :type growable-vector))

(declaim (sb-ext:freeze-type outline-points))

(defun grow-fill-pointer (vector count)
  "Make room in the growable VECTOR for COUNT more elements and move its fill
pointer past them; their values are left to the caller."
  (declare (type growable-vector vector)
           (type (integer 0 #.array-dimension-limit) count))
  (let ((new (the (integer 0 #.array-dimension-limit) (+ (fill-pointer vector) count))))
    (when (> new (array-dimension vector 0))
      (adjust-array vector (max new (* 2 (array-dimension vector 0)))))
    (setf (fill-pointer vector) new)
    vector))

(defun loader-gvar-coords (loader)
  "The instance as raw 2.14 coordinates when 'gvar' applies to it, else NIL.
Worked out once for an instance (SET-INSTANCE forgets it): hinting asks for it
for every glyph."
  (let ((cache (%gvar-coords-cache loader)))
    (if (eq cache :unset)
        (setf (%gvar-coords-cache loader)
              (let ((coords (normalized-coordinates loader)))
                (when (and coords (table-info "gvar" loader))
                  (if (listp coords)
                      (let ((v (make-array (length (the list coords)) :element-type t)))
                        (loop for c in coords for i of-type fixnum from 0
                              do (setf (svref v i) (runes/otf/charstring::f2dot14-from-real c)))
                        v)
                      (let* ((cv (the simple-vector coords))
                             (v (make-array (length cv) :element-type t)))
                        (loop for i of-type fixnum from 0 below (length cv)
                              do (setf (svref v i) (runes/otf/charstring::f2dot14-from-real (svref cv i))))
                        v)))))
        cache)))

;;; Reading a simple glyph

(defun read-point-flags (fd pos n)
  "The N point flags at POS, a flag with the repeat bit followed by its
repeat count; and the position after them."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) pos) (fixnum n))
  (let ((flags (make-array n :element-type '(unsigned-byte 8)))
        (i 0))
    (loop while (< i n)
          do (let ((flag (runes/otf:font-data-read-u8 fd pos))
                   (repeats 0))
               (incf pos)
               (when (logbitp 3 flag)
                 (setf repeats (runes/otf:font-data-read-u8 fd pos))
                 (incf pos))
               ;; A repeat past the last point is dropped.
               (loop repeat (min (1+ repeats) (- n i))
                     do (setf (aref flags i) flag)
                        (incf i))))
    (values flags pos)))

(defun read-simple-points (fd pos contour-count)
  "End points, x and y coordinates and on-curve flags of the simple glyph
whose contour count is at POS - 10, as four vectors."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) pos)
           (type (unsigned-byte 16) contour-count))
  (let ((ends (make-array contour-count)))
    (declare (simple-vector ends))
    (dotimes (i contour-count)
      (setf (aref ends i) (runes/otf:font-data-read-u16 fd (+ pos (* 2 i)))))
    (incf pos (* 2 contour-count))
    (let* ((n (if (zerop contour-count) 0 (1+ (aref ends (1- contour-count)))))
           (xs (make-array n))
           (ys (make-array n)))
      (declare (simple-vector xs ys) (type (unsigned-byte 16) n))
      ;; Skip the instructions.
      (incf pos (+ 2 (runes/otf:font-data-read-u16 fd pos)))
      (multiple-value-bind (flags flags-end) (read-point-flags fd pos n)
        (declare (type (simple-array (unsigned-byte 8) (*)) flags))
        (setf pos flags-end)
        (flet ((coordinates (out short-bit same-bit)
                 ;; Each a byte (sign from SAME-BIT), the previous value
                 ;; (SAME-BIT without SHORT-BIT), or a word.
                 (declare (simple-vector out) (type (integer 0 7) short-bit same-bit))
                 (let ((current 0))
                   (declare (fixnum current))
                   (dotimes (j n)
                     (let ((flag (aref flags j)))
                       (incf current
                             (cond ((logbitp short-bit flag)
                                    (let ((v (runes/otf:font-data-read-u8 fd pos)))
                                      (incf pos)
                                      (if (logbitp same-bit flag) v (- v))))
                                   ((logbitp same-bit flag) 0)
                                   (t (prog1 (runes/otf:font-data-read-i16 fd pos) (incf pos 2)))))
                       (setf (aref out j) current))))))
          (coordinates xs 1 4)
          (coordinates ys 2 5))
        (values ends xs ys
                (let ((on (make-array n)))
                  (dotimes (i n on)
                    (setf (svref on i) (logbitp 0 (aref flags i))))))))))

;;; Phantom points

(deftype phantom-array ()
  "The four phantom points of a glyph as X0 Y0 X1 Y1 X2 Y2 X3 Y3."
  '(simple-array (signed-byte 32) (8)))

(defun phantom-array (loader gid x-min y-max)
  "The four phantom points of GID, in font units, from its metrics."
  (declare (type font-loader loader)
           (type (unsigned-byte 16) gid)
           (type fixnum x-min y-max))
  (let* ((lsb (horizontal-bearing loader gid))
         (advance (horizontal-advance loader gid))
         (ascent (the fixnum (slot-value loader 'ascender)))
         (descent (the fixnum (slot-value loader 'descender)))
         (result (make-array 8 :element-type '(signed-byte 32))))
    (declare (type fixnum lsb advance ascent descent))
    (multiple-value-bind (tsb vadvance)
        (if (vmtx-missing-p loader)
            (values (the fixnum (- ascent y-max)) (the fixnum (- ascent descent)))
            (values (the fixnum (vertical-bearing loader gid))
                    (the fixnum (vertical-advance loader gid))))
      (let ((pp1x (- x-min lsb)))
        (setf (aref result 0) pp1x
              (aref result 1) 0
              (aref result 2) (+ pp1x advance)
              (aref result 3) 0
              (aref result 4) 0
              (aref result 5) (+ y-max tsb)
              (aref result 6) 0
              (aref result 7) (- (+ y-max tsb) vadvance))))
    result))

(declaim (ftype (function (font-loader (unsigned-byte 16) fixnum fixnum)
                          (values simple-vector &optional))
                phantom-points))
(defun phantom-points (loader gid x-min y-max)
  "The four phantom points of GID as a vector of (x . y), from its metrics."
  (let ((a (phantom-array loader gid x-min y-max)))
    (vector (cons (aref a 0) (aref a 1))
            (cons (aref a 2) (aref a 3))
            (cons (aref a 4) (aref a 5))
            (cons (aref a 6) (aref a 7)))))

(defun phantom-column (phantom column)
  "The x (COLUMN 0) or y (COLUMN 1) of the four phantom points in the array
PHANTOM, as a vector."
  (declare (type phantom-array phantom) (type (integer 0 1) column))
  (vector (aref phantom column) (aref phantom (+ 2 column))
          (aref phantom (+ 4 column)) (aref phantom (+ 6 column))))

(defun add-phantom-array-deltas (phantom dx dy start)
  "A copy of the array PHANTOM moved by the rounded deltas at START of DX and DY."
  (declare (type phantom-array phantom)
           (type fixed-array dx dy)
           (type fixnum start))
  (let ((result (copy-seq phantom)))
    (declare (type phantom-array result))
    (dotimes (i 4 result)
      (incf (aref result (* 2 i)) (the fixnum (fixed-round (aref dx (+ start i)))))
      (incf (aref result (1+ (* 2 i))) (the fixnum (fixed-round (aref dy (+ start i))))))))

(defun add-phantom-array-deltas! (phantom dx dy start)
  "PHANTOM, the array, moved by the rounded deltas at START of DX and DY."
  (declare (type phantom-array phantom)
           (type fixed-array dx dy)
           (type fixnum start))
  (dotimes (i 4 phantom)
    (incf (aref phantom (* 2 i)) (the fixnum (fixed-round (aref dx (+ start i)))))
    (incf (aref phantom (1+ (* 2 i))) (the fixnum (fixed-round (aref dy (+ start i)))))))

(defun add-phantom-deltas (phantom dx dy start)
  "Phantom points moved by the rounded deltas at START of DX and DY."
  (declare (type simple-vector phantom)
           (type fixed-array dx dy)
           (type fixnum start))
  (let ((res (make-array 4)))
    (dotimes (i 4 res)
      (let ((p (the cons (svref phantom i))))
        (setf (svref res i)
              (cons (the fixnum (+ (the fixnum (car p)) (the fixnum (fixed-round (aref dx (+ start i))))))
                    (the fixnum (+ (the fixnum (cdr p)) (the fixnum (fixed-round (aref dy (+ start i))))))))))))

;;; Loading

(declaim (ftype (function (font-loader (unsigned-byte 16) outline-points (or null simple-vector))
                          (values simple-vector &optional))
                load-glyph-points))
(defun load-glyph-points (loader gid out coords)
  "Append the points of glyph GID to OUT; return its phantom points."
  (declare (type font-loader loader)
           (type (unsigned-byte 16) gid)
           (type outline-points out)
           (type (or null simple-vector) coords))
  (when (> (the fixnum (list-length *compound-glyph-stack*)) (the fixnum *max-compound-glyph-depth*))
    (error 'unsupported-value :description "Compound glyph nesting too deep"
                              :location (format nil "glyph ~D" gid)
                              :actual-value (the fixnum (list-length *compound-glyph-stack*))
                              :expected-values (list *max-compound-glyph-depth*)))
  (when (member gid *compound-glyph-stack*)
    (error 'unsupported-value :description "Recursive compound glyph"
                              :location (format nil "glyph ~D" (first *compound-glyph-stack*))
                              :actual-value gid
                              :expected-values '("a glyph that does not contain itself")))
  (multiple-value-bind (g-off len) (glyph-loca-info loader gid)
    (let* ((tables (loader-outline-tables loader))
           (glyf-offset (ot-glyf-offset tables))
           (fd (ot-fd tables)))
      (if (or (zerop len) (null glyf-offset))
          ;; An empty glyph: only its metrics can vary.
          (let ((phantom (phantom-points loader gid 0 0))
                (data (and coords (gvar-glyph-data loader gid))))
            (if data
                (let ((px (make-array 4)) (py (make-array 4)))
                  (dotimes (k 4)
                    (setf (svref px k) (car (svref phantom k))
                          (svref py k) (cdr (svref phantom k))))
                  (multiple-value-bind (dx dy)
                      (simple-glyph-deltas data coords px py '())
                    (if dx
                        (let ((moved (add-phantom-deltas phantom dx dy 0)))
                          ;; Only the horizontal phantom points take deltas here.
                          (vector (aref moved 0) (aref moved 1) (aref phantom 2) (aref phantom 3)))
                        phantom)))
                phantom))
          (let* ((pos (+ glyf-offset g-off))
                 (contour-count (runes/otf:font-data-read-i16 fd pos))
                 (phantom (phantom-points loader gid
                                          (runes/otf:font-data-read-i16 fd (+ pos 2))
                                          (runes/otf:font-data-read-i16 fd (+ pos 8)))))
            (if (minusp contour-count)
                (let ((*compound-glyph-stack* (cons gid *compound-glyph-stack*)))
                  (load-composite-points loader gid (+ pos 10) phantom out coords))
                (load-simple-points loader gid fd (+ pos 10) contour-count phantom out coords)))))))

(defun load-simple-points (loader gid fd pos contour-count phantom out coords)
  (declare (type font-loader loader)
           (type (unsigned-byte 16) gid)
           (type runes/otf:font-data fd)
           (type (unsigned-byte 32) pos)
           (type fixnum contour-count)
           (type simple-vector phantom)
           (type outline-points out)
           (type (or null simple-vector) coords))
  (multiple-value-bind (ends xs ys on) (read-simple-points fd pos contour-count)
    (declare (type simple-vector ends xs ys on))
    (let* ((n (length xs))
           (data (and coords (gvar-glyph-data loader gid))))
      (when data
        ;; Deltas cover the points and the phantom points.
        (let ((all-x (make-array (+ n 4)))
              (all-y (make-array (+ n 4))))
          (replace all-x xs)
          (replace all-y ys)
          (dotimes (k 4)
            (setf (svref all-x (+ n k)) (car (svref phantom k))
                  (svref all-y (+ n k)) (cdr (svref phantom k))))
          (multiple-value-bind (dx dy) (simple-glyph-deltas data coords all-x all-y (coerce ends 'list))
            (when dx
              (let ((dx (the fixed-array dx))
                    (dy (the fixed-array dy)))
                (dotimes (i n)
                  (setf (svref xs i) (the fixnum (+ (the fixnum (svref xs i)) (the fixnum (fixed-round (aref dx i)))))
                        (svref ys i) (the fixnum (+ (the fixnum (svref ys i)) (the fixnum (fixed-round (aref dy i)))))))
                (setf phantom (add-phantom-deltas phantom dx dy n)))))))
      (let ((base (fill-pointer (op-xs out))))
        (declare (fixnum base))
        (grow-fill-pointer (op-xs out) n)
        (grow-fill-pointer (op-ys out) n)
        (grow-fill-pointer (op-on out) n)
        (sb-kernel:with-array-data ((oxs (op-xs out)) (xs0) (xe))
          (declare (ignore xe) (simple-vector oxs) (fixnum xs0))
          (sb-kernel:with-array-data ((oys (op-ys out)) (ys0) (ye))
            (declare (ignore ye) (simple-vector oys) (fixnum ys0))
            (sb-kernel:with-array-data ((oon (op-on out)) (on0) (oe))
              (declare (ignore oe) (simple-vector oon) (fixnum on0))
              (dotimes (i n)
                (setf (svref oxs (+ xs0 base i)) (svref xs i)
                      (svref oys (+ ys0 base i)) (svref ys i)
                      (svref oon (+ on0 base i)) (svref on i))))))
        (let ((ends-out (op-ends out)))
          (declare (type growable-vector ends-out))
          (let ((first (fill-pointer ends-out)))
            (declare (type fixnum first))
            (grow-fill-pointer ends-out (length ends))
            (sb-kernel:with-array-data ((e ends-out) (e0) (ee))
              (declare (ignore ee) (simple-vector e) (fixnum e0))
              (let ((idx (the fixnum (+ e0 first))))
                (declare (fixnum idx))
                (dotimes (c (length ends))
                  (setf (svref e idx) (the fixnum (+ base (the fixnum (svref ends c)))))
                  (incf idx)))))))
      phantom)))

(defun component-fields (fd pos)
  "The component record at POS: its flags, glyph, two arguments, the 16.16
transform xx yx xy yy, and the position after it."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) pos))
  (let* ((flags (the (unsigned-byte 16) (runes/otf:font-data-read-u16 fd pos)))
         (glyph (the (unsigned-byte 16) (runes/otf:font-data-read-u16 fd (+ pos 2))))
         (words (logbitp 0 flags))
         (xy (logbitp 1 flags))
         (p (the (unsigned-byte 32) (+ pos 4)))
         arg1 arg2)
    (declare (type (unsigned-byte 16) flags glyph)
             (type (unsigned-byte 32) p))
    (if words
        (setf arg1 (if xy (runes/otf:font-data-read-i16 fd p) (runes/otf:font-data-read-u16 fd p))
              arg2 (if xy (runes/otf:font-data-read-i16 fd (+ p 2)) (runes/otf:font-data-read-u16 fd (+ p 2)))
              p (+ p 4))
        (setf arg1 (if xy (runes/otf:font-data-read-i8 fd p) (runes/otf:font-data-read-u8 fd p))
              arg2 (if xy (runes/otf:font-data-read-i8 fd (+ p 1)) (runes/otf:font-data-read-u8 fd (+ p 1)))
              p (+ p 2)))
    ;; Transform elements as 16.16: the 2.14 bits times 4.
    (let ((xx #x10000) (yx 0) (xy-skew 0) (yy #x10000))
      (declare (type (signed-byte 32) xx yx xy-skew yy))
      (flet ((f2 (at) (the (signed-byte 32) (* 4 (the fixnum (runes/otf:font-data-read-i16 fd at))))))
        (declare (inline f2))
        (cond ((logbitp 3 flags) (setf xx (f2 p) yy xx) (incf p 2))
              ((logbitp 6 flags) (setf xx (f2 p) yy (f2 (+ p 2))) (incf p 4))
              ((logbitp 7 flags) (setf xx (f2 p) yx (f2 (+ p 2)) xy-skew (f2 (+ p 4)) yy (f2 (+ p 6)))
               (incf p 8))))
      (values flags glyph (the fixnum arg1) (the fixnum arg2) xx yx xy-skew yy p))))

(defun read-component (fd pos)
  "The component record at POS as a plist, and the position after it."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) pos))
  (multiple-value-bind (flags glyph arg1 arg2 xx yx xy-skew yy p) (component-fields fd pos)
    (declare (type (unsigned-byte 16) flags glyph)
             (type fixnum arg1 arg2)
             (type (signed-byte 32) xx yx xy-skew yy)
             (type (unsigned-byte 32) p))
    (let ((m (make-array 4 :element-type '(signed-byte 32))))
      (setf (aref m 0) xx (aref m 1) yx (aref m 2) xy-skew (aref m 3) yy)
      (values (list :flags flags :glyph glyph :xy (logbitp 1 flags) :arg1 arg1 :arg2 arg2
                    :transform m
                    :transformed (logtest flags #b11001000))
              p))))

(declaim (inline hypot-fixed))
(declaim (ftype (function ((signed-byte 32) (signed-byte 32))
                          (values (signed-byte 32) &optional))
                hypot-fixed))
(defun hypot-fixed (a b)
  (declare (type (signed-byte 32) a b))
  (let ((a (abs a)) (b (abs b)))
    (declare (type (unsigned-byte 32) a b))
    (wrap32 (the (unsigned-byte 34)
                 (if (> a b)
                     (+ a (the (unsigned-byte 31) (ash (the (unsigned-byte 34) (* 3 b)) -3)))
                     (+ b (the (unsigned-byte 31) (ash (the (unsigned-byte 34) (* 3 a)) -3))))))))

(defun read-components (fd pos)
  "The component records of the composite glyph at POS, in order."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) pos))
  (loop for p of-type (unsigned-byte 32) = pos then next
        for (component next) = (multiple-value-list (read-component fd p))
        collect component
        while (logbitp 5 (the fixnum (getf component :flags)))))

(defun transform-component-points (xs ys start end m)
  "Apply the 2x2 matrix M to points START below END."
  (declare (type growable-vector xs ys)
           (type fixnum start end)
           (type (simple-array (signed-byte 32) (4)) m))
  (let ((m0 (aref m 0)) (m1 (aref m 1))
        (m2 (aref m 2)) (m3 (aref m 3)))
    (declare (type (signed-byte 32) m0 m1 m2 m3))
    (loop for j of-type fixnum from start below end
          for x of-type (signed-byte 32) = (the fixnum (aref xs j))
          for y of-type (signed-byte 32) = (the fixnum (aref ys j))
          ;; Font units taken as raw 16.16, as FreeType does.
          do (setf (aref xs j) (f+ (f* x m0) (f* y m2))
                   (aref ys j) (f+ (f* x m1) (f* y m3))))))

(defun component-offset (component gid xs ys point-base start end delta-x delta-y)
  "The offset of a component whose points are START below END: its x and y
arguments, plus the variation deltas DELTA-X and DELTA-Y (16.16, or NIL),
or the distance that moves its point ARG2 onto the glyph's point ARG1."
  (declare (type list component)
           (type (unsigned-byte 16) gid)
           (type growable-vector xs ys)
           (type fixnum point-base start end)
           (type (or null (signed-byte 32)) delta-x delta-y))
  (if (getf component :xy)
      (let ((x (the fixnum (getf component :arg1)))
            (y (the fixnum (getf component :arg2)))
            (m (the (simple-array (signed-byte 32) (4)) (getf component :transform))))
        ;; SCALED_COMPONENT_OFFSET without UNSCALED_COMPONENT_OFFSET.
        (when (and (getf component :transformed)
                   (= (logand (the fixnum (getf component :flags)) #x1800) #x0800))
          (setf x (f* x (hypot-fixed (aref m 0) (aref m 2)))
                y (f* y (hypot-fixed (aref m 3) (aref m 1)))))
        (if delta-x
            (values (the fixnum (+ x (the fixnum (fixed-round delta-x))))
                    (the fixnum (+ y (the fixnum (fixed-round (the (signed-byte 32) delta-y))))))
            (values x y)))
      (let ((base (+ point-base (the fixnum (getf component :arg1))))
            (child (+ start (the fixnum (getf component :arg2)))))
        (unless (and (< base start) (< child end))
          (error 'unsupported-value
                 :description "Compound glyph anchor point out of range"
                 :location (format nil "glyph ~D" gid)
                 :actual-value (list (getf component :arg1) (getf component :arg2))
                 :expected-values '("existing point indices")))
        (values (the fixnum (- (the fixnum (aref xs base)) (the fixnum (aref xs child))))
                (the fixnum (- (the fixnum (aref ys base)) (the fixnum (aref ys child))))))))

(defun load-composite-points (loader gid pos phantom out coords)
  (declare (type font-loader loader)
           (type (unsigned-byte 16) gid)
           (type (unsigned-byte 32) pos)
           (type simple-vector phantom)
           (type outline-points out)
           (type (or null simple-vector) coords))
  (let* ((components (read-components (font-data loader) pos))
         (count (+ (length components) 4))
         (data (and coords (gvar-glyph-data loader gid)))
         (dx nil) (dy nil)
         (point-base (fill-pointer (op-xs out))))
    (declare (type fixnum count point-base))
    (when data
      (multiple-value-setq (dx dy) (composite-glyph-deltas data coords count))
      (when dx
        (setf phantom (add-phantom-deltas phantom dx dy (- count 4)))))
    (loop for component in components
          for i of-type fixnum from 0
          do (let* ((start (fill-pointer (op-xs out)))
                    (component-phantom (load-glyph-points loader (getf component :glyph) out coords))
                    (end (fill-pointer (op-xs out)))
                    (xs (op-xs out))
                    (ys (op-ys out)))
               (declare (type fixnum start end))
               ;; USE_MY_METRICS takes the component's phantom points.
               (when (logbitp 9 (the fixnum (getf component :flags)))
                 (setf phantom component-phantom))
               (when (getf component :transformed)
                 (transform-component-points xs ys start end (getf component :transform)))
               (multiple-value-bind (ox oy)
                   (component-offset component gid xs ys point-base start end
                                     (and dx (aref (the fixed-array dx) i))
                                     (and dy (aref (the fixed-array dy) i)))
                 (declare (type fixnum ox oy))
                 (unless (and (zerop ox) (zerop oy))
                   (loop for j of-type fixnum from start below end
                         do (incf (the fixnum (aref xs j)) ox)
                            (incf (the fixnum (aref ys j)) oy))))))
    phantom))

(defun glyph-outline-points (loader gid)
  "The outline points of GID at LOADER's instance, and its phantom points."
  (let* ((out (make-outline-points))
         (phantom (load-glyph-points loader gid out (loader-gvar-coords loader))))
    (values out phantom)))

(defun outline-points->contours (out)
  (let ((ends (op-ends out))
        (start 0))
    (declare (fixnum start))
    (sb-kernel:with-array-data ((xs (op-xs out)) (xs0) (xe))
      (declare (ignore xe) (simple-vector xs) (fixnum xs0))
      (sb-kernel:with-array-data ((ys (op-ys out)) (ys0) (ye))
        (declare (ignore ye) (simple-vector ys) (fixnum ys0))
        (sb-kernel:with-array-data ((on (op-on out)) (on0) (oe))
          (declare (ignore oe) (simple-vector on) (fixnum on0))
          (let ((contours (make-array (length ends))))
            (dotimes (c (length ends) contours)
              (let* ((end (aref ends c))
                     (contour (make-array (max 0 (- (1+ end) start)))))
                (declare (fixnum end))
                (loop for i of-type fixnum from start to end
                      for j of-type fixnum from 0
                      do (setf (svref contour j)
                               (make-control-point (svref xs (+ xs0 i)) (svref ys (+ ys0 i))
                                                   (svref on (+ on0 i)))))
                (setf start (1+ end))
                (setf (svref contours c) contour)))))))))
