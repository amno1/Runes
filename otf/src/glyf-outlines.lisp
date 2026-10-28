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

(defstruct (outline-points (:conc-name op-))
  (xs (make-array 32 :adjustable t :fill-pointer 0))
  (ys (make-array 32 :adjustable t :fill-pointer 0))
  (on (make-array 32 :adjustable t :fill-pointer 0))
  ;; End point of each contour, as indices into the arrays above.
  (ends (make-array 8 :adjustable t :fill-pointer 0)))

(defun loader-gvar-coords (loader)
  "The instance as raw 2.14 coordinates when 'gvar' applies to it, else NIL."
  (let ((coords (normalized-coordinates loader)))
    (when (and coords (table-info "gvar" loader))
      (map 'vector (lambda (c) (zpb-otf/charstring::f2dot14-from-real c)) coords))))

;;; Reading a simple glyph

(defun read-simple-points (fd pos contour-count)
  "End points, x and y coordinates and on-curve flags of the simple glyph
whose contour count is at POS - 10, as four vectors."
  (let ((ends (make-array contour-count)))
    (dotimes (i contour-count)
      (setf (aref ends i) (zpb-otf:font-data-read-u16 fd pos))
      (incf pos 2))
    (let* ((n (if (zerop contour-count) 0 (1+ (aref ends (1- contour-count)))))
           (flags (make-array n :element-type '(unsigned-byte 8)))
           (xs (make-array n))
           (ys (make-array n)))
      (incf pos (+ 2 (zpb-otf:font-data-read-u16 fd pos)))
      (let ((i 0))
        (loop while (< i n)
              do (let ((flag (zpb-otf:font-data-read-u8 fd pos)))
                   (incf pos)
                   (setf (aref flags i) flag)
                   (incf i)
                   (when (logbitp 3 flag)
                     (let ((repeats (zpb-otf:font-data-read-u8 fd pos)))
                       (incf pos)
                       (loop repeat repeats
                             while (< i n)
                             do (setf (aref flags i) flag)
                                (incf i)))))))
      (flet ((coordinates (out short-bit same-bit)
               (let ((current 0))
                 (dotimes (j n)
                   (let ((flag (aref flags j)))
                     (incf current
                           (cond ((logbitp short-bit flag)
                                  (let ((v (zpb-otf:font-data-read-u8 fd pos)))
                                    (incf pos)
                                    (if (logbitp same-bit flag) v (- v))))
                                 ((logbitp same-bit flag) 0)
                                 (t (prog1 (zpb-otf:font-data-read-i16 fd pos) (incf pos 2)))))
                     (setf (aref out j) current))))))
        (coordinates xs 1 4)
        (coordinates ys 2 5))
      (values ends xs ys (map 'vector (lambda (f) (logbitp 0 f)) flags)))))

;;; Phantom points

(defun phantom-points (loader gid x-min y-max)
  "The four phantom points of GID as a vector of (x . y), from its metrics."
  (let* ((lsb (bounded-aref (left-side-bearings loader) gid))
         (advance (bounded-aref (advance-widths loader) gid))
         (ascent (slot-value loader 'ascender))
         (descent (slot-value loader 'descender)))
    (multiple-value-bind (tsb vadvance)
        (if (vmtx-missing-p loader)
            (values (- ascent y-max) (- ascent descent))
            (values (bounded-aref (top-side-bearings loader) gid)
                    (bounded-aref (advance-heights loader) gid)))
      (vector (cons (- x-min lsb) 0)
              (cons (+ (- x-min lsb) advance) 0)
              (cons 0 (+ y-max tsb))
              (cons 0 (- (+ y-max tsb) vadvance))))))

(defun add-phantom-deltas (phantom dx dy start)
  "Phantom points moved by the rounded deltas at START of DX and DY."
  (map 'vector (lambda (p i)
                 (cons (+ (car p) (fixed-round (aref dx (+ start i))))
                       (+ (cdr p) (fixed-round (aref dy (+ start i))))))
       phantom #(0 1 2 3)))

;;; Loading

(defun load-glyph-points (loader gid out coords)
  "Append the points of glyph GID to OUT; return its phantom points."
  (when (> (length *compound-glyph-stack*) *max-compound-glyph-depth*)
    (error 'unsupported-value :description "Compound glyph nesting too deep"
                              :location (format nil "glyph ~D" gid)
                              :actual-value (length *compound-glyph-stack*)
                              :expected-values (list *max-compound-glyph-depth*)))
  (when (member gid *compound-glyph-stack*)
    (error 'unsupported-value :description "Recursive compound glyph"
                              :location (format nil "glyph ~D" (first *compound-glyph-stack*))
                              :actual-value gid
                              :expected-values '("a glyph that does not contain itself")))
  (multiple-value-bind (g-off len) (glyph-loca-info loader gid)
    (let ((glyf (table-info "glyf" loader))
          (fd (font-data loader)))
      (if (or (zerop len) (null glyf))
          ;; An empty glyph: only its metrics can vary.
          (let ((phantom (phantom-points loader gid 0 0))
                (data (and coords (gvar-glyph-data loader gid))))
            (if data
                (multiple-value-bind (dx dy)
                    (simple-glyph-deltas data coords (map 'vector #'car phantom) (map 'vector #'cdr phantom) '())
                  (if dx
                      (let ((moved (add-phantom-deltas phantom dx dy 0)))
                        ;; Only the horizontal phantom points take deltas here.
                        (vector (aref moved 0) (aref moved 1) (aref phantom 2) (aref phantom 3)))
                      phantom))
                phantom))
          (let* ((pos (+ (offset glyf) g-off))
                 (contour-count (zpb-otf:font-data-read-i16 fd pos))
                 (phantom (phantom-points loader gid
                                          (zpb-otf:font-data-read-i16 fd (+ pos 2))
                                          (zpb-otf:font-data-read-i16 fd (+ pos 8)))))
            (if (minusp contour-count)
                (let ((*compound-glyph-stack* (cons gid *compound-glyph-stack*)))
                  (load-composite-points loader gid (+ pos 10) phantom out coords))
                (load-simple-points loader gid fd (+ pos 10) contour-count phantom out coords)))))))

(defun load-simple-points (loader gid fd pos contour-count phantom out coords)
  (multiple-value-bind (ends xs ys on) (read-simple-points fd pos contour-count)
    (let* ((n (length xs))
           (data (and coords (gvar-glyph-data loader gid))))
      (when data
        ;; Deltas cover the points and the phantom points.
        (let ((all-x (concatenate 'vector xs (map 'vector #'car phantom)))
              (all-y (concatenate 'vector ys (map 'vector #'cdr phantom))))
          (multiple-value-bind (dx dy) (simple-glyph-deltas data coords all-x all-y (coerce ends 'list))
            (when dx
              (dotimes (i n)
                (setf (aref xs i) (+ (aref xs i) (fixed-round (aref dx i)))
                      (aref ys i) (+ (aref ys i) (fixed-round (aref dy i)))))
              (setf phantom (add-phantom-deltas phantom dx dy n))))))
      (let ((base (fill-pointer (op-xs out))))
        (dotimes (i n)
          (vector-push-extend (aref xs i) (op-xs out))
          (vector-push-extend (aref ys i) (op-ys out))
          (vector-push-extend (aref on i) (op-on out)))
        (loop for end across ends
              do (vector-push-extend (+ base end) (op-ends out))))
      phantom)))

(defun read-component (fd pos)
  "The component record at POS as a plist, and the position after it."
  (let* ((flags (zpb-otf:font-data-read-u16 fd pos))
         (glyph (zpb-otf:font-data-read-u16 fd (+ pos 2)))
         (words (logbitp 0 flags))
         (xy (logbitp 1 flags))
         (p (+ pos 4))
         arg1 arg2)
    (if words
        (setf arg1 (if xy (zpb-otf:font-data-read-i16 fd p) (zpb-otf:font-data-read-u16 fd p))
              arg2 (if xy (zpb-otf:font-data-read-i16 fd (+ p 2)) (zpb-otf:font-data-read-u16 fd (+ p 2)))
              p (+ p 4))
        (setf arg1 (if xy (zpb-otf:font-data-read-i8 fd p) (zpb-otf:font-data-read-u8 fd p))
              arg2 (if xy (zpb-otf:font-data-read-i8 fd (+ p 1)) (zpb-otf:font-data-read-u8 fd (+ p 1)))
              p (+ p 2)))
    ;; Transform elements as 16.16: the 2.14 bits times 4.
    (let ((xx #x10000) (yx 0) (xy-skew 0) (yy #x10000))
      (flet ((f2 (at) (* 4 (zpb-otf:font-data-read-i16 fd at))))
        (cond ((logbitp 3 flags) (setf xx (f2 p) yy xx) (incf p 2))
              ((logbitp 6 flags) (setf xx (f2 p) yy (f2 (+ p 2))) (incf p 4))
              ((logbitp 7 flags) (setf xx (f2 p) yx (f2 (+ p 2)) xy-skew (f2 (+ p 4)) yy (f2 (+ p 6)))
               (incf p 8))))
      (values (list :flags flags :glyph glyph :xy xy :arg1 arg1 :arg2 arg2
                    :transform (vector xx yx xy-skew yy)
                    :transformed (logtest flags #b11001000))
              p))))

(defun hypot-fixed (a b)
  (let ((a (abs a)) (b (abs b)))
    (if (> a b) (+ a (ash (* 3 b) -3)) (+ b (ash (* 3 a) -3)))))

(defun load-composite-points (loader gid pos phantom out coords)
  (let ((fd (font-data loader)))
    (let ((components '()) (p pos))
      ;; Read the component records, up to the last one.
      (loop (multiple-value-bind (component next) (read-component fd p)
              (push component components)
              (setf p next)
              (unless (logbitp 5 (getf component :flags)) (return))))
      (setf components (nreverse components))
      (let* ((count (+ (length components) 4))
             (data (and coords (gvar-glyph-data loader gid)))
             (dx nil) (dy nil)
             (point-base (fill-pointer (op-xs out))))
        (when data
          (multiple-value-setq (dx dy) (composite-glyph-deltas data coords count))
          (when dx
            (setf phantom (add-phantom-deltas phantom dx dy (- count 4)))))
        (loop for component in components
              for i from 0
              do (let* ((start (fill-pointer (op-xs out)))
                        (component-phantom (load-glyph-points loader (getf component :glyph) out coords))
                        (end (fill-pointer (op-xs out)))
                        (flags (getf component :flags))
                        (xs (op-xs out))
                        (ys (op-ys out)))
                   ;; USE_MY_METRICS takes the component's phantom points.
                   (when (logbitp 9 flags)
                     (setf phantom component-phantom))
                   (when (getf component :transformed)
                     (let ((m (getf component :transform)))
                       (loop for j from start below end
                             do (let ((x (aref xs j)) (y (aref ys j)))
                                  ;; Font units taken as raw 16.16, as FreeType does.
                                  (setf (aref xs j) (f+ (f* x (aref m 0)) (f* y (aref m 2)))
                                        (aref ys j) (f+ (f* x (aref m 1)) (f* y (aref m 3))))))))
                   (multiple-value-bind (ox oy)
                       (if (getf component :xy)
                           (let ((x (getf component :arg1)) (y (getf component :arg2))
                                 (m (getf component :transform)))
                             ;; SCALED_COMPONENT_OFFSET without UNSCALED_COMPONENT_OFFSET.
                             (when (and (getf component :transformed)
                                        (= (logand flags #x1800) #x0800))
                               (setf x (f* x (hypot-fixed (aref m 0) (aref m 2)))
                                     y (f* y (hypot-fixed (aref m 3) (aref m 1)))))
                             (if dx
                                 (values (+ x (fixed-round (aref dx i))) (+ y (fixed-round (aref dy i))))
                                 (values x y)))
                           ;; Point matching: the component's point ARG2 onto point ARG1.
                           (let ((base (+ point-base (getf component :arg1)))
                                 (child (+ start (getf component :arg2))))
                             (unless (and (< base start) (< child end))
                               (error 'unsupported-value
                                      :description "Compound glyph anchor point out of range"
                                      :location (format nil "glyph ~D" gid)
                                      :actual-value (list (getf component :arg1) (getf component :arg2))
                                      :expected-values '("existing point indices")))
                             (values (- (aref xs base) (aref xs child))
                                     (- (aref ys base) (aref ys child)))))
                     (unless (and (zerop ox) (zerop oy))
                       (loop for j from start below end
                             do (incf (aref xs j) ox)
                                (incf (aref ys j) oy))))))
        phantom))))

(defun glyph-outline-points (loader gid)
  "The outline points of GID at LOADER's instance, and its phantom points."
  (let* ((out (make-outline-points))
         (phantom (load-glyph-points loader gid out (loader-gvar-coords loader))))
    (values out phantom)))

(defun outline-points->contours (out)
  (let ((start 0))
    (map 'vector
         (lambda (end)
           (prog1 (coerce (loop for i from start to end
                                collect (make-control-point (aref (op-xs out) i) (aref (op-ys out) i)
                                                            (aref (op-on out) i)))
                          'simple-vector)
             (setf start (1+ end))))
         (op-ends out))))
