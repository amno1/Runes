;;;; util.lisp — Utility routines for zpb-ttf
;;;; Copyright (c) 2006 Zachary Beane, 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

(in-package #:zpb-ttf)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(declaim (inline make-control-point))

;; A structure, not a class: a hinted or scaled outline makes one per point,
;; and an instance of a class costs several times as much to make and to
;; read.  The accessors are X, Y and ON-CURVE-P as they always were.
(defstruct (control-point (:constructor make-control-point (x y on-curve-p))
                          (:conc-name nil)
                          (:copier nil))
  (x nil)
  (y nil)
  (on-curve-p nil :read-only t))

(declaim (inline bounded-aref (setf bounded-aref)))
(defun bounded-aref (vector index)
  "Return the element at INDEX, or the last element if INDEX is past the end."
  (declare (type (or null simple-vector) vector) (fixnum index))
  (if (or (null vector) (zerop (length vector)))
      0
      (aref vector (min (1- (length vector)) (max 0 index)))))

(defun (setf bounded-aref) (new-value vector index)
  (declare (type (or null simple-vector) vector) (fixnum index))
  (when (and vector (plusp (length vector)))
    (setf (aref vector (min (1- (length vector)) (max 0 index))) new-value)))

(defun make-transformer (a b c d e f)
  "Given the elements of the transformation matrix specified by A, B,
C, D, E, and F, return a function of two arguments that returns the
arguments transformed as multiple values. The offsets E and F are
applied unscaled, as the OpenType spec requires by default."
  ;; Matrix elements and coordinates may be integers or floats.
  (declare (type real a b c d e f)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (lambda (x y)
    (declare (type real x y))
    (values (round (+ (* a x) (* c y) e))
            (round (+ (* b x) (* d y) f)))))

(defun transform-contours (fn contours)
  (declare (type function fn)
           (type simple-vector contours))
  (loop for contour of-type simple-vector across contours do
        (loop for p across contour do
              (multiple-value-bind (new-x new-y) (funcall fn (x p) (y p))
                (setf (x p) new-x
                      (y p) new-y))))
  contours)

(declaim (ftype (function (simple-string) (values (unsigned-byte 32) &optional))
                tag->number))

(defun tag->number (tag)
  "Convert the four-character string TAG to a 32-bit number based on
the CHAR-CODE of each character."
  (declare (simple-string tag))
  (loop for char across tag
        for offset from 24 downto 0 by 8
        summing (ash (logand #xFF (char-code char)) offset) of-type (unsigned-byte 32)))

(defun number->tag (number)
  "Convert the 32-bit NUMBER to a string of four characters based on
the CODE-CHAR of each octet in the number."
  (declare (type (unsigned-byte 32) number))
  (let ((tag (make-string 4)))
    (loop for i below 4
          for offset from 24 downto 0 by 8
          do (setf (schar tag i)
                   (code-char (ldb (byte 8 offset) number))))
    tag))
