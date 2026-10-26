;;;; gvar-test.lisp — TrueType outlines compared with FreeType's
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; Compares zpb-ttf's contours with the unscaled outlines FreeType 2.13
;;; produces, as dumped by Fontations' extract_glyphs.py into
;;; fonts/reference/. Loaded after charstring-test.lisp, whose check macros
;;; it uses.

(defpackage #:zpb-ttf/gvar-test
  (:use #:cl)
  (:import-from #:zpb-otf/charstring-test #:check)
  (:export #:run-gvar-tests))

(in-package #:zpb-ttf/gvar-test)

(defvar *test-fonts-dir*
  (merge-pathnames "fonts/" (make-pathname :directory (pathname-directory *load-truename*))))

(defun numbers (string)
  (with-input-from-string (s (substitute #\Space #\, string))
    (loop for x = (read s nil) while x collect x)))

(defun read-reference (name)
  "Unscaled entries of a reference dump, as plists (:gid :coords :ends :points :tags)."
  (with-open-file (in (merge-pathnames (format nil "reference/~A-glyphs.txt" name) *test-fonts-dir*))
    (let ((entries '()) (current nil))
      (loop for line = (read-line in nil) while line
            do (let* ((space (position #\Space line))
                      (key (subseq line 0 space))
                      (rest (if space (subseq line (1+ space)) "")))
                 (cond
                   ((string= key "glyph")
                    (destructuring-bind (gid size) (subseq (numbers (substitute #\Space #\n rest)) 0 2)
                      (setf current (when (zerop size) (list :gid gid :coords '())))
                      (when current (push current entries))))
                   ((null current))
                   ((string= key "coords") (setf (getf (car entries) :coords) (numbers rest)))
                   ((string= key "contours") (setf (getf (car entries) :ends) (numbers rest)))
                   ((string= key "points")
                    (setf (getf (car entries) :points)
                          (loop for (x y) on (numbers rest) by #'cddr collect (cons x y))))
                   ((string= key "tags") (setf (getf (car entries) :tags) (numbers rest))))))
      (nreverse entries))))

(defun glyph-outline (loader gid)
  "Contour ends, points and on-curve tags of glyph GID, flattened as FreeType reports them."
  (let ((ends '()) (points '()) (tags '()) (n 0))
    (loop for contour across (zpb-ttf:contours (zpb-ttf:index-glyph gid loader))
          do (loop for p across contour
                   do (push (cons (zpb-ttf:x p) (zpb-ttf:y p)) points)
                      (push (if (zpb-ttf:on-curve-p p) 1 0) tags)
                      (incf n))
             (push (1- n) ends))
    (list (nreverse ends) (nreverse points) (nreverse tags))))

(defun compare-font (name &key (report 3))
  "Compare every unscaled reference entry of NAME. Return the number of
entries and the number that differ, printing the first REPORT differences."
  (let ((entries (read-reference name)) (bad 0))
    (zpb-ttf:with-font-loader (loader (merge-pathnames (format nil "~A.ttf" name) *test-fonts-dir*))
      (dolist (e entries)
        (zpb-ttf::set-instance loader nil (when (some (lambda (c) (/= c 0)) (getf e :coords))
                                            (mapcar #'rational (getf e :coords))))
        (destructuring-bind (ends points tags) (handler-case (glyph-outline loader (getf e :gid))
                                                 (error (c) (list :error c nil)))
          (unless (and (equal ends (getf e :ends)) (equal points (getf e :points)) (equal tags (getf e :tags)))
            (incf bad)
            (when (plusp report)
              (decf report)
              (format t "    ~A glyph ~D at ~A:~%      want ~A~%      got  ~A~%"
                      name (getf e :gid) (getf e :coords)
                      (subseq (getf e :points) 0 (min 6 (length (getf e :points))))
                      (if (eq ends :error) points (subseq points 0 (min 6 (length points))))))))))
    (values (length entries) bad)))

(defun run-gvar-tests ()
  (let ((zpb-otf/charstring-test::*failures* 0)
        (zpb-otf/charstring-test::*count* 0))
    (format t "========================================================~%")
    (format t "TrueType Outlines Against FreeType 2.13~%")
    (format t "========================================================~%")
    (dolist (name '("glyf_components" "vazirmatn_var_trimmed" "material_symbols_subset"
                    "hvar_with_truncated_adv_index_map"))
      (multiple-value-bind (count bad) (compare-font name)
        (check (format nil "~A: ~D outlines match FreeType" name count) (zerop bad))))
    (format t "~%Results: ~D tests run, ~D failures.~%"
            zpb-otf/charstring-test::*count* zpb-otf/charstring-test::*failures*)
    (unless (zerop zpb-otf/charstring-test::*failures*)
      (error "gvar test suite failed with ~D failure(s)" zpb-otf/charstring-test::*failures*))
    t))
