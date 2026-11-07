;;;; hint-test.lisp — TrueType hinting compared with FreeType's
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; Compares the outlines the bytecode interpreter produces (HINT-OUTLINE-26DOT6)
;;; and the hinted advance widths (HINTED-ADVANCE-WIDTH) with those of
;;; FreeType 2.15's v40 interpreter, recorded in fonts/reference/ by
;;; freetype-hinted-dump.py:
;;;
;;;   python3 -I freetype-hinted-dump.py fonts/LiberationSans-Regular.ttf 9,12,16,24 0 2500 7
;;;   python3 -I freetype-hinted-dump.py DejaVuSans.ttf 9,12,16 0 599 3
;;;   python3 -I freetype-hinted-dump.py fonts/LiberationSans-Regular.ttf 12,17,24 0 1500 21 advance
;;;
;;; (the outlines of Liberation Sans: every 21st glyph of those, plus a few
;;; from a wider recording; the advances in fonts/reference/*-advances.txt).
;;; Outlines in *EXCLUSIONS*, if any, differ still, and the test checks that
;;; they do, so that a fix, or a change that moves other glyphs, is noticed:
;;; when an excluded outline starts to match, remove it from the list.
;;; FreeType's loading is the default one (FT_LOAD_NO_BITMAP | FT_LOAD_NO_AUTOHINT,
;;; so its v40 interpreter, with no subpixel rendering).  DejaVu Sans is not
;;; among the test fonts, so its part is skipped when the font is not
;;; installed.  Loaded after charstring-test.lisp, whose check macros it uses.

(defpackage #:runes/otf/hint-test
  (:use #:cl)
  (:import-from #:runes/otf/charstring-test #:check)
  (:export #:run-hint-tests))

(in-package #:runes/otf/hint-test)

(defparameter *exclusions*
  '(("DejaVuSans")
    ("LiberationSans"))
  "For each font, the glyphs FreeType hints differently still, as lists
(GID PPEM...), with the sizes (ppem) at which they do.  None is known now.")

(defun excluded-p (name gid ppem)
  (member ppem (cdr (assoc gid (cdr (assoc name *exclusions* :test #'string=))))))

(defvar *tests-dir*
  (make-pathname :directory (pathname-directory *load-truename*)))

(defvar *dejavu-candidates*
  '("/usr/share/fonts/TTF/DejaVuSans.ttf"
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
    "/usr/share/fonts/dejavu/DejaVuSans.ttf"
    "/usr/share/fonts/dejavu-sans-fonts/DejaVuSans.ttf"
    "/Library/Fonts/DejaVuSans.ttf"))

(defun read-reference (name)
  "The recorded entries as lists (PPEM GID X0 Y0 X1 Y1 ...)."
  (with-open-file (in (merge-pathnames (format nil "fonts/reference/~A-hinted.txt" name) *tests-dir*))
    (loop for line = (read-line in nil) while line
          collect (with-input-from-string (s line)
                    (loop for x = (read s nil) while x collect x)))))

(defun hinted-points (loader ppem gid)
  "X0 Y0 X1 Y1 ... of the hinted glyph GID, in 26.6 pixels, or :ERROR."
  (handler-case
      (multiple-value-bind (xs ys) (zpb-ttf:hint-outline-26dot6 loader gid ppem)
        (loop for x across xs for y across ys collect x collect y))
    (error () :error)))

(defun compare-hinted (loader name)
  "Return the numbers of recorded entries that are expected to match and that
do not, and of the excluded entries that match after all, printing the first
few of each."
  (let ((count 0) (bad 0) (fixed 0))
    (dolist (entry (read-reference name))
      (destructuring-bind (ppem gid &rest expected) entry
        (let ((same (equal (hinted-points loader ppem gid) expected)))
          (cond ((excluded-p name gid ppem)
                 (when same
                   (incf fixed)
                   (when (<= fixed 3)
                     (format t "  now matches: glyph ~D at ~D ppem~%" gid ppem))))
                (t
                 (incf count)
                 (unless same
                   (incf bad)
                   (when (<= bad 3)
                     (format t "  differs: glyph ~D at ~D ppem~%" gid ppem))))))))
    (values count bad fixed)))

(defun compare-advances (loader name)
  "Return the number of recorded hinted advance widths and how many differ."
  (let ((count 0) (bad 0))
    (with-open-file (in (merge-pathnames (format nil "fonts/reference/~A-advances.txt" name) *tests-dir*))
      (loop for line = (read-line in nil) while line
            do (destructuring-bind (ppem gid advance)
                   (with-input-from-string (s line)
                     (loop for x = (read s nil) while x collect x))
                 (incf count)
                 (unless (eql advance
                              (round (* 64 (zpb-ttf:hinted-advance-width
                                            (zpb-ttf:index-glyph gid loader) ppem))))
                   (incf bad)
                   (when (<= bad 3)
                     (format t "  advance differs: glyph ~D at ~D ppem~%" gid ppem))))))
    (values count bad)))

(defun run-hint-tests ()
  (let ((runes/otf/charstring-test::*failures* 0)
        (runes/otf/charstring-test::*count* 0))
    (format t "========================================================~%")
    (format t "TrueType Hinting Against FreeType 2.15~%")
    (format t "========================================================~%")
    (flet ((compare (label path name)
             (let ((loader (zpb-ttf:open-font-loader path)))
               (unwind-protect
                    (multiple-value-bind (count bad fixed) (compare-hinted loader name)
                      (check (format nil "~A: ~D hinted outlines match FreeType" label count)
                             (zerop bad))
                      (check (format nil "~A: the excluded outlines still differ" label)
                             (zerop fixed)))
                    (multiple-value-bind (count bad) (compare-advances loader name)
                      (check (format nil "~A: ~D hinted advances match FreeType" label count)
                             (zerop bad)))
                 (zpb-ttf:close-font-loader loader)))))
      (compare "LiberationSans-Regular"
               (merge-pathnames "fonts/LiberationSans-Regular.ttf" *tests-dir*)
               "LiberationSans")
      (let ((dejavu (find-if #'probe-file *dejavu-candidates*)))
        (if dejavu
            (compare "DejaVu Sans" dejavu "DejaVuSans")
            (format t "  [SKIP] DejaVu Sans is not installed.~%"))))
    (format t "~%Results: ~D tests run, ~D failures.~%"
            runes/otf/charstring-test::*count* runes/otf/charstring-test::*failures*)
    (unless (zerop runes/otf/charstring-test::*failures*)
      (error "hinting test suite failed with ~D failure(s)" runes/otf/charstring-test::*failures*))
    t))
