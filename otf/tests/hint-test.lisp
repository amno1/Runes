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
;;;   python3 -I freetype-hinted-dump.py /usr/share/fonts/Adwaita/AdwaitaSans-Regular.ttf 12,16,24 0 1500 13 wght=700
;;;
;;; (the last for the variable font Adwaita Sans at wght=700, see
;;; COMPARE-VARIABLE; the outlines of Liberation Sans: every 21st glyph of those, plus a few
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

(defvar *adwaita-candidates*
  '("/usr/share/fonts/Adwaita/AdwaitaSans-Regular.ttf"
    "/usr/share/fonts/adwaita-sans-fonts/AdwaitaSans-Regular.ttf"
    "/usr/share/fonts/truetype/adwaita/AdwaitaSans-Regular.ttf"))

(defun compare-variable (loader)
  "Compare the hinted outlines of a variable font's instance with FreeType's:
return the numbers of recorded entries, of those that differ, and of those that
differ by more than one 64th of a pixel in some point.

FreeType keeps the normalized axis coordinates at 16.16 and this library, like
Fontations, at 2.14, which moves a few points whose deltas are inferred by a
64th; so a few entries are not equal, and the test bounds how many."
  (let ((count 0) (differ 0) (far 0))
    (with-open-file (in (merge-pathnames "fonts/reference/AdwaitaSans-wght700-hinted.txt" *tests-dir*))
      (loop for line = (read-line in nil) while line
            do (destructuring-bind (ppem gid &rest expected)
                   (with-input-from-string (s line)
                     (loop for x = (read s nil) while x collect x))
                 (incf count)
                 (let ((got (hinted-points loader ppem gid)))
                   (unless (equal got expected)
                     (incf differ)
                     (when (or (eq got :error)
                               (/= (length got) (length expected))
                               (some (lambda (a b) (> (abs (- a b)) 1)) got expected))
                       (incf far)
                       (when (<= far 3)
                         (format t "  differs: glyph ~D at ~D ppem~%" gid ppem))))))))
    (values count differ far)))

(defun zone-snapshot (zone n)
  (list (subseq (runes/otf/tt-hint:zone-unscaled-x zone) 0 n)
        (subseq (runes/otf/tt-hint:zone-unscaled-y zone) 0 n)
        (subseq (runes/otf/tt-hint:zone-orig-x zone) 0 n)
        (subseq (runes/otf/tt-hint:zone-cur-y zone) 0 n)
        (subseq (runes/otf/tt-hint:zone-flags zone) 0 n)))

(defun check-decoder-fallback (loader)
  "The glyph decoder reads without checks when the font data holds all a glyph's
points, and with them near its end.  Decode simple glyphs with the font data
cut where each glyph's data ends, which makes the checked decoder run, and
return the numbers of glyphs decoded and of those that decode differently."
  (let* ((fd (zpb-ttf::font-data loader))
         (glyf (zpb-ttf::offset (zpb-ttf::table-info "glyf" loader)))
         (zone (runes/otf/tt-hint::allocate-zone 128 16))
         (count 0) (different 0))
    (dotimes (gid (min 1500 (zpb-ttf:glyph-count loader)))
      (multiple-value-bind (offset length) (zpb-ttf::glyph-loca-info loader gid)
        (when (plusp length)
          (let* ((pos (+ glyf offset))
                 (contours (runes/otf:font-data-read-i16 fd pos)))
            (when (plusp contours)
              (let* ((n (1+ (runes/otf:font-data-read-u16 fd (+ pos 10 (* 2 (1- contours))))))
                     (full (progn
                             (zpb-ttf::decode-simple-glyph-into-zone
                              fd (+ pos 10) contours
                              (runes/otf/tt-hint:ensure-zone-capacity zone (+ n 4) contours
                                                                      :n-glyph-points n)
                              n 18432)
                             (zone-snapshot zone n)))
                     (cut (runes/otf:font-data-slice fd 0 (+ pos length))))
                (zpb-ttf::decode-simple-glyph-into-zone
                 cut (+ pos 10) contours
                 (runes/otf/tt-hint:ensure-zone-capacity zone (+ n 4) contours :n-glyph-points n)
                 n 18432)
                (incf count)
                (unless (equalp full (zone-snapshot zone n))
                  (incf different))))))))
    (values count different)))

(defun check-engine-eviction (loader)
  "Engines are kept for at most *MAX-HINTING-SIZES* sizes of a font, the
least recently used going first, and an evicted size hints as before."
  (let ((limit zpb-ttf::*max-hinting-sizes*)
        (before (multiple-value-list (zpb-ttf:hint-outline-26dot6 loader 40 12))))
    (setf zpb-ttf::*max-hinting-sizes* 4)
    (unwind-protect
         (let ((kept 0))
           (loop for ppem from 8 to 40
                 do (zpb-ttf:hint-outline-26dot6 loader 40 ppem)
                    (setf kept (max kept (zpb-ttf::hinting-engine-sizes loader))))
           (values (<= kept 4)
                   (equalp (subseq before 0 4)
                           (subseq (multiple-value-list
                                    (zpb-ttf:hint-outline-26dot6 loader 40 12))
                                   0 4))))
      (setf zpb-ttf::*max-hinting-sizes* limit))))

(defun check-endless-program-stops ()
  "A program that jumps back to its start for ever is stopped by the instruction
limit, after about as many instructions as it, and with its condition."
  (let ((engine (runes/otf/tt-hint:make-tt-engine))
        ;; PUSHW[0] -3, JMPR: back to the PUSHW.
        (program (coerce '(#xB8 #xFF #xFD #x1C) '(simple-array (unsigned-byte 8) (*)))))
    (setf (svref (runes/otf/tt-hint::eng-programs engine) 2) program)
    (handler-case (progn (runes/otf/tt-hint:run-program engine 2) nil)
      (runes/otf/tt-hint::tt-hint-limit-exceeded () t))))

(defun check-outline-into (loader)
  "HINT-OUTLINE-26DOT6-INTO fills one buffer, through glyphs of different
sizes, with what HINT-OUTLINE-26DOT6 returns."
  (let ((outline (zpb-ttf:make-outline-26dot6)) (same t))
    (dolist (ppem '(12 16))
      (dotimes (gid (min 400 (zpb-ttf:glyph-count loader)))
        (multiple-value-bind (xs ys on ends phantom) (zpb-ttf:hint-outline-26dot6 loader gid ppem)
          (zpb-ttf:hint-outline-26dot6-into loader gid ppem outline)
          (let ((n (length xs)) (n-ends (length ends)))
            (unless (and (= n (zpb-ttf:o26-n outline))
                         (= n-ends (zpb-ttf:o26-n-ends outline))
                         (equalp xs (subseq (zpb-ttf:o26-xs outline) 0 n))
                         (equalp ys (subseq (zpb-ttf:o26-ys outline) 0 n))
                         (equalp on (subseq (zpb-ttf:o26-on-curve outline) 0 n))
                         (equalp ends (subseq (zpb-ttf:o26-ends outline) 0 n-ends))
                         (equalp phantom (zpb-ttf:o26-phantom outline)))
              (setf same nil))))))
    same))

(defun check-concurrent-hinting (loader)
  "Threads hinting at the same size and at different ones, with engines taken
from and returned to the same pools, get the outlines one thread does."
  (let* ((glyphs (loop for g below (min 300 (zpb-ttf:glyph-count loader)) collect g))
         (sizes '(12 16))
         (expected (make-hash-table :test 'equal))
         (failures 0)
         (lock (sb-thread:make-mutex)))
    (dolist (ppem sizes)
      (dolist (g glyphs)
        (setf (gethash (cons ppem g) expected)
              (multiple-value-list (zpb-ttf:hint-outline-26dot6 loader g ppem)))))
    (let ((threads
            (loop for k below 6
                  collect (let ((k k))
                            (sb-thread:make-thread
                             (lambda ()
                               (dotimes (round 4)
                                 (dolist (g (if (evenp k) glyphs (reverse glyphs)))
                                   (let ((ppem (nth (mod (+ k round) 2) sizes)))
                                     (unless (equalp (multiple-value-list
                                                      (zpb-ttf:hint-outline-26dot6 loader g ppem))
                                                     (gethash (cons ppem g) expected))
                                       (sb-thread:with-mutex (lock) (incf failures))))))))))))
      (mapc #'sb-thread:join-thread threads))
    (zerop failures)))

(defun check-instance-change (loader)
  "Hinting follows the instance of a variable font as it is changed: the first
instance again gives the first outlines, a different one other outlines."
  (flet ((outlines (weight)
           (setf (zpb-ttf:variation-coordinates loader) `(("wght" . ,weight)))
           (loop for gid in '(36 43 50 61 100)
                 collect (multiple-value-list (zpb-ttf:hint-outline-26dot6 loader gid 16)))))
    (let* ((first (outlines 400)) (other (outlines 800)) (again (outlines 400)))
      (and (equalp first again) (not (equalp first other))))))

(defun check-cff-hinted-contours (path)
  "A CFF font gives the hinted contours and advances of its glyphs, at integer
and fractional sizes: every glyph of the first 80 loads, and the ones with an
outline have points."
  (let ((loader (zpb-ttf:open-font-loader path)) (with-points 0) (ok t))
    (unwind-protect
         (dolist (ppem (list 12 12.5 25/2 16.25d0))
           (dotimes (g (min 80 (zpb-ttf:glyph-count loader)))
             (let ((glyph (zpb-ttf:index-glyph g loader)))
               (handler-case
                   (let ((contours (zpb-ttf:hinted-contours glyph ppem)))
                     (zpb-ttf:hinted-advance-width glyph ppem)
                     (zpb-ttf:hinted-cubic-contours glyph ppem)
                     (when (plusp (length contours)) (incf with-points)))
                 (error () (setf ok nil))))))
      (zpb-ttf:close-font-loader loader))
    (and ok (plusp with-points))))

(defun check-bad-glyph-index (loader)
  "A glyph index past the font's glyphs is an error, not a read past the table."
  (let ((count (zpb-ttf:glyph-count loader)))
    (and (handler-case (progn (zpb-ttf:hint-outline-26dot6 loader count 12) nil)
           (error () t))
         (handler-case (progn (zpb-ttf:hint-outline-26dot6 loader 70000 12) nil)
           (error () t)))))

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
                    (check (format nil "~A: HINT-OUTLINE-26DOT6-INTO gives the same outlines" label)
                           (check-outline-into loader))
                    (check (format nil "~A: six threads hint what one does" label)
                           (check-concurrent-hinting loader))
                    (check (format nil "~A: a glyph index out of range is an error" label)
                           (check-bad-glyph-index loader))
                    (multiple-value-bind (count different) (check-decoder-fallback loader)
                      (check (format nil "~A: ~D glyphs decode the same with and without the unchecked reads"
                                     label count)
                             (and (plusp count) (zerop different))))
                    (multiple-value-bind (bounded same) (check-engine-eviction loader)
                      (check (format nil "~A: engines are kept for a bounded number of sizes" label)
                             bounded)
                      (check (format nil "~A: an evicted size hints as before" label) same))
                    (multiple-value-bind (count bad) (compare-advances loader name)
                      (check (format nil "~A: ~D hinted advances match FreeType" label count)
                             (zerop bad)))
                 (zpb-ttf:close-font-loader loader)))))
      (compare "LiberationSans-Regular"
               (merge-pathnames "fonts/LiberationSans-Regular.ttf" *tests-dir*)
               "LiberationSans")
      (check "CFF hinted contours load (Cantarell-VF, integer and fractional sizes)"
             (check-cff-hinted-contours (merge-pathnames "fonts/Cantarell-VF.otf" *tests-dir*)))
      (check "CFF hinted contours load (Noto Sans JP subset)"
             (check-cff-hinted-contours (merge-pathnames "fonts/NotoSansJP-VF.subset.otf" *tests-dir*)))
      (let ((adwaita (find-if #'probe-file *adwaita-candidates*)))
        (if adwaita
            (let ((loader (zpb-ttf:open-font-loader adwaita)))
              (unwind-protect
                   (progn
                     (setf (zpb-ttf:variation-coordinates loader) '(("wght" . 700)))
                     (multiple-value-bind (count differ far) (compare-variable loader)
                       (check (format nil "Adwaita Sans wght=700: ~D hinted outlines, at most 16 differ from FreeType's"
                                      count)
                              (<= differ 16))
                       (check "Adwaita Sans wght=700: none differs by more than a 64th of a pixel"
                              (zerop far))
                       (check "Adwaita Sans: hinting follows a change of instance"
                              (check-instance-change loader))))
                (zpb-ttf:close-font-loader loader)))
            (format t "  [SKIP] Adwaita Sans is not installed.~%")))
      (let ((dejavu (find-if #'probe-file *dejavu-candidates*)))
        (if dejavu
            (compare "DejaVu Sans" dejavu "DejaVuSans")
            (format t "  [SKIP] DejaVu Sans is not installed.~%"))))
    (check "an endless TrueType program is stopped by the instruction limit"
           (check-endless-program-stops))
    (format t "~%Results: ~D tests run, ~D failures.~%"
            runes/otf/charstring-test::*count* runes/otf/charstring-test::*failures*)
    (unless (zerop runes/otf/charstring-test::*failures*)
      (error "hinting test suite failed with ~D failure(s)" runes/otf/charstring-test::*failures*))
    t))
