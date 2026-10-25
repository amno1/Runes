;;;; cff-test.lisp — Tests for CFF/CFF2 DICTs, font matrices and fonts
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; Ported from the tests in Fontations' read-fonts src/ps/num.rs,
;;; src/ps/cff/dict.rs and src/ps/transform.rs, plus checks on the bundled
;;; CFF and CFF2 fonts. Loaded after charstring-test.lisp, whose helpers and
;;; CFF2 example table it uses.

(defpackage #:zpb-otf/cff-test
  (:use #:cl #:zpb-otf/charstring)
  (:import-from #:zpb-otf/charstring-test
                #:check #:check-error #:f #:octets #:*cff2-example* #:example-blend)
  (:export #:run-cff-tests))

(in-package #:zpb-otf/cff-test)

(defvar *test-fonts-dir*
  (merge-pathnames "fonts/" (make-pathname :directory (pathname-directory *load-truename*))))

(defun cursor (&rest bytes)
  (zpb-otf/charstring::charstring-cursor (apply #'octets bytes)))

(defun bcd (&rest bytes)
  (zpb-otf/charstring::parse-bcd (apply #'cursor bytes)))

(defun value (bcd &optional scale) (zpb-otf/charstring::bcd-value bcd scale))

(defun dynamic (bcd) (multiple-value-list (zpb-otf/charstring::bcd-dynamic-value bcd)))

(defun entries (bytes &rest args) (apply #'dict-entries (apply #'octets bytes) args))

(defun example-slice (start end)
  "Bytes START to END inclusive of the CFF2 example table."
  (subseq *cff2-example* start (1+ end)))

(defun test-numbers ()
  (format t "~%--- DICT numbers (num.rs) ---~%")
  (check "int_operands"
         (flet ((p (b0 &rest bytes)
                  (zpb-otf/charstring::parse-int (apply #'cursor bytes) b0)))
           (equal (list (p 32) (p 246) (p 247 0) (p 250 255) (p 251 0) (p 254 255))
                  '(-107 107 108 1131 -108 -1131))))
  (check "binary_coded_decimal_operands"
         (and (= (value (bcd #xe2 #xa2 #x5f)) (f -2.25))
              (= (value (bcd #x0a #x14 #x05 #x41 #xc3 #xff)) (f 0.140541d-3))
              ;; FreeType's precision for 375e-4
              (= (value (bcd #x37 #x5c #x4f)) (f 0.0370025634765625d0))))
  (check "scaled_binary_coded_decimal_operands"
         (and (= (value (bcd #xA #x06 #x25 #xf) t) (f 0.0625))
              (= (value (bcd #x37 #x5c #x4f) t) (f 0.037506103515625d0))))
  (check "dynamically_scaled_binary_coded_decimal_operands"
         (and (equal (dynamic (bcd #xA #x06 #x25 #xf)) (list (f 6250) -5))
              (equal (dynamic (bcd #x37 #x5c #x4f)) (list (f 375) -4))
              (equal (dynamic (bcd #xa0 #x1 #x95 #x31 #x25 #xff)) (list 1280000000 -7))))
  (check "binary_coded_decimal_exponent_overflow_consumes_number"
         (flet ((parsed (&rest bytes)
                  (let ((c (apply #'cursor bytes)))
                    (list (value (zpb-otf/charstring::parse-bcd c))
                          (zpb-otf/charstring::cursor-pos c)))))
           (and (equal (parsed #x1b #x10 #x01 #x00 #xff #x8b) (list #x7FFFFFFF 5))
                (equal (parsed #xe1 #xb1 #x00 #x10 #x0f) (list (- #x7FFFFFFF) 5))
                (equal (parsed #x1c #x10 #x01 #x00 #xff) (list 0 5))
                (equal (parsed #x0b #x10 #x01 #x00 #xff) (list 0 5)))))
  (check "blue_scale_fraction_length_of_0"
         (= (value (bcd #x37 #xC3 #xFF) t) (f 0.0370025634765625d0))))

(defun test-matrices ()
  (format t "~%--- Font matrices (transform.rs) ---~%")
  (check "normalize_matrix"
         (let ((m (zpb-otf/charstring::normalize-font-matrix
                   (zpb-otf/charstring::make-scaled-font-matrix
                    :elements (vector 65536 0 0 32768 0 0) :scale 1))))
           (and (equalp (font-matrix-elements m) #(131072 0 0 65536 0 0))
                (= (font-matrix-scale m) 2))))
  (check "combine_matrix"
         (equalp (zpb-otf/charstring::combine-scaled
                  (map 'vector #'f '(0.5 0.75 -1.0 2.0 0.0 0.0))
                  (map 'vector #'f '(1.5 -1.0 0.25 -1.0 1.0 2.0))
                  1)
                 (map 'vector #'f '(1.75 -0.875 1.125 -1.8125 -1.5 4.75))))
  (check "degenerate matrices"
         (and (zpb-otf/charstring::matrix-degenerate-p #(0 0 0 0 0 0))
              (zpb-otf/charstring::matrix-degenerate-p (vector #x10000 #x10000 #x10000 #x10000 0 0))
              (not (zpb-otf/charstring::matrix-degenerate-p (vector #x10000 0 0 #x10000 0 0))))))

(defun test-dicts ()
  (format t "~%--- DICTs (dict.rs) ---~%")
  (check "example_top_dict_entries"
         (equal (dict-entries (example-slice #x5 #xB))
                '((:fd-array . 68) (:charstrings . 56) (:vstore . 16))))
  (check "example_private_dict_entries"
         (equal (dict-entries (example-slice #x4f #xc0) :blend-state (example-blend 0))
                (list (cons :blue-values (mapcar #'f '(-20 0 472 490 525 540 645 660 670 690 730 750)))
                      (cons :other-blues (mapcar #'f '(-250 -240)))
                      (cons :family-blues (mapcar #'f '(-20 0 473 491 525 540 644 659 669 689 729 749)))
                      (cons :family-other-blues (mapcar #'f '(-249 -239)))
                      (cons :blue-scale (f 0.037506103515625d0))
                      (cons :blue-fuzz 0)
                      (cons :std-hw (f 55))
                      (cons :std-vw (f 80))
                      (cons :stem-snap-h (mapcar #'f '(40 55)))
                      (cons :stem-snap-v (mapcar #'f '(80 90)))
                      (cons :subrs 114))))
  (check "unhinted_private_entries_preserve_blends"
         (loop for coord in '(-0.75 0 0.25 0.75)
               always (equal (dict-entries (example-slice #x4f #xc0) :blend-state (example-blend coord)
                                                                    :unhinted-private t)
                             (remove-if-not (lambda (e) (member (car e) '(:subrs :vsindex :default-width-x
                                                                         :nominal-width-x)))
                                            (dict-entries (example-slice #x4f #xc0)
                                                          :blend-state (example-blend coord))))))
  (check "unhinted_private_entries_preserve_stack_and_special_operators"
         (equal (entries '(142 143 6           ; ignored BlueValues
                           10                  ; ignored StdHW with a missing operand
                           30 10 0 31 12 9     ; BlueScale with its special BCD handling
                           28 2 88 20          ; default width 600
                           30 10 0 31 139 139 30 10 0 31 139 139 12 7 ; FontMatrix
                           140 255             ; unknown operator must clear its operand
                           159 21              ; nominal width 20
                           149 19)             ; subrs offset 10
                         :unhinted-private t)
                (list (cons :default-width-x (f 600)) (cons :nominal-width-x (f 20)) '(:subrs . 10))))
  (check "private_dict_range_avoid_overflow"
         ;; -1 -1 Private: a negative range is reported, not followed.
         (eq :error (car (first (entries '(29 #xFF #xFF #xFF #xFF 29 #xFF #xFF #xFF #xFF 18))))))
  (check "read_font_matrix"
         ;; From ttx: 0.001 0 0.000167 0.001 0 0, scaled by 1000 as FreeType does
         (let ((m (cdr (first (entries '(30 10 0 31 139 30 10 0 1 103 255 30 10 0 31 139 139 12 7))))))
           (equalp (font-matrix-elements m)
                   (vector #x10000 0 (f 0.167007446289062d0) #x10000 0 0))))
  (check "parse_degenerate_font_matrix"
         (null (entries '(30 #x0F 30 #x0F 30 #x0F 30 #x0F 30 #x0F 30 #x0F 12 7))))
  (check-error "blend in a DICT without a blend state is reported" charstring-missing-resource
               (let ((e (assoc :error (dict-entries (example-slice #x4f #xc0)))))
                 (when e (error (cdr e))))))

;;; Fonts

(defclass point-sink ()
  ((points :initform '() :accessor points)
   (commands :initform 0 :accessor commands)))
(defmethod sink-move-to ((s point-sink) x y) (incf (commands s)) (push (cons x y) (points s)))
(defmethod sink-line-to ((s point-sink) x y) (incf (commands s)) (push (cons x y) (points s)))
(defmethod sink-curve-to ((s point-sink) a b c d x y)
  (declare (ignore a b c d))
  (incf (commands s)) (push (cons x y) (points s)))
(defmethod sink-close ((s point-sink)))

(defun open-test-font (name)
  "The zpb-ttf loader for NAME and its CFF font."
  (let* ((loader (zpb-ttf:open-font-loader (merge-pathnames name *test-fonts-dir*)))
         (info (or (zpb-ttf::table-info "CFF2" loader) (zpb-ttf::table-info "CFF " loader))))
    (values loader
            (open-cff-font (zpb-otf:font-data-slice (zpb-ttf::font-data loader)
                                                    (zpb-ttf::offset info) (zpb-ttf::size info))
                           :upem (zpb-ttf:units/em loader)))))

(defun draw-all (loader font coords)
  "Draw every glyph; return the command count, the number of glyphs whose
width matches 'hmtx', and the number of on-path points outside 'head'."
  (let ((commands 0) (widths-ok 0) (outside 0)
        (bbox (zpb-ttf:bounding-box loader))
        (subfonts (make-hash-table)))
    (dotimes (gid (cff-num-glyphs font))
      (let* ((index (cff-subfont-index font gid))
             (subfont (or (gethash index subfonts)
                          (setf (gethash index subfonts) (cff-subfont font index coords))))
             (sink (make-instance 'point-sink))
             (width (cff-draw-glyph font subfont gid sink coords)))
        (incf commands (commands sink))
        (when (and width (= (/ width 65536) (zpb-ttf::bounded-aref (zpb-ttf::advance-widths loader) gid)))
          (incf widths-ok))
        (dolist (p (points sink))
          (unless (and (<= (aref bbox 0) (/ (car p) 65536) (aref bbox 2))
                       (<= (aref bbox 1) (/ (cdr p) 65536) (aref bbox 3)))
            (incf outside)))))
    (values commands widths-ok outside)))

(defun test-fonts ()
  (format t "~%--- CFF and CFF2 fonts ---~%")
  (check "the CFF2 example table opens"
         (let ((font (open-cff-font (zpb-otf:make-font-data *cff2-example*))))
           (and (cff-font-cff2-p font) (eq (cff-font-kind font) :cff2)
                (= 1 (cff-num-subfonts font))
                ;; The Private DICT says Subrs at 114 from its start.
                (zpb-otf/charstring::cff-subfont-subrs (cff-subfont font 0 '(0))))))
  (multiple-value-bind (loader font) (open-test-font "base.ttf")
    (unwind-protect
         (multiple-value-bind (commands widths-ok outside) (draw-all loader font #())
           (check "base.ttf: a CID-keyed CFF font with 2 subfonts"
                  (and (not (cff-font-cff2-p font)) (eq (cff-font-kind font) :cid)
                       (= 2 (cff-num-subfonts font)) (= 12 (cff-num-glyphs font))))
           (check "base.ttf: every glyph draws, inside the head bounding box"
                  (and (= commands 494) (zerop outside)))
           (check "base.ttf: charstring widths match hmtx" (= widths-ok 12)))
      (zpb-ttf:close-font-loader loader)))
  (multiple-value-bind (loader font) (open-test-font "Cantarell-VF.otf")
    (unwind-protect
         (progn
           (check "Cantarell-VF: a CFF2 font with a variation store"
                  (and (cff-font-cff2-p font) (= 1307 (cff-num-glyphs font))))
           (loop for coord in '(-1 0 1)
                 do (check (format nil "Cantarell-VF: every glyph draws at wght ~D" coord)
                           (= 22656 (draw-all loader font (list coord))))))
      (zpb-ttf:close-font-loader loader))))

;;; Outlines through zpb-ttf

(defun cubic-at (p0 p1 p2 p3 u)
  (flet ((c (a b c d) (+ (* (expt (- 1 u) 3) a) (* 3 (expt (- 1 u) 2) u b) (* 3 (- 1 u) u u c) (* u u u d))))
    (cons (c (car p0) (car p1) (car p2) (car p3)) (c (cdr p0) (cdr p1) (cdr p2) (cdr p3)))))

(defun quadratic-error (p0 p1 p2 p3)
  "Largest sampled distance between cubic P0..P3 and its quadratic pieces."
  (let ((pieces (zpb-ttf::cubic->quadratics p0 p1 p2 p3 zpb-ttf::*cubic-tolerance*))
        (start p0) (worst 0))
    (loop for (control . end) in pieces for k from 0
          do (loop for i to 8
                   for u = (/ i 8)
                   for q = (cons (+ (* (expt (- 1 u) 2) (car start)) (* 2 (- 1 u) u (car control)) (* u u (car end)))
                                 (+ (* (expt (- 1 u) 2) (cdr start)) (* 2 (- 1 u) u (cdr control)) (* u u (cdr end))))
                   for c = (cubic-at p0 p1 p2 p3 (/ (+ k u) (length pieces)))
                   do (setf worst (max worst (sqrt (float (+ (expt (- (car q) (car c)) 2)
                                                             (expt (- (cdr q) (cdr c)) 2))
                                                          1d0)))))
             (setf start end))
    worst))

(defun test-outlines ()
  (format t "~%--- CFF outlines through zpb-ttf ---~%")
  (zpb-ttf:with-font-loader (loader (merge-pathnames "Cantarell-VF.otf" *test-fonts-dir*))
    (let* ((glyph (zpb-ttf:find-glyph #\O loader))
           (outline (zpb-ttf:cubic-contours glyph))
           (worst 0))
      (check "a CFF2 glyph has contours and a bounding box around them"
             (let ((bbox (zpb-ttf:bounding-box glyph)))
               (and (= 2 (zpb-ttf:contour-count glyph))
                    (< (aref bbox 0) (aref bbox 2))
                    (< (aref bbox 1) (aref bbox 3))
                    (loop for contour across (zpb-ttf::contours glyph)
                          always (loop for p across contour
                                       always (and (<= (aref bbox 0) (zpb-ttf:x p) (aref bbox 2))
                                                   (<= (aref bbox 1) (zpb-ttf:y p) (aref bbox 3))))))))
      (loop for contour across outline
            do (let ((current nil))
                 (loop for command across contour
                       do (if (eq (first command) :curve)
                              (destructuring-bind (a b c d x y) (rest command)
                                (setf worst (max worst (quadratic-error current (cons a b) (cons c d) (cons x y)))
                                      current (cons x y)))
                              (setf current (cons (second command) (third command)))))))
      (check "its quadratic contours stay within the tolerance of the cubics"
             (<= worst zpb-ttf::*cubic-tolerance*))
      (check "do-contour-segments walks a CFF contour"
             (let ((n 0))
               (zpb-ttf:do-contour-segments (p0 p1 p2) (zpb-ttf:contour glyph 0)
                 (when (and p0 p2) (incf n)))
               (plusp n)))))
  (zpb-ttf:with-font-loader (loader (merge-pathnames "LiberationSans-Regular.ttf" *test-fonts-dir*))
    (let* ((glyph (zpb-ttf:find-glyph #\O loader))
           (outline (zpb-ttf:cubic-contours glyph)))
      (check "cubic-contours of a TrueType glyph traces its contours"
             (and (= (length outline) (zpb-ttf:contour-count glyph))
                  (every (lambda (contour) (eq :move (first (aref contour 0)))) outline))))))

(defun run-cff-tests ()
  (let ((zpb-otf/charstring-test::*failures* 0)
        (zpb-otf/charstring-test::*count* 0))
    (format t "========================================================~%")
    (format t "CFF DICT, Font Matrix and Font Tests~%")
    (format t "========================================================~%")
    (test-numbers)
    (test-matrices)
    (test-dicts)
    (test-fonts)
    (test-outlines)
    (format t "~%Results: ~D tests run, ~D failures.~%"
            zpb-otf/charstring-test::*count* zpb-otf/charstring-test::*failures*)
    (unless (zerop zpb-otf/charstring-test::*failures*)
      (error "CFF test suite failed with ~D failure(s)" zpb-otf/charstring-test::*failures*))
    t))
