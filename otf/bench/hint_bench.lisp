;;;; hint_bench.lisp — TrueType hinting speed of runes/otf
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; Prints one row per benchmark, "NAME OPERATIONS TIME ms", in the format
;;; bench3x3.py reads.  An operation is one glyph hinted at one size, the
;;; engine for that size already made and the glyph's data already in
;;; memory, as FreeType's FT_Load_Glyph is timed in ft_hint_bench.c.

(require :asdf)
(ignore-errors (load "~/quicklisp/setup.lisp"))
(let ((root (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*))))
  (push root asdf:*central-registry*)
  (push (merge-pathnames "decompress/" root) asdf:*central-registry*))
(asdf:load-system :zpb-ttf)

(defparameter *fonts-dir*
  (merge-pathnames "../tests/fonts/" (make-pathname :name nil :type nil :defaults *load-truename*)))

(defparameter *dejavu*
  ;; DEJAVU, set by run_comparison.sh, names the font FreeType is timed on.
  (find-if (lambda (path) (and path (probe-file path)))
           (list (sb-ext:posix-getenv "DEJAVU")
                 "/usr/share/fonts/TTF/DejaVuSans.ttf"
                 "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
                 "/usr/share/fonts/dejavu/DejaVuSans.ttf"
                 "/usr/share/fonts/dejavu-sans-fonts/DejaVuSans.ttf")))

(defparameter *repetitions* 20)

;; Lines starting with # are read by bench3x3.py for its disclosure.
(format t "# SBCL ~a~%" (lisp-implementation-version))

(defun now-ms ()
  "The monotonic clock, in milliseconds, to the nanosecond."
  (multiple-value-bind (seconds nanoseconds) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* seconds 1000d0) (/ nanoseconds 1d6))))

;; Each row allocates tens of megabytes (the outlines it returns).  A collection
;; falling inside one row and not the next would show as a difference of the
;; hinting, so a collection runs before each row and the nursery is large
;; enough that none runs during it.
(setf (sb-ext:bytes-consed-between-gcs) (* 512 1024 1024))

(defvar *nursery-bytes* (sb-ext:bytes-consed-between-gcs))

(defun time-ms (thunk)
  "The milliseconds THUNK takes.  It is an error if a collection ran meanwhile
(the nursery was too small for what the row allocates), as that would time
a pause that the row compared with does not have."
  (sb-ext:gc :full t)
  (let ((consed (sb-ext:get-bytes-consed))
        (gc-time sb-ext:*gc-run-time*)
        (start (now-ms)))
    (funcall thunk)
    (let ((elapsed (- (now-ms) start))
          (allocated (- (sb-ext:get-bytes-consed) consed)))
      (when (or (> allocated *nursery-bytes*) (/= gc-time sb-ext:*gc-run-time*))
        (error "a garbage collection may have run during the row: ~:D bytes allocated, nursery ~:D"
               allocated *nursery-bytes*))
      elapsed)))

(defun row (name operations ms)
  (format t "~a ~d ~,3f ms~%" name operations ms))

(defun bench-hint-outlines (name path ppem glyph-count)
  "The hinted outline of each glyph into one buffer, as FreeType loads into its
glyph slot."
  (zpb-ttf:with-font-loader (loader path)
    (let ((n (min glyph-count (zpb-ttf:glyph-count loader)))
          (outline (zpb-ttf:make-outline-26dot6)))
      ;; Warm up: the engine for the size, the buffer's size, and the code paths.
      (dotimes (g n) (zpb-ttf:hint-outline-26dot6-into loader g ppem outline))
      (row name (* n *repetitions*)
           (time-ms (lambda ()
                      (dotimes (r *repetitions*)
                        (dotimes (g n)
                          (zpb-ttf:hint-outline-26dot6-into loader g ppem outline)))))))))

(defun bench-hint-outlines-fresh (name path ppem glyph-count)
  "As BENCH-HINT-OUTLINES with the arrays of each outline new, as
HINT-OUTLINE-26DOT6 returns them."
  (zpb-ttf:with-font-loader (loader path)
    (let ((n (min glyph-count (zpb-ttf:glyph-count loader))))
      (dotimes (g n) (zpb-ttf:hint-outline-26dot6 loader g ppem))
      (row name (* n *repetitions*)
           (time-ms (lambda ()
                      (dotimes (r *repetitions*)
                        (dotimes (g n)
                          (zpb-ttf:hint-outline-26dot6 loader g ppem)))))))))

(defun bench-hinted-contours (name path ppem glyph-count)
  "What a renderer calls: the contours as control points."
  (zpb-ttf:with-font-loader (loader path)
    (let* ((n (min glyph-count (zpb-ttf:glyph-count loader)))
           (glyphs (loop for g below n collect (zpb-ttf:index-glyph g loader))))
      (dolist (g glyphs) (zpb-ttf:hinted-contours g ppem))
      (row name (* n *repetitions*)
           (time-ms (lambda ()
                      (dotimes (r *repetitions*)
                        (dolist (g glyphs) (zpb-ttf:hinted-contours g ppem)))))))))

(defun bench-hinted-advances (name path ppem glyph-count)
  (zpb-ttf:with-font-loader (loader path)
    (let* ((n (min glyph-count (zpb-ttf:glyph-count loader)))
           (glyphs (loop for g below n collect (zpb-ttf:index-glyph g loader))))
      (dolist (g glyphs) (zpb-ttf:hinted-advance-width g ppem))
      (row name (* n *repetitions*)
           (time-ms (lambda ()
                      (dotimes (r *repetitions*)
                        (dolist (g glyphs) (zpb-ttf:hinted-advance-width g ppem)))))))))

(let ((liberation (namestring (merge-pathnames "LiberationSans-Regular.ttf" *fonts-dir*))))
  (when *dejavu*
    (bench-hint-outlines "dejavu-9" *dejavu* 9 4000)
    (bench-hint-outlines "dejavu-16" *dejavu* 16 4000)
    (bench-hint-outlines "dejavu-32" *dejavu* 32 4000))
  (bench-hint-outlines "liberation-12" liberation 12 2500)
  ;; Rows with no FreeType counterpart: what a renderer asks for.
  (when *dejavu*
    (bench-hint-outlines-fresh "dejavu-16-fresh" *dejavu* 16 4000)
    (bench-hinted-contours "dejavu-16-contours" *dejavu* 16 4000)
    (bench-hinted-advances "dejavu-16-advances" *dejavu* 16 4000)))
