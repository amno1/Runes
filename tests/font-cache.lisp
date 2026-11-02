;;;; tests/font-cache.lisp — CACHE-FONTS does not hold up other threads
;;;; finding fonts while it scans, and metrics are unchanged by caching
;;;; units per em.
;;;; Run with tests/run.sh (see there).

(asdf:load-asd (merge-pathnames "../clxcb-truetype.asd" *load-truename*))
(ql:quickload :clxcb-truetype :silent t)

(defpackage :font-cache-test (:use :cl))
(in-package :font-cache-test)

(defmacro check (name form expect)
  `(let ((v ,form)) (format t "~&~:[FAIL~;ok  ~] ~a: ~s~%" (equal v ,expect) ,name v)))

(xcb-truetype:cache-fonts)
(let* ((font (make-instance 'xcb-truetype:font :family "DejaVu Sans" :size 17))
       (scan-time 0) (lookups 0) (slowest 0) (done nil)
       (scanner (bt:make-thread
                 (lambda ()
                   (let ((t0 (get-internal-real-time)))
                     (dotimes (i 3)
                       (xcb-truetype:cache-fonts))
                     (setf scan-time
                           (/ (- (get-internal-real-time) t0) internal-time-units-per-second)
                           done t))))))

  ;; While the scanner runs, keep finding a font's file and making fonts.
  (loop until done
        do (let ((t0 (get-internal-real-time)))
             (xcb-truetype::get-font-pathname font)
             (make-instance 'xcb-truetype:font :family "DejaVu Sans" :size 11)
             (incf lookups)
             (setf slowest
                   (max slowest (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))))

  (bt:join-thread scanner)
  (format t "~&  three scans took ~,2fs; ~d lookups meanwhile, the slowest ~,4fs~%" scan-time lookups slowest)
  (check "lookups go on while the font directories are scanned" (> lookups 10) t)
  (check "  none waits for a whole scan" (< slowest (/ scan-time 3)) t)
  (check "the font cache still has the families" (and (member "DejaVu Sans" (xcb-truetype:get-font-families) :test #'string=) t) t)

  ;; FONT-UNITS->PIXELS is the font size over 72 points per inch, per unit of the em.
  (xcb-truetype::with-font-loader (loader font)
    (check "units->pixels as computed from the font file"
           (xcb-truetype::font-units->pixels-x 96 font)
           (/ (* 17 96) (* 72 (zpb-ttf:units/em loader)))))

  (setf (xcb-truetype:font-size font) 23)

  (xcb-truetype::with-font-loader (loader font)
    (check "  and follows a change of size"
           (xcb-truetype::font-units->pixels-y 96 font)
           (/ (* 23 96) (* 72 (zpb-ttf:units/em loader))))))

;;; Every font type is indexed: the WOFF and WOFF2 test fonts of runes/otf,
;;; and a WOFF2 collection, whose fonts are cached as (PATHNAME . INDEX).
(let ((xcb-truetype:*font-dirs*
        (list (merge-pathnames "../otf/tests/fonts/woff/"
                              (make-pathname :name nil :type nil
                                             :defaults *load-truename*)))))
  (xcb-truetype:cache-fonts)
  (let ((liberation (make-instance 'xcb-truetype:font :family "Liberation Sans"
                                                      :subfamily "Regular" :size 24))
        (ttc (make-instance 'xcb-truetype:font :family "Test TTF"
                                               :subfamily "Regular" :size 24)))
    (check "a WOFF or WOFF2 font is cached"
           (and (member (pathname-type (xcb-truetype::get-font-pathname liberation))
                        '("woff" "woff2") :test #'string=)
                t)
           t)
    (check "  and measured" (plusp (xcb-truetype:text-width 96 liberation "Hag")) t)
    (check "a font of a WOFF2 collection is cached with its index"
           (let ((source (xcb-truetype::font-source ttc)))
             (and (consp source) (string= (pathname-type (car source)) "woff2")))
           t)
    (check "  and measured" (plusp (xcb-truetype:text-width 96 ttc "Hag")) t)
    (check "a font without a family name is left out"
           (member nil (xcb-truetype:get-font-families))
           nil)))

;;; The same font as .ttf (in otf/tests/fonts/) and as .woff and .woff2 (in
;;; its woff/ subdirectory, scanned later): the .ttf is kept.
(let ((xcb-truetype:*font-dirs*
        (list (merge-pathnames "../otf/tests/fonts/"
                               (make-pathname :name nil :type nil
                                              :defaults *load-truename*)))))
  (xcb-truetype:cache-fonts)
  (check "a plain font is preferred over its WOFF and WOFF2 files"
         (pathname-type (xcb-truetype::get-font-pathname
                         (make-instance 'xcb-truetype:font :family "Liberation Sans"
                                                           :subfamily "Regular" :size 24)))
         "ttf"))
(xcb-truetype:cache-fonts)
