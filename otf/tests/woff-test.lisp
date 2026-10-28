;;;; woff-test.lisp — WOFF 1.0 and WOFF2 decoding against the original fonts
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; The fonts in fonts/woff/ are made from fonts in fonts/ by
;;; fonts/woff/make-woff.py (WOFF 1.0 by its own encoder, WOFF2 by
;;; woff2_compress). Each decoded font must have the original's tables:
;;; byte for byte, except head's checkSumAdjustment (recomputed for the
;;; rebuilt file) and WOFF2's transformed glyf, loca and hmtx (and head, whose
;;; flags it may change), which are compared as outlines and metrics.
;;; woff2_compress drops DSIG, since a signature cannot survive the transforms. The loader must open them directly, and broken files must
;;; signal a FONT-ERROR. Loaded after charstring-test.lisp, whose check
;;; macros it uses.

(defpackage #:zpb-ttf/woff-test
  (:use #:cl)
  (:import-from #:zpb-otf/charstring-test #:check #:check-error)
  (:export #:run-woff-tests))

(in-package #:zpb-ttf/woff-test)

(defvar *test-fonts-dir*
  (merge-pathnames "fonts/" (make-pathname :directory (pathname-directory *load-truename*))))

(defun font-path (name) (merge-pathnames name *test-fonts-dir*))
(defun woff-path (name) (merge-pathnames (concatenate 'string "woff/" name) *test-fonts-dir*))

(defun slurp (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun octets (fd)
  "The octets of FONT-DATA FD."
  (let ((start (zpb-otf::font-data-offset fd)))
    (subseq (zpb-otf::font-data-bytes fd) start (+ start (zpb-otf:font-data-length fd)))))

(defun u32 (v i) (logior (ash (aref v i) 24) (ash (aref v (+ i 1)) 16) (ash (aref v (+ i 2)) 8) (aref v (+ i 3))))

(defun sfnt-tables (v &optional (font 0))
  "Alist of (tag . octets) of font FONT of the sfnt or TTC in V."
  (let ((dir (if (= (u32 v 0) #x74746366) (u32 v (+ 12 (* 4 font))) 0)))
    (loop for i below (ash (u32 v (+ dir 4)) -16)
          for e = (+ dir 12 (* 16 i))
          collect (cons (map 'string #'code-char (subseq v e (+ e 4)))
                        (subseq v (u32 v (+ e 8)) (+ (u32 v (+ e 8)) (u32 v (+ e 12))))))))

(defun font-count (v) (if (= (u32 v 0) #x74746366) (u32 v 8) 1))

(defun same-table-p (tag a b)
  "Whether table TAG's octets A and B match, head's checkSumAdjustment aside."
  (if (and (string= tag "head") a b (= (length a) (length b)) (>= (length a) 12))
      (and (equalp (subseq a 0 8) (subseq b 0 8)) (equalp (subseq a 12) (subseq b 12)))
      (equalp a b)))

(defun without-dsig (tables)
  (remove "DSIG" tables :key #'car :test #'string=))

(defun tables-match-p (orig decoded transformed &key (font 0) woff2)
  "Whether the tables of font FONT of the sfnt or TTC octets ORIG and DECODED
match, TRANSFORMED (tag strings) excepted; with WOFF2, DSIG may be missing.
Prints the first difference."
  (let ((a (sfnt-tables orig font)) (b (sfnt-tables decoded font)))
    (when woff2 (setf a (without-dsig a)))
    (and (or (= (length a) (length b))
             (progn (format t "    ~D tables, want ~D~%" (length b) (length a)) nil))
         (loop for (tag . data) in a
               for other = (cdr (assoc tag b :test #'string=))
               always (or (member tag transformed :test #'string=)
                          (same-table-p tag data other)
                          (progn (format t "    table ~A differs~%" tag) nil))))))

(defun glyph-signature (loader i)
  "Outline points, on-curve flags and metrics of glyph I."
  (let ((g (zpb-ttf:index-glyph i loader)))
    (list (zpb-ttf:advance-width g) (zpb-ttf:left-side-bearing g)
          (map 'list (lambda (c) (map 'list (lambda (p) (list (zpb-ttf:x p) (zpb-ttf:y p)
                                                              (zpb-ttf:on-curve-p p)))
                                      c))
               (zpb-ttf:contours g)))))

(defun glyphs-match-p (orig-path decoded &key (collection-index 0))
  "Whether every glyph of the font at ORIG-PATH has the same outline and
metrics when DECODED (a FONT-DATA) is opened."
  (zpb-ttf:with-font-loader (a orig-path :collection-index collection-index)
    (zpb-ttf:with-font-loader (b decoded :collection-index collection-index)
      (and (= (zpb-ttf:glyph-count a) (zpb-ttf:glyph-count b))
           (loop for i below (zpb-ttf:glyph-count a)
                 always (or (equal (glyph-signature a i) (glyph-signature b i))
                            (progn (format t "    glyph ~D differs~%" i) nil)))))))

(defparameter *fonts*
  ;; source font, base name, TrueType outlines?
  '(("base.ttf" "base" nil)
    ("glyf_components.ttf" "glyf_components" t)
    ("vazirmatn_var_trimmed.ttf" "vazirmatn_var_trimmed" t)
    ("LiberationSans-Regular.ttf" "LiberationSans-Regular" t)
    ("Cantarell-VF.otf" "Cantarell-VF" nil)))

(defun check-decoding ()
  (dolist (f *fonts*)
    (destructuring-bind (src base glyf) f
      (let ((orig (slurp (font-path src))))
        (let ((woff1 (woff-path (concatenate 'string base ".woff"))))
          (check (format nil "~A.woff: tables match the original" base)
                 (tables-match-p orig (octets (zpb-otf:decode-woff1 woff1 :verify-checksums t)) '()))
          (when glyf
            (check (format nil "~A.woff: glyphs match the original" base)
                   (glyphs-match-p (font-path src) (zpb-otf:decode-woff1 woff1)))))
        (let ((woff2 (woff-path (concatenate 'string base ".woff2"))))
          (check (format nil "~A.woff2: untransformed tables match the original" base)
                 (tables-match-p orig (octets (zpb-otf:decode-woff2 woff2))
                                 '("glyf" "loca" "hmtx" "head") :woff2 t))
          (when glyf
            (check (format nil "~A.woff2: glyphs match the original" base)
                   (glyphs-match-p (font-path src) (zpb-otf:decode-woff2 woff2))))))))
  ;; a collection
  (let ((orig (slurp (font-path "TTC.ttc")))
        (decoded (octets (zpb-otf:decode-woff2 (woff-path "TTC.woff2")))))
    (check "TTC.woff2: a collection of as many fonts"
           (= (font-count orig) (font-count decoded)))
    (dotimes (k (font-count orig))
      (check (format nil "TTC.woff2: font ~D's tables are the original's" k)
             (tables-match-p orig decoded '("glyf" "loca" "hmtx" "head") :font k :woff2 t))
      (check (format nil "TTC.woff2: font ~D's glyphs match" k)
             (glyphs-match-p (font-path "TTC.ttc") (zpb-otf:decode-woff2 (woff-path "TTC.woff2"))
                             :collection-index k)))))

(defun check-loader ()
  (let ((want (zpb-ttf:with-font-loader (l (font-path "LiberationSans-Regular.ttf"))
                (list (zpb-ttf:family-name l) (zpb-ttf:glyph-count l)))))
    (dolist (ext '(".woff" ".woff2"))
      (let ((path (woff-path (concatenate 'string "LiberationSans-Regular" ext))))
        (flet ((same (loader) (equal want (list (zpb-ttf:family-name loader)
                                                (zpb-ttf:glyph-count loader)))))
          (check (format nil "open-font-loader detects ~A from a pathname" ext)
                 (zpb-ttf:with-font-loader (l path) (same l)))
          (check (format nil "open-font-loader detects ~A from a stream" ext)
                 (zpb-ttf:with-font-loader (l (open path :element-type '(unsigned-byte 8)))
                   (same l)))
          (check (format nil "open-font-loader detects ~A from octets" ext)
                 (zpb-ttf:with-font-loader (l (slurp path)) (same l)))))))
  (check "open-font-loader opens font 1 of TTC.woff2"
         (zpb-ttf:with-font-loader (l (woff-path "TTC.woff2") :collection-index 1)
           (plusp (zpb-ttf:glyph-count l)))))

(defun check-extras ()
  (let ((woff (woff-path "base.woff")))
    (check "base.woff: metadata block"
           (search "zpb-ttf.test.base" (map 'string #'code-char (zpb-otf:decode-woff1-metadata woff))))
    (check "base.woff: private data block"
           (string= "zpb-ttf private data"
                    (map 'string #'code-char (octets (zpb-otf:woff1-private-data woff)))))
    (check "glyf_components.woff: no metadata or private data"
           (and (null (zpb-otf:decode-woff1-metadata (woff-path "glyf_components.woff")))
                (null (zpb-otf:woff1-private-data (woff-path "glyf_components.woff")))))))

(defun check-broken ()
  ;; truncated anywhere, or with a byte flipped in the header or directory:
  ;; a FONT-ERROR, or (for a flip the format cannot detect) a decoded font
  (dolist (name '("base.woff" "base.woff2" "TTC.woff2"))
    (let* ((v (slurp (woff-path name)))
           (decode (if (search "woff2" name) #'zpb-otf:decode-woff2 #'zpb-otf:decode-woff1))
           (bad '()))
      (loop for len in (list 0 3 20 44 47 60 (floor (length v) 2) (1- (length v)))
            do (handler-case (progn (funcall decode (subseq v 0 len)) (push len bad))
                 (zpb-otf:font-error ())
                 (error (e) (push (list len e) bad))))
      (check (format nil "~A: every truncation signals FONT-ERROR" name) (null bad))
      (setf bad '())
      (loop for i from 0 below (min 120 (length v))
            do (let ((w (copy-seq v)))
                 (setf (aref w i) (logxor (aref w i) #x5A))
                 (handler-case (funcall decode w)
                   (zpb-otf:font-error ())
                   (error (e) (push (list i e) bad)))))
      (check (format nil "~A: header and directory flips decode or signal FONT-ERROR" name)
             (or (null bad) (progn (format t "    ~S~%" (first bad)) nil))))))

(defun run-woff-tests ()
  (let ((zpb-otf/charstring-test::*failures* 0)
        (zpb-otf/charstring-test::*count* 0))
    (format t "========================================================~%")
    (format t "WOFF 1.0 and WOFF2 Against the Original Fonts~%")
    (format t "========================================================~%")
    (check-decoding)
    (check-loader)
    (check-extras)
    (check-broken)
    (format t "~%Results: ~D tests run, ~D failures.~%"
            zpb-otf/charstring-test::*count* zpb-otf/charstring-test::*failures*)
    (unless (zerop zpb-otf/charstring-test::*failures*)
      (error "WOFF test suite failed with ~D failure(s)" zpb-otf/charstring-test::*failures*))
    t))
