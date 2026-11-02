;;;; font-cache.lisp — discovery and in-memory cache of TrueType font files
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:runes)

(defvar *font-dirs*
  #+(or unix netbsd openbsd freebsd)
  (list "/usr/share/fonts/"
        "/usr/local/share/fonts/"
        #+darwin "/Library/Fonts/"
        #+darwin "/System/Library/Fonts/"
        ;; Where fontconfig installs a user's fonts today, and where it did.
        (namestring (merge-pathnames ".local/share/fonts/" (user-homedir-pathname)))
        (namestring (merge-pathnames ".fonts/" (user-homedir-pathname))))
  #+windows
  (list (namestring
         (merge-pathnames "fonts/" 
                          (pathname (concatenate 'string (or (uiop:getenv "WINDIR") "C:/Windows") "/")))))
  "List of directories, which contain TrueType fonts.")

(defvar *font-cache* (make-hash-table :test 'equal)
  "Hashmap for caching font families, subfamilies and files.  Guarded by
   *FONT-CACHE-LOCK*.")

(defvar *font-cache-lock* (bt:make-recursive-lock "truetype font cache")
  "Guards *FONT-CACHE*, so that CACHE-FONTS in one thread does not run
   into another thread scanning or reading it.")

(defun cache-font-file (pathname)
  "Add the font in the file PATHNAME to *FONT-CACHE*.  A file that is not
   a font ZPB-TTF can read is skipped."
  (bt:with-recursive-lock-held (*font-cache-lock*)
    (%cache-font-file pathname *font-cache*)))

(defun %cache-font-file (pathname cache)
  "Add the font in PATHNAME to CACHE, a table like *FONT-CACHE*."
  (handler-case 
      (zpb-ttf:with-font-loader (font pathname)
        (multiple-value-bind (hash-table exists-p)
            (gethash (zpb-ttf:family-name font) cache
                     (make-hash-table :test 'equal))
          (setf (gethash (zpb-ttf:subfamily-name font) hash-table)
                pathname)
          (unless exists-p
            (setf (gethash (zpb-ttf:family-name font) cache)
                  hash-table))))
    ;; A file that is not a font zpb-ttf can read is skipped; warnings
    ;; are not errors, and go on to the caller.
    (error () (return-from %cache-font-file))))

(defun ttf-pathname-test (pathname)
  (let ((type (pathname-type pathname)))
    (and (stringp type) (string-equal "ttf" type))))

(defun cache-fonts ()
  "Cache the fonts in the *FONT-DIRS* directories, replacing *FONT-CACHE*.
   The directories are scanned into a new table without holding
   *FONT-CACHE-LOCK*, which is taken only to put the table in place, so
   other threads go on finding fonts in the old table meanwhile."
  (let ((cache (%scan-font-dirs)))
    (bt:with-recursive-lock-held (*font-cache-lock*)
      (setf *font-cache* cache))))

(defun %scan-font-dirs ()
  (let ((cache (make-hash-table :test 'equal)))
    (labels ((scan-dir (dir)
               (when (probe-file dir)
                 (dolist (file (uiop:directory-files dir))
                   (when (ttf-pathname-test file)
                     (%cache-font-file file cache)))
                 (dolist (subdir (uiop:subdirectories dir))
                   (scan-dir subdir)))))
      (dolist (font-dir *font-dirs*)
        (let ((p (probe-file font-dir)))
          (when p
            (scan-dir p)))))
    cache))

(defun get-font-families ()
  "Returns cached font families."
  (declare (special *font-cache*))
  (let ((result (list)))
    (bt:with-recursive-lock-held (*font-cache-lock*)
      (maphash (lambda (key value)
                 (declare (ignorable value))
                 (push key result))
               *font-cache*))
    (nreverse result)))

(defun get-font-subfamilies (font-family)
  "Returns font subfamilies for current FONT-FAMILY. For e.g. regular, italic, bold, etc."
  (declare (special *font-cache*))
  (let ((result (list)))
    (bt:with-recursive-lock-held (*font-cache-lock*)
     (maphash (lambda (family value)
               (declare (ignorable family))
               (when (string-equal font-family family)
                 (maphash (lambda (subfamily pathname)
                            (declare (ignorable pathname))
                            (push subfamily result))
                          value)
                 (return-from get-font-subfamilies
                   (nreverse result))))
             *font-cache*))
    (nreverse result)))
