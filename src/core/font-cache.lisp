;;;; font-cache.lisp — discovery and in-memory cache of font files
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
  "List of directories, which contain fonts.")

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
  "Add the fonts in PATHNAME to CACHE, a table like *FONT-CACHE*: one font,
   or every font of a collection.  The file is read, and a WOFF or WOFF2
   file decoded, once for all of them.  A font in a collection is cached
   as (PATHNAME . INDEX), any other as PATHNAME.  A font already cached
   from a file of a better format (see FORMAT-RANK) is kept."
  (flet ((add (font source)
           ;; A font without a family or subfamily name cannot be asked for
           (let ((family (zpb-ttf:family-name font))
                 (subfamily (zpb-ttf:subfamily-name font)))
             (when (and family subfamily)
               (let* ((subfamilies (or (gethash family cache)
                                       (setf (gethash family cache)
                                             (make-hash-table :test 'equal))))
                      (old (gethash subfamily subfamilies)))
                 (when (or (null old)
                           (<= (format-rank source) (format-rank old)))
                   (setf (gethash subfamily subfamilies) source)))))))
    ;; A file or a font that zpb-ttf cannot read is skipped; warnings are
    ;; not errors, and go on to the caller.
    (let ((data (handler-case (%sfnt-data pathname)
                  (error () (return-from %cache-font-file)))))
      (let ((count (handler-case
                       (zpb-ttf:with-font-loader (font data)
                         (let ((count (zpb-ttf:collection-font-count font)))
                           (unless count (add font pathname))
                           count))
                     (error () nil))))
        (when count
          (dotimes (i count)
            (handler-case
                (zpb-ttf:with-font-loader (font data :collection-index i)
                  (add font (cons pathname i)))
              (error () nil))))))))

(defun %sfnt-data (pathname)
  "The font data of PATHNAME, a WOFF or WOFF2 file decoded to an sfnt."
  (let ((data (runes/otf:ensure-font-data pathname)))
    (if (< (runes/otf:font-data-length data) 4)
        data
        (case (runes/otf:font-data-read-u32 data 0)
          (#x774F4646 (runes/otf:decode-woff1 data)) ; 'wOFF'
          (#x774F4632 (runes/otf:decode-woff2 data)) ; 'wOF2'
          (t data)))))

(defun format-rank (source)
  "How much a font cache entry is preferred over another of the same font:
   0 for a plain TrueType or OpenType file, 1 for WOFF and 2 for WOFF2,
   which have to be decoded every time they are opened, WOFF2 more slowly."
  (let ((type (pathname-type (if (consp source) (car source) source))))
    (cond ((string-equal type "woff") 1)
          ((string-equal type "woff2") 2)
          (t 0))))

(defparameter *font-file-types* '("ttf" "otf" "ttc" "otc" "woff" "woff2")
  "The file types CACHE-FONTS reads.")

(defun font-pathname-test (pathname)
  (let ((type (pathname-type pathname)))
    (and (stringp type)
         (member type *font-file-types* :test #'string-equal))))

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
                   (when (font-pathname-test file)
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
