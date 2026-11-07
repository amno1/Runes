;;;; font-loader.lisp — Font loader object and metadata accessors
;;;; Copyright (c) 2006 Zachary Beane, 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

(in-package #:zpb-ttf)

(defstruct (long-metrics (:conc-name lm-))
  "An 'hmtx' or 'vmtx' table, read where it lies: LONG-COUNT (advance, bearing)
pairs at OFFSET of FD, then bare bearings; SIZE bytes in all.  COUNT is the
number of glyphs with a bearing, the table's long ones included."
  (fd nil :type runes/otf:font-data)
  (offset 0 :type (unsigned-byte 32))
  (size 0 :type (unsigned-byte 32))
  (long-count 0 :type (unsigned-byte 16))
  (count 0 :type (unsigned-byte 32)))

(declaim (sb-ext:freeze-type long-metrics))

(defclass font-loader ()
  ((tables :initform (make-hash-table :test 'equal) :reader tables)
   ;; :GLYF or :CFF once CFF-OUTLINES-P has looked, as it does for every glyph.
   (outlines-kind :initform nil :accessor %outlines-kind)
   ;; (TABLE-INFO . GVAR) once GVAR-GLYPH-DATA has read the header, or :NONE.
   (gvar-cache :initform nil :accessor %gvar-cache)
   ;; What the glyph loaders read from 'glyf' and 'loca', found on first use.
   (outline-tables :initform nil :accessor %outline-tables)
   (font-data :initarg :font-data :accessor font-data)
   (input-stream :initarg :input-stream :initform nil :accessor input-stream)
   (table-count :initarg :table-count :initform 0 :reader table-count)
   ;; from 'head' table
   (units/em :accessor units/em :initform 2048)
   (bounding-box :accessor bounding-box :initform (vector 0 0 0 0))
   (loca-offset-format :accessor loca-offset-format :initform :short)
   ;; from 'loca' table
   (glyph-locations :accessor glyph-locations :initform nil)
   ;; from 'cmap' table
   ;; Glyph index -> lowest code point mapping to it; built on first use.
   (inverse-character-map :accessor inverse-character-map :initform nil)
   (cmap-lookup-fn :accessor cmap-lookup-fn :initform nil)
   ;; Function of one argument FN, calling (FN code-point glyph-index) for each mapping.
   (cmap-mappings-fn :accessor cmap-mappings-fn :initform nil)
   ;; from 'maxp' table
   (glyph-count :accessor glyph-count :initform 0)
   ;; from 'hhea' table
   (ascender :accessor ascender :initform 0)
   (descender :accessor descender :initform 0)
   (line-gap :accessor line-gap :initform 0)
   (max-width :accessor max-width :initform 0)
   ;; from 'hmtx' table: where it is, not a copy of it (see HORIZONTAL-ADVANCE)
   (hmetrics :accessor hmetrics :initform nil)
   ;; from 'vhea' table
   (vhea-missing-p :initform nil :accessor vhea-missing-p)
   (vascender :accessor vascender :initform 0)
   (vdescender :accessor vdescender :initform 0)
   ;; from 'vhea' and 'vmtx' tables
   (vmtx-missing-p :initform nil :accessor vmtx-missing-p)
   (vmetrics :accessor vmetrics :initform nil)
   ;; The advance height of every glyph when there is no 'vmtx'.
   (missing-advance-height :accessor missing-advance-height :initform 0)
   ;; from 'kern' table: where it is, and the pairs it gives once they have been
   ;; wanted (see KERNING-TABLE)
   (kern-position :initform nil :accessor kern-position)
   (kerning-table :initform nil :accessor %kerning-table)
   ;; from 'GPOS': the 'kern' lookups, found on first use, and the kerning
   ;; of each pair asked for (see gpos-kerning.lisp)
   (%gpos-kern-lookups :initform :unknown :accessor %gpos-kern-lookups)
   (%gdef-variation-store :initform :unknown :accessor %gdef-variation-store)
   (gpos-kerning-cache :initform (make-hash-table) :accessor gpos-kerning-cache)
   ;; from 'name' table
   ;; The entries of the 'name' table, made when a name is first asked for
   (name-entries :initform nil :accessor %name-entries)
   ;; from 'post' table
   (italic-angle :accessor italic-angle :initform 0)
   (fixed-pitch-p :accessor fixed-pitch-p :initform nil)
   (underline-position :accessor underline-position :initform 0)
   (underline-thickness :accessor underline-thickness :initform 0)
   (postscript-glyph-names :accessor postscript-glyph-names :initform nil)
   ;; glyph cache
   (glyph-cache :accessor glyph-cache :initform nil)
   ;; from 'CFF ' or 'CFF2': the font, opened on first use, and its subfonts
   (cff-font :accessor %cff-font :initform nil)
   (cff-subfonts :accessor cff-subfonts :initform (make-hash-table))
   ;; Instance of a variable font: as set, and normalized (NIL for the default)
   (user-coordinates :accessor user-coordinates :initform nil)
   (normalized-coordinates :accessor normalized-coordinates :initform nil)
   ;; TTC info
   (collection-font-count :reader collection-font-count :initform nil :initarg :collection-font-count)
   (collection-font-index :reader collection-font-index :initform nil :initarg :collection-font-index)))

(declaim (inline long-metrics-advance long-metrics-bearing))

;; The reads need no check of their own: MAKE-LONG-METRICS-AT has seen that the
;; whole table is in the font data, and every position below is inside it.
(defun long-metrics-advance (metrics gid)
  "The advance of glyph GID in METRICS (a LONG-METRICS or NIL); the last
glyph's for one past the table's, 0 if the table has none."
  (declare (type (or null long-metrics) metrics) (fixnum gid) (optimize (speed 3) (safety 0)))
  (if (or (null metrics) (zerop (lm-long-count metrics)))
      0
      (let* ((fd (lm-fd metrics))
             (bytes (runes/otf:font-data-bytes fd))
             (pos (+ (runes/otf:font-data-offset fd) (lm-offset metrics)
                     (* 4 (the fixnum (min (1- (lm-long-count metrics)) (max 0 gid)))))))
        (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos))
        (logior (ash (aref bytes pos) 8) (aref bytes (1+ pos))))))

(defun long-metrics-bearing (metrics gid)
  "The bearing of glyph GID in METRICS; that of the last glyph for one past
them, 0 where the table is too short for it."
  (declare (type (or null long-metrics) metrics) (fixnum gid) (optimize (speed 3) (safety 0)))
  (if (or (null metrics) (zerop (lm-count metrics)))
      0
      (let* ((i (min (1- (lm-count metrics)) (max 0 gid)))
             (long (lm-long-count metrics))
             (offset (lm-offset metrics))
             (rel (if (< i long)
                      (+ (* 4 i) 2)
                      (+ (* 4 long) (* 2 (- i long))))))
        (declare (fixnum i rel))
        (if (<= (+ rel 2) (lm-size metrics))
            (let* ((fd (lm-fd metrics))
                   (bytes (runes/otf:font-data-bytes fd))
                   (pos (+ (runes/otf:font-data-offset fd) offset rel))
                   (u (logior (ash (aref bytes pos) 8) (aref bytes (1+ pos)))))
              (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (fixnum pos))
              (if (logbitp 15 u) (- u 65536) u))
            0))))

(declaim (inline horizontal-advance horizontal-bearing vertical-advance vertical-bearing))

(defun horizontal-advance (loader gid)
  (long-metrics-advance (hmetrics loader) gid))

(defun horizontal-bearing (loader gid)
  (long-metrics-bearing (hmetrics loader) gid))

(defun vertical-advance (loader gid)
  (if (vmtx-missing-p loader)
      (missing-advance-height loader)
      (long-metrics-advance (vmetrics loader) gid)))

(defun vertical-bearing (loader gid)
  (long-metrics-bearing (vmetrics loader) gid))

(defclass table-info ()
  ((name :initarg :name :reader name)
   (offset :initarg :offset :reader offset)
   (size :initarg :size :reader size)))

(defmethod print-object ((object table-info) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (name object))))

(defmethod table-info ((tag string) (font-loader font-loader))
  (gethash tag (tables font-loader)))

(defmethod table-info ((tag integer) (font-loader font-loader))
  (gethash (tag->number-string tag) (tables font-loader)))

(defun tag->number-string (num)
  (let ((s (make-string 4)))
    (setf (char s 0) (code-char (ldb (byte 8 24) num))
          (char s 1) (code-char (ldb (byte 8 16) num))
          (char s 2) (code-char (ldb (byte 8 8) num))
          (char s 3) (code-char (ldb (byte 8 0) num)))
    s))

(defmethod table-exists-p (tag font-loader)
  (nth-value 1 (table-info tag font-loader)))

(defmethod table-position ((tag string) (font-loader font-loader))
  (let ((info (table-info tag font-loader)))
    (if info
        (offset info)
        (error "No such table -- ~A" tag))))

(defmethod table-size ((tag string) (font-loader font-loader))
  (let ((info (table-info tag font-loader)))
    (if info
        (size info)
        (error "No such table -- ~A" tag))))

;;; Name table entries

(defclass name-entry ()
  ((font-loader :initarg :font-loader :reader font-loader)
   (platform-id :initarg :platform-id :reader platform-id)
   (encoding-id :initarg :encoding-id :reader encoding-id)
   (language-id :initarg :language-id :reader language-id)
   (name-id :initarg :name-id :reader name-id)
   ;; The string, decoded when it is asked for: where it is, and how, until then.
   (%value :initarg :value :initform :unread :accessor %value)
   (source :initarg :source :initform nil :reader source)))

(defmethod value ((entry name-entry))
  (let ((value (%value entry)))
    (if (eq value :unread)
        (destructuring-bind (position length) (source entry)
          (setf (%value entry)
                (decode-name-string-from-fd (font-data (font-loader entry))
                                            position length
                                            (platform-id entry) (encoding-id entry))))
        value)))

(defmethod print-object ((entry name-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (format stream "name-id=~D: ~S" (name-id entry) (value entry))))

(defvar *name-identifiers*
  #(:copyright-notice
    :font-family
    :font-subfamily
    :unique-subfamily
    :full-name
    :name-table-version
    :postscript-name
    :trademark-notice
    :manufacturer-name
    :designer
    :description
    :vendor-url
    :designer-url
    :license-description
    :licence-info-url
    :reserved
    :preferred-family
    :preferred-subfamily
    :compatible-full
    :sample-text))

(defun name-identifier-id (symbol)
  (let ((id (position symbol *name-identifiers*)))
    (if id id (error "Unknown NAME identifier: ~S" symbol))))

(defmethod find-name-entry (platform-id encoding-id language-id name-id
                            (font-loader font-loader))
  (loop for name-entry across (name-entries font-loader)
        when (and (or (null platform-id) (= (platform-id name-entry) platform-id))
                  (or (null encoding-id) (= (encoding-id name-entry) encoding-id))
                  (or (null language-id) (= (language-id name-entry) language-id))
                  (or (null name-id) (= (name-id name-entry) name-id)))
        return name-entry))

(defun name-entry-rank (entry)
  "Preference rank of ENTRY, lower is better."
  (let ((plat (platform-id entry))
        (enc (encoding-id entry))
        (lang (language-id entry)))
    (cond ((and (= plat 3) (= enc 1) (= lang #x0409)) 0) ; Windows US-English
          ((and (= plat 3) (= enc 10) (= lang #x0409)) 1)
          ((= plat 3) 2)
          ((= plat 0) 3)                                 ; Unicode
          ((and (= plat 1) (= enc 0) (= lang 0)) 4)      ; Mac Roman
          (t 5))))

(defmethod name-entry-value (name-designator (font-loader font-loader))
  (let ((name-id (etypecase name-designator
                   (keyword (name-identifier-id name-designator))
                   (integer name-designator)))
        (best nil)
        (best-rank nil))
    (loop for entry across (name-entries font-loader)
          when (= (name-id entry) name-id)
            do (let ((rank (name-entry-rank entry)))
                 (when (or (null best-rank) (< rank best-rank))
                   (setf best entry best-rank rank))))
    (when best
      (value best))))

(defmethod postscript-name ((font-loader font-loader))
  (name-entry-value :postscript-name font-loader))

(defmethod family-name ((font-loader font-loader))
  (name-entry-value :font-family font-loader))

(defmethod subfamily-name ((font-loader font-loader))
  (name-entry-value :font-subfamily font-loader))

(defmethod full-name ((font-loader font-loader))
  (name-entry-value :full-name font-loader))

;;; Character mapping & glyph indexing

(defgeneric code-point-font-index (code-point font-loader)
  (:method (code-point (font-loader font-loader))
    (let ((fn (cmap-lookup-fn font-loader)))
      (if fn
          (funcall fn code-point)
          0))))

(defun build-inverse-character-map (font-loader)
  (let ((inv (make-array (glyph-count font-loader) :initial-element 0))
        (fn (cmap-mappings-fn font-loader)))
    (when fn
      (funcall fn (lambda (cp gid)
                    (when (and (< 0 gid (length inv))
                               (or (zerop (aref inv gid)) (< cp (aref inv gid))))
                      (setf (aref inv gid) cp)))))
    inv))

(defgeneric font-index-code-point (glyph-index font-loader)
  (:method (glyph-index (font-loader font-loader))
    (let ((inv (or (inverse-character-map font-loader)
                   (setf (inverse-character-map font-loader)
                         (build-inverse-character-map font-loader)))))
      (if (and inv (< glyph-index (length inv)))
          (let ((cp (aref inv glyph-index)))
            (if (plusp cp) cp 0))
          0))))

(defmethod glyph-exists-p (char-or-code (font-loader font-loader))
  (let ((cp (if (characterp char-or-code) (char-code char-or-code) char-or-code)))
    (not (zerop (code-point-font-index cp font-loader)))))

(defmethod index-glyph (index (font-loader font-loader))
  (unless (and (integerp index) (< -1 index (glyph-count font-loader)))
    (error 'unsupported-value
           :description "Glyph index out of range"
           :actual-value index
           :expected-values (list (format nil "0..~D" (1- (glyph-count font-loader))))))
  ;; The vector of glyph objects is made when the first is asked for: code that
  ;; works with glyph indices only (hinting a glyph, say) never needs it.
  (let ((cache (or (glyph-cache font-loader)
                   (setf (glyph-cache font-loader)
                         (make-array (glyph-count font-loader) :initial-element nil)))))
    (or (aref cache index)
        (setf (aref cache index)
              (make-instance 'glyph
                             :font-loader font-loader
                             :font-index index)))))

(defmethod find-glyph (char-or-code (font-loader font-loader))
  (let* ((cp (if (characterp char-or-code) (char-code char-or-code) char-or-code))
         (index (code-point-font-index cp font-loader)))
    (index-glyph index font-loader)))

(defmethod all-kerning-pairs ((font-loader font-loader))
  (let ((pairs '()))
    (maphash (lambda (k v)
               (let ((g1-idx (ldb (byte 16 16) k))
                     (g2-idx (ldb (byte 16 0) k)))
                 (push (list (index-glyph g1-idx font-loader)
                             (index-glyph g2-idx font-loader)
                             v)
                       pairs)))
             (kerning-table font-loader))
    (nreverse pairs)))