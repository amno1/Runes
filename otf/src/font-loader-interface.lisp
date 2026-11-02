;;;; font-loader-interface.lisp — Initialization and entry points for font-loader
;;;; Copyright (c) 2006 Zachary Beane, 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

(in-package #:zpb-ttf)

(defun close-font-loader (font-loader)
  (when (input-stream font-loader)
    (close (input-stream font-loader))
    (setf (input-stream font-loader) nil))
  font-loader)

(defmacro with-font-loader ((loader thing &rest args) &body body)
  `(let ((,loader (open-font-loader ,thing ,@args)))
     (unwind-protect
          (progn ,@body)
       (close-font-loader ,loader))))

;;; Mac OS Roman code points for bytes #x80-#xFF.
(defparameter *mac-roman-high-codes*
  (coerce '(#x00C4 #x00C5 #x00C7 #x00C9 #x00D1 #x00D6 #x00DC #x00E1
            #x00E0 #x00E2 #x00E4 #x00E3 #x00E5 #x00E7 #x00E9 #x00E8
            #x00EA #x00EB #x00ED #x00EC #x00EE #x00EF #x00F1 #x00F3
            #x00F2 #x00F4 #x00F6 #x00F5 #x00FA #x00F9 #x00FB #x00FC
            #x2020 #x00B0 #x00A2 #x00A3 #x00A7 #x2022 #x00B6 #x00DF
            #x00AE #x00A9 #x2122 #x00B4 #x00A8 #x2260 #x00C6 #x00D8
            #x221E #x00B1 #x2264 #x2265 #x00A5 #x00B5 #x2202 #x2211
            #x220F #x03C0 #x222B #x00AA #x00BA #x03A9 #x00E6 #x00F8
            #x00BF #x00A1 #x00AC #x221A #x0192 #x2248 #x2206 #x00AB
            #x00BB #x2026 #x00A0 #x00C0 #x00C3 #x00D5 #x0152 #x0153
            #x2013 #x2014 #x201C #x201D #x2018 #x2019 #x00F7 #x25CA
            #x00FF #x0178 #x2044 #x20AC #x2039 #x203A #xFB01 #xFB02
            #x2021 #x00B7 #x201A #x201E #x2030 #x00C2 #x00CA #x00C1
            #x00CB #x00C8 #x00CD #x00CE #x00CF #x00CC #x00D3 #x00D4
            #xF8FF #x00D2 #x00DA #x00DB #x00D9 #x0131 #x02C6 #x02DC
            #x00AF #x02D8 #x02D9 #x02DA #x00B8 #x02DD #x02DB #x02C7)
          '(simple-array (unsigned-byte 16) (128))))

(defun decode-utf-16be-from-fd (fd offset length)
  "Decode LENGTH bytes of UTF-16BE; a trailing odd byte is ignored and
unpaired surrogates become U+FFFD."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) offset length))
  (let ((units (floor length 2))
        (out (make-string-output-stream))
        (i 0))
    (flet ((unit (k) (runes/otf:font-data-read-u16 fd (+ offset (* k 2)))))
      (loop while (< i units)
            do (let ((u (unit i)))
                 (incf i)
                 (write-char
                  (cond
                    ((and (<= #xD800 u #xDBFF) (< i units) (<= #xDC00 (unit i) #xDFFF))
                     (prog1 (code-char (+ #x10000 (ash (- u #xD800) 10) (- (unit i) #xDC00)))
                       (incf i)))
                    ((<= #xD800 u #xDFFF) (code-char #xFFFD))
                    (t (code-char u)))
                  out))))
    (get-output-stream-string out)))

(defun decode-name-string-from-fd (fd offset length platform-id encoding-id)
  (cond
    ;; UTF-16BE: platform 0 (Unicode) or platform 3 (Windows) with encoding 1 or 10
    ((or (zerop platform-id)
         (and (= platform-id 3) (or (= encoding-id 1) (= encoding-id 10))))
     (decode-utf-16be-from-fd fd offset length))
    ;; Macintosh Roman
    ((and (= platform-id 1) (zerop encoding-id))
     (let ((str (make-string length)))
       (dotimes (i length str)
         (let ((byte (runes/otf:font-data-read-u8 fd (+ offset i))))
           (setf (char str i)
                 (code-char (if (< byte #x80)
                                byte
                                (aref *mac-roman-high-codes* (- byte #x80)))))))))
    ;; Other single-byte encodings: Latin-1 as a best effort
    (t
     (let ((str (make-string length)))
       (dotimes (i length str)
         (setf (char str i) (code-char (runes/otf:font-data-read-u8 fd (+ offset i)))))))))

(defun cmap-subtable-length (fd sub-off fmt table-end)
  "Length of the cmap subtable at SUB-OFF from its own header, clamped to TABLE-END."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) sub-off table-end)
           (type (unsigned-byte 16) fmt))
  (let ((declared (if (>= fmt 8)
                      (runes/otf:font-data-read-u32 fd (+ sub-off 4))
                      (runes/otf:font-data-read-u16 fd (+ sub-off 2)))))
    (min declared (max 0 (- table-end sub-off)))))

(defun best-cmap-subtable (fd cmap cmap-offset)
  "The format and position of the 'cmap' subtable to use: the first Unicode
format 12 subtable or, without one, the first Unicode format 4 subtable."
  (let ((format-4 nil))
    (dotimes (i (runes/otf:cmap-num-tables cmap) (when format-4 (values 4 format-4)))
      (let* ((erec (runes/otf:cmap-encoding-records-ref cmap i))
             (plat (runes/otf:encoding-record-platform-id erec))
             (enc (runes/otf:encoding-record-encoding-id erec))
             (sub-off (+ cmap-offset (runes/otf:encoding-record-subtable-offset erec)))
             (fmt (runes/otf:font-data-read-u16 fd sub-off)))
        (cond ((and (= fmt 12) (or (= plat 0) (= plat 3)))
               (return (values 12 sub-off)))
              ((and (= fmt 4) (null format-4)
                    (or (= plat 0) (and (= plat 3) (or (= enc 1) (= enc 10)))))
               (setf format-4 sub-off)))))))

(defun setup-cmap12-lookup (loader fd sub-off table-end)
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) sub-off table-end))
  (let* ((sub-len (cmap-subtable-length fd sub-off 12 table-end))
         (cmap12 (runes/otf:read-cmap12 fd sub-off sub-len))
         (num-groups (min (runes/otf:cmap12-num-groups cmap12)
                          (max 0 (floor (- sub-len 16) 12)))))
    (flet ((group (i)
             ;; SequentialMapGroup I: startCharCode, endCharCode and
             ;; startGlyphID, read in place.  NUM-GROUPS keeps I inside the
             ;; subtable.
             (declare (type (unsigned-byte 32) i))
             (let ((pos (+ sub-off 16 (* 12 i))))
               (values (runes/otf:font-data-read-u32 fd pos)
                       (runes/otf:font-data-read-u32 fd (+ pos 4))
                       (runes/otf:font-data-read-u32 fd (+ pos 8))))))
      (setf (cmap-lookup-fn loader)
            (lambda (cp)
              ;; Binary search of the groups, which are sorted.
              (let ((low 0) (high (1- num-groups)))
                (loop while (<= low high)
                      do (let ((mid (floor (+ low high) 2)))
                           (multiple-value-bind (start end gid) (group mid)
                             (cond ((< cp start) (setf high (1- mid)))
                                   ((> cp end) (setf low (1+ mid)))
                                   (t (return (+ gid (- cp start)))))))
                      finally (return 0))))
            (cmap-mappings-fn loader)
            (lambda (fn)
              (dotimes (g num-groups)
                (multiple-value-bind (start end gid) (group g)
                  (loop for cp from start to (min end #x10FFFF)
                        do (funcall fn cp (+ gid (- cp start)))))))))))

(defun setup-cmap4-lookup (loader fd sub-off table-end)
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) sub-off table-end))
  (let* ((sub-len (cmap-subtable-length fd sub-off 4 table-end))
         (sub-end (+ sub-off sub-len))
         (cmap4 (runes/otf:read-cmap4 fd sub-off sub-len))
         ;; Four u16 arrays of segCount entries follow the 16-byte header.
         (seg-len (min (floor (runes/otf:cmap4-seg-count-x2 cmap4) 2)
                       (max 0 (floor (- sub-len 16) 8))))
         (ro-base (+ sub-off 16 (* 6 seg-len))))
    (labels ((segment-glyph (s cp)
               (declare (type (unsigned-byte 16) s) (type (unsigned-byte 32) cp))
               (let ((start (runes/otf:cmap4-start-code-ref cmap4 s))
                     (delta (runes/otf:cmap4-id-delta-ref cmap4 s))
                     (ro-off (runes/otf:cmap4-id-range-offsets-ref cmap4 s)))
                 (cond
                   ((< cp start) 0)
                   ((zerop ro-off) (logand #xFFFF (+ cp delta)))
                   (t
                    (let ((g-addr (+ ro-base (* 2 s) ro-off (* 2 (- cp start)))))
                      ;; A malformed idRangeOffset must not read past the subtable.
                      (if (> (+ g-addr 2) sub-end)
                          0
                          (let ((gid (runes/otf:font-data-read-u16 fd g-addr)))
                            (if (zerop gid) 0 (logand #xFFFF (+ gid delta))))))))))
             (find-segment (cp)
               ;; First segment whose endCode >= CP.
               (let ((low 0) (high seg-len))
                 (loop while (< low high)
                       do (let ((mid (floor (+ low high) 2)))
                            (if (< (runes/otf:cmap4-end-code-ref cmap4 mid) cp)
                                (setf low (1+ mid))
                                (setf high mid))))
                 (when (< low seg-len) low))))
      (setf (cmap-lookup-fn loader)
            (lambda (cp)
              (let ((s (and (<= cp #xFFFF) (find-segment cp))))
                (if s (segment-glyph s cp) 0)))
            (cmap-mappings-fn loader)
            (lambda (fn)
              (dotimes (s seg-len)
                (loop for cp from (runes/otf:cmap4-start-code-ref cmap4 s)
                        to (min (runes/otf:cmap4-end-code-ref cmap4 s) #xFFFE)
                      for gid = (segment-glyph s cp)
                      unless (zerop gid) do (funcall fn cp gid))))))))

(defun setup-cmap-lookup (loader cmap cmap-offset cmap-length)
  (let ((fd (font-data loader))
        (table-end (+ cmap-offset cmap-length)))
    (multiple-value-bind (format sub-off) (best-cmap-subtable fd cmap cmap-offset)
      (case format
        (12 (setup-cmap12-lookup loader fd sub-off table-end))
        (4 (setup-cmap4-lookup loader fd sub-off table-end))
        (t (setf (cmap-lookup-fn loader) (constantly 0)))))))

(defun read-long-metrics (data offset size long-count glyph-count)
  "Read an hmtx/vmtx style table: LONG-COUNT (advance, bearing) pairs
followed by bare bearings for the remaining glyphs. Return a vector of
advances and a vector of GLYPH-COUNT bearings."
  (declare (type runes/otf:font-data data) (type (unsigned-byte 32) offset size)
           (type (unsigned-byte 16) long-count glyph-count))
  (let* ((long-count (min long-count (floor size 4)))
         (advances (make-array long-count))
         (bearings (make-array (max glyph-count long-count) :initial-element 0))
         (trailing-end (+ offset size)))
    (dotimes (i long-count)
      (let ((pos (+ offset (* i 4))))
        (setf (aref advances i) (runes/otf:font-data-read-u16 data pos)
              (aref bearings i) (runes/otf:font-data-read-i16 data (+ pos 2)))))
    (loop for i from long-count below glyph-count
          for pos = (+ offset (* long-count 4) (* (- i long-count) 2))
          while (<= (+ pos 2) trailing-end)
          do (setf (aref bearings i) (runes/otf:font-data-read-i16 data pos)))
    (values advances bearings)))

(defun read-kerning-format-0 (loader body kend)
  "Format 0: an ordered list of (left, right, value) pairs at BODY."
  (declare (type (unsigned-byte 32) body kend))
  (let* ((data (font-data loader))
         (n-pairs (min (runes/otf:font-data-read-u16 data body)
                       (floor (max 0 (- kend body 8)) 6)))
         (pos (+ body 8)))
    (dotimes (p n-pairs)
      (let ((left (runes/otf:font-data-read-u16 data pos))
            (right (runes/otf:font-data-read-u16 data (+ pos 2)))
            (val (runes/otf:font-data-read-i16 data (+ pos 4))))
        ;; Apple specifies a terminating #xFFFF/#xFFFF entry with value 0.
        (unless (and (= left #xFFFF) (= right #xFFFF) (zerop val))
          (setf (gethash (logior (ash left 16) right) (kerning-table loader)) val))
        (incf pos 6)))))

(defun read-kerning-class-table (data pos kend)
  "Return the first glyph and the vector of offsets of the class table at POS."
  (declare (type runes/otf:font-data data) (type (unsigned-byte 32) pos kend))
  (if (> (+ pos 4) kend)
      (values 0 (vector))
      (let* ((first (runes/otf:font-data-read-u16 data pos))
             (count (min (runes/otf:font-data-read-u16 data (+ pos 2))
                         (floor (- kend pos 4) 2)))
             (offsets (make-array count)))
        (dotimes (i count)
          (setf (aref offsets i) (runes/otf:font-data-read-u16 data (+ pos 4 (* 2 i)))))
        (values first offsets))))

(defun read-kerning-format-2 (loader sub-off body kend)
  "Format 2: a class-based kerning array. The class-table offsets, and the
pre-multiplied row and column offsets they hold, are relative to the
start of the subtable; a pair's value is at SUB-OFF + left + right."
  (declare (type (unsigned-byte 32) sub-off body kend))
  (let ((data (font-data loader)))
    (when (<= (+ body 8) kend)
      (multiple-value-bind (left-first left-offsets)
          (read-kerning-class-table data (+ sub-off (runes/otf:font-data-read-u16 data (+ body 2))) kend)
        (multiple-value-bind (right-first right-offsets)
            (read-kerning-class-table data (+ sub-off (runes/otf:font-data-read-u16 data (+ body 4))) kend)
          (loop for lv across left-offsets
                for left from left-first
                do (loop for rv across right-offsets
                         for right from right-first
                         for pos = (+ sub-off lv rv)
                         when (<= (+ pos 2) kend)
                           do (let ((val (runes/otf:font-data-read-i16 data pos)))
                                (unless (zerop val)
                                  (setf (gethash (logior (ash left 16) right) (kerning-table loader))
                                        val))))))))))

(defun read-kerning-subtable (loader format sub-off body kend)
  (case format
    (0 (read-kerning-format-0 loader body kend))
    (2 (read-kerning-format-2 loader sub-off body kend))
    ;; Format 1 (contextual state table) and others are ignored.
    ))

(defun load-microsoft-kern-table (loader koff kend)
  "The subtables of a version 0 'kern' table.  Their header: version u16,
length u16, coverage u16 with the format in the high byte and flags in the
low byte."
  (declare (type (unsigned-byte 32) koff kend))
  (let ((data (font-data loader))
        (n-tables (runes/otf:font-data-read-u16 (font-data loader) (+ koff 2)))
        (sub-off (+ koff 4)))
    (dotimes (t-idx n-tables)
      (when (> (+ sub-off 6) kend) (return))
      (let* ((sub-len (runes/otf:font-data-read-u16 data (+ sub-off 2)))
             (sub-cov (runes/otf:font-data-read-u16 data (+ sub-off 4)))
             (sub-end (min kend (+ sub-off (max sub-len 6)))))
        ;; Horizontal, not minimum values, not cross-stream.
        (when (= (logand sub-cov #b111) 1)
          (read-kerning-subtable loader (ldb (byte 8 8) sub-cov) sub-off (+ sub-off 6)
                                 ;; Fonts with many pairs overflow the 16-bit length.
                                 (if (= t-idx (1- n-tables)) kend sub-end)))
        (when (< sub-len 6) (return))
        (incf sub-off sub-len)))))

(defun load-apple-kern-table (loader koff kend)
  "The subtables of a version 1.0 'kern' table.  Their header: length u32,
coverage u16 with flags in the high byte and the format in the low byte,
tupleIndex u16."
  (declare (type (unsigned-byte 32) koff kend))
  (let* ((data (font-data loader))
         (n-tables (min (runes/otf:font-data-read-u32 data (+ koff 4))
                        (floor (max 0 (- kend koff 8)) 8)))
         (sub-off (+ koff 8)))
    (dotimes (t-idx n-tables)
      (when (> (+ sub-off 8) kend) (return))
      (let ((sub-len (runes/otf:font-data-read-u32 data sub-off))
            (sub-cov (runes/otf:font-data-read-u16 data (+ sub-off 4))))
        ;; Not vertical, not cross-stream, not variation.
        (when (zerop (logand sub-cov #xE000))
          (read-kerning-subtable loader (ldb (byte 8 0) sub-cov) sub-off (+ sub-off 8)
                                 (min kend (+ sub-off (max sub-len 8)))))
        (when (< sub-len 8) (return))
        (incf sub-off sub-len)))))

(defun load-kern-table (loader koff ksize)
  "Read the horizontal format 0 and format 2 subtables of the Microsoft
(version 0) or Apple (version 1.0) 'kern' table into the loader's
kerning table."
  (let ((data (font-data loader))
        (kend (+ koff ksize)))
    (case (runes/otf:font-data-read-u16 data koff)
      (0 (load-microsoft-kern-table loader koff kend))
      ;; Version 1.0: a 16.16 version whose low half is 0.
      (1 (when (zerop (runes/otf:font-data-read-u16 data (+ koff 2)))
           (load-apple-kern-table loader koff kend))))))

(defparameter *sfnt-versions*
  (list #x00010000 #x4F54544F #x74727565 #x74797031) ; 1.0, 'OTTO', 'true', 'typ1'
  "Valid sfntVersion values at the start of a font's table directory.")

(defmacro with-table ((view info) (tag loader reader) &body body)
  "When LOADER's font has the table TAG, run BODY with INFO bound to its
TABLE-INFO and VIEW to the table read with READER."
  `(let ((,info (table-info ,tag ,loader)))
     (when ,info
       (let ((,view (,reader (font-data ,loader) (offset ,info) (size ,info))))
         (declare (ignorable ,view))
         ,@body))))

(defun load-maxp (loader)
  (with-table (maxp info) ("maxp" loader runes/otf:read-maxp)
    (setf (glyph-count loader) (runes/otf:maxp-num-glyphs maxp))))

(defun load-head (loader)
  (with-table (head info) ("head" loader runes/otf:read-head)
    (setf (units/em loader) (runes/otf:head-units-per-em head)
          (bounding-box loader) (vector (runes/otf:head-x-min head) (runes/otf:head-y-min head)
                                        (runes/otf:head-x-max head) (runes/otf:head-y-max head))
          (loca-offset-format loader) (if (zerop (runes/otf:head-index-to-loc-format head))
                                          :short
                                          :long))))

(defun load-horizontal-metrics (loader)
  "'hhea', and the advance widths and left side bearings of 'hmtx'."
  (with-table (hhea info) ("hhea" loader runes/otf:read-hhea)
    (setf (ascender loader) (runes/otf:hhea-ascender hhea)
          (descender loader) (runes/otf:hhea-descender hhea)
          (line-gap loader) (runes/otf:hhea-line-gap hhea)
          (max-width loader) (runes/otf:hhea-advance-width-max hhea))
    (with-table (hmtx hmtx-info) ("hmtx" loader runes/otf:read-hmtx)
      (multiple-value-bind (widths lsbs)
          (read-long-metrics (font-data loader) (offset hmtx-info) (size hmtx-info)
                             (runes/otf:hhea-number-of-h-metrics hhea) (glyph-count loader))
        (setf (advance-widths loader) widths
              (left-side-bearings loader) lsbs)))))

(defun load-vertical-metrics (loader)
  "'vhea' and 'vmtx' or, without both, vertical metrics made up from the
horizontal ones."
  (let ((vhea-info (table-info "vhea" loader))
        (vmtx-info (table-info "vmtx" loader))
        (data (font-data loader)))
    (if (and vhea-info vmtx-info)
        (let ((vhea (runes/otf:read-vhea data (offset vhea-info) (size vhea-info))))
          (runes/otf:read-vmtx data (offset vmtx-info) (size vmtx-info))
          (setf (vascender loader) (runes/otf:vhea-ascender vhea)
                (vdescender loader) (runes/otf:vhea-descender vhea))
          (multiple-value-bind (heights tsbs)
              (read-long-metrics data (offset vmtx-info) (size vmtx-info)
                                 (runes/otf:vhea-number-of-long-ver-metrics vhea) (glyph-count loader))
            (setf (advance-heights loader) heights
                  (top-side-bearings loader) tsbs)))
        (let ((dx (/ (max-width loader) 2)))
          (setf (vhea-missing-p loader) t
                (vmtx-missing-p loader) t
                (vascender loader) dx
                (vdescender loader) (- dx)
                (advance-heights loader) (vector (- (ascender loader) (descender loader))))))))

(defun load-post (loader)
  (with-table (post info) ("post" loader runes/otf:read-post)
    (setf (italic-angle loader) (runes/otf:post-italic-angle post)
          (fixed-pitch-p loader) (not (zerop (runes/otf:post-is-fixed-pitch post)))
          (underline-position loader) (runes/otf:post-underline-position post)
          (underline-thickness loader) (runes/otf:post-underline-thickness post))))

(defun load-names (loader)
  "Every entry of the 'name' table, decoded."
  (with-table (name info) ("name" loader runes/otf:read-name)
    (let ((storage (+ (offset info) (runes/otf:name-storage-offset name)))
          (entries (make-array (runes/otf:name-count name))))
      (dotimes (i (length entries))
        (let* ((rec (runes/otf:name-name-record-ref name i))
               (plat (runes/otf:name-record-platform-id rec))
               (enc (runes/otf:name-record-encoding-id rec)))
          (setf (aref entries i)
                (make-instance 'name-entry
                               :font-loader loader
                               :platform-id plat
                               :encoding-id enc
                               :language-id (runes/otf:name-record-language-id rec)
                               :name-id (runes/otf:name-record-name-id rec)
                               :value (decode-name-string-from-fd
                                       (font-data loader)
                                       (+ storage (runes/otf:name-record-string-offset rec))
                                       (runes/otf:name-record-length rec)
                                       plat enc)))))
      (setf (name-entries loader) entries))))

(defun load-cmap (loader)
  (with-table (cmap info) ("cmap" loader runes/otf:read-cmap)
    (setup-cmap-lookup loader cmap (offset info) (size info))))

(defun load-kern (loader)
  "The 'kern' table; a truncated one gives what was read before the end."
  (let ((info (table-info "kern" loader)))
    (when info
      (handler-case (load-kern-table loader (offset info) (size info))
        (runes/otf:font-out-of-bounds-error () nil)))))

(defun collection-font-offset (data collection-index)
  "The position of font COLLECTION-INDEX's table directory, and the number
of fonts in the collection (NIL for a single font)."
  (if (/= (runes/otf:font-data-read-u32 data 0) #x74746366) ; 'ttcf'
      (values 0 nil)
      (let ((num-fonts (runes/otf:font-data-read-u32 data 8)))
        (when (>= collection-index num-fonts)
          (error 'unsupported-value
                 :description "Font index out of range in collection"
                 :actual-value collection-index
                 :expected-values (list num-fonts)))
        (values (runes/otf:font-data-read-u32 data (+ 12 (* collection-index 4)))
                num-fonts))))

(defun open-font-loader-from-data (data &key (collection-index 0) input-stream)
  ;; WOFF and WOFF2 ('wOFF', 'wOF2'): decode to an sfnt (or TTC) in memory first
  (let ((decoder (when (>= (runes/otf:font-data-length data) 4)
                   (case (runes/otf:font-data-read-u32 data 0)
                     (#x774F4646 #'runes/otf:decode-woff1)
                     (#x774F4632 #'runes/otf:decode-woff2)))))
    (when decoder
      (return-from open-font-loader-from-data
        (open-font-loader-from-data (funcall decoder data)
                                    :collection-index collection-index
                                    :input-stream input-stream))))
  (multiple-value-bind (font-offset collection-count) (collection-font-offset data collection-index)
    (let ((sfnt-version (runes/otf:font-data-read-u32 data font-offset)))
      (unless (member sfnt-version *sfnt-versions*)
        (error 'bad-magic
               :location (format nil "table directory at offset ~D" font-offset)
               :actual-value sfnt-version
               :expected-values *sfnt-versions*)))
    (let* ((dir (runes/otf:read-table-directory data font-offset))
           (num-tables (runes/otf:table-directory-num-tables dir))
           (loader (make-instance 'font-loader
                                  :font-data data
                                  :input-stream input-stream
                                  :table-count num-tables
                                  :collection-font-count collection-count
                                  :collection-font-index (when collection-count collection-index))))
      (dotimes (i num-tables)
        (let* ((rec (runes/otf:table-directory-table-records-ref dir i))
               (tag (runes/otf:table-record-tag rec)))
          (setf (gethash tag (tables loader))
                (make-instance 'table-info :name tag
                                           :offset (runes/otf:table-record-offset rec)
                                           :size (runes/otf:table-record-length rec)))))
      ;; In this order: 'hmtx' needs the glyph count from 'maxp', and the
      ;; made-up vertical metrics need 'hhea'.
      (load-maxp loader)
      (load-head loader)
      (load-horizontal-metrics loader)
      (load-vertical-metrics loader)
      (load-post loader)
      (load-names loader)
      (load-cmap loader)
      (load-kern loader)
      (setf (glyph-cache loader) (make-array (glyph-count loader) :initial-element nil))
      loader)))

(defun open-font-loader (thing &key (collection-index 0))
  (cond
    ((streamp thing)
     (let ((ok nil))
       (unwind-protect
            (let* ((len (file-length thing))
                   (buf (make-array len :element-type '(unsigned-byte 8))))
              (file-position thing 0)
              (read-sequence buf thing)
              (prog1 (open-font-loader-from-data (runes/otf:make-font-data buf)
                                                 :collection-index collection-index
                                                 :input-stream thing)
                (setf ok t)))
         ;; The loader owns the stream; don't leak it when loading fails.
         (unless ok (close thing)))))
    (t
     (let ((data (runes/otf:ensure-font-data thing)))
       (open-font-loader-from-data data :collection-index collection-index)))))
