;;;; gpos-kerning.lisp — Pair kerning from the 'GPOS' table
;;;; Copyright (c) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

;;; Modern fonts keep their kerning in 'GPOS', as PairPos lookups of the
;;; 'kern' feature, and many have no 'kern' table at all.  KERNING-OFFSET
;;; uses them as HarfBuzz does for a pair of glyphs: the lookups of every
;;; 'kern' feature, in lookup order, each adding the x advance adjustment
;;; of the first subtable that covers the pair.  The result is the font's
;;; static kerning: device tables and variation deltas are not applied, and
;;; the lookup flags are not, since a pair has no glyphs to skip.
;;;
;;; The structures are read in place: a lookup for every pair of every
;;; string must not allocate.  Offsets are relative to the start of the
;;; structure that holds them, as the OpenType common table formats say.
;;;
;;; The search code runs at safety 0: it only computes positions from
;;; 16-bit values, and reads through the RUNES/OTF:FONT-DATA-READ-*
;;; functions, which keep their own bounds and type checks when inlined.
;;; The entry points, which take values from callers, run at safety 1.

(in-package #:zpb-ttf)

(deftype table-position () '(unsigned-byte 32))
(deftype glyph-index () '(unsigned-byte 16))

(declaim (inline gpos-u16))
(defun gpos-u16 (fd pos)
  (runes/otf:font-data-read-u16 fd pos))

;;; Finding the 'kern' lookups

(defun kern-lookup-indices (fd feature-list)
  "The lookup indices of every 'kern' feature in the FeatureList at
FEATURE-LIST, whatever its script and language, sorted and without
duplicates."
  (declare (type runes/otf:font-data fd) (type table-position feature-list))
  (let ((indices '()))
    (loop for i below (gpos-u16 fd feature-list)
          for record = (+ feature-list 2 (* 6 i))
          when (= (runes/otf:font-data-read-u32 fd record) #x6B65726E) ; 'kern'
            do (loop with feature = (+ feature-list (gpos-u16 fd (+ record 4)))
                     for k below (gpos-u16 fd (+ feature 2))
                     do (pushnew (gpos-u16 fd (+ feature 4 (* 2 k))) indices)))
    (sort indices #'<)))

(defun pair-pos-subtables (fd lookup end)
  "The positions of the PairPos subtables of the Lookup at LOOKUP, with an
Extension lookup's subtables resolved to the ones they wrap.  An Extension
that points past END, the end of 'GPOS', is skipped."
  (declare (type runes/otf:font-data fd) (type table-position lookup end))
  (loop with type = (gpos-u16 fd lookup)
        for s below (gpos-u16 fd (+ lookup 4))
        for sub of-type table-position = (+ lookup (gpos-u16 fd (+ lookup 6 (* 2 s))))
        for extension-p = (= type 9)
        for real = (if extension-p
                       (+ sub (runes/otf:font-data-read-u32 fd (+ sub 4)))
                       sub)
        when (and (< real end)
                  (= (if extension-p (gpos-u16 fd (+ sub 2)) type) 2))
          collect real))

(defun gpos-kern-subtables (loader)
  "For each lookup of a 'kern' feature of LOADER's 'GPOS' that has PairPos
subtables, in lookup order, their positions; NIL when there are none."
  (let ((info (table-info "GPOS" loader))
        (fd (font-data loader)))
    (when info
      (loop with gpos of-type table-position = (offset info)
            with end of-type table-position = (+ gpos (size info))
            with lookup-list of-type table-position = (+ gpos (gpos-u16 fd (+ gpos 8)))
            for index in (kern-lookup-indices fd (+ gpos (gpos-u16 fd (+ gpos 6))))
            for subtables = (when (< index (gpos-u16 fd lookup-list))
                              (pair-pos-subtables
                               fd (+ lookup-list (gpos-u16 fd (+ lookup-list 2 (* 2 index)))) end))
            when subtables
              collect subtables))))

(defun gpos-kern-lookups (loader)
  "GPOS-KERN-SUBTABLES of LOADER, found once.  A 'GPOS' that cannot be read
gives no kerning."
  (let ((lookups (%gpos-kern-lookups loader)))
    (if (eq lookups :unknown)
        (setf (%gpos-kern-lookups loader)
              (handler-case (gpos-kern-subtables loader)
                (runes/otf:font-error () nil)))
        lookups)))

;;; Searching sorted records

(defun find-glyph-record (fd start count size glyph)
  "The position of the record for GLYPH among COUNT records of SIZE bytes
from START, sorted by the glyph index that starts each; NIL if none."
  (declare (optimize (speed 3) (safety 0))
           (type runes/otf:font-data fd) (type table-position start)
           (type (unsigned-byte 16) count) (type (integer 2 64) size) (type glyph-index glyph))
  (let ((lo 0) (hi count))
    (declare (type (unsigned-byte 17) lo hi))
    (loop while (< lo hi)
          do (let* ((mid (ash (+ lo hi) -1))
                    (record (+ start (* size mid)))
                    (g (gpos-u16 fd record)))
               (cond ((< g glyph) (setf lo (1+ mid)))
                     ((> g glyph) (setf hi mid))
                     (t (return record)))))))

(defun find-range-record (fd start count glyph)
  "The position of the 6-byte record (first glyph, last glyph, value)
whose range holds GLYPH, among COUNT sorted ones from START; NIL if none."
  (declare (optimize (speed 3) (safety 0))
           (type runes/otf:font-data fd) (type table-position start)
           (type (unsigned-byte 16) count) (type glyph-index glyph))
  (let ((lo 0) (hi count))
    (declare (type (unsigned-byte 17) lo hi))
    (loop while (< lo hi)
          do (let* ((mid (ash (+ lo hi) -1))
                    (record (+ start (* 6 mid))))
               (cond ((< glyph (gpos-u16 fd record)) (setf hi mid))
                     ((> glyph (gpos-u16 fd (+ record 2))) (setf lo (1+ mid)))
                     (t (return record)))))))

;;; Coverage and class definitions

(defun coverage-index (fd coverage glyph)
  "GLYPH's index in the Coverage table at COVERAGE, or NIL."
  (declare (optimize (speed 3) (safety 0))
           (type runes/otf:font-data fd) (type table-position coverage) (type glyph-index glyph))
  (let ((count (gpos-u16 fd (+ coverage 2)))
        (records (+ coverage 4)))
    (case (gpos-u16 fd coverage)
      ;; A sorted glyph array.
      (1 (let ((record (find-glyph-record fd records count 2 glyph)))
           (when record (ash (- record records) -1))))
      ;; Ranges, each with the coverage index of its first glyph.
      (2 (let ((range (find-range-record fd records count glyph)))
           (when range
             (+ (gpos-u16 fd (+ range 4)) (- glyph (gpos-u16 fd range)))))))))

(defun glyph-class (fd class-def glyph)
  "GLYPH's class in the ClassDef table at CLASS-DEF; 0 for a glyph it does
not list."
  (declare (optimize (speed 3) (safety 0))
           (type runes/otf:font-data fd) (type table-position class-def) (type glyph-index glyph))
  (or (case (gpos-u16 fd class-def)
        ;; Classes for a run of glyphs from startGlyphID.
        (1 (let ((start (gpos-u16 fd (+ class-def 2))))
             (when (and (>= glyph start) (< (- glyph start) (gpos-u16 fd (+ class-def 4))))
               (gpos-u16 fd (+ class-def 6 (* 2 (- glyph start)))))))
        ;; Ranges, each with a class.
        (2 (let ((range (find-range-record fd (+ class-def 4) (gpos-u16 fd (+ class-def 2)) glyph)))
             (when range (gpos-u16 fd (+ range 4))))))
      0))

;;; Pair adjustments

(declaim (inline value-record-size value-record-x-advance))

(defun value-record-size (format)
  "Size in bytes of a ValueRecord: two bytes for each bit of FORMAT."
  (declare (type (unsigned-byte 16) format))
  (* 2 (logcount (logand format #xFF))))

(defun value-record-x-advance (fd pos format)
  "The XAdvance of the ValueRecord at POS, or 0 if FORMAT has none.  It
follows XPlacement and YPlacement, when those are present."
  (declare (type runes/otf:font-data fd) (type table-position pos) (type (unsigned-byte 16) format))
  (if (logbitp 2 format)
      (runes/otf:font-data-read-i16 fd (+ pos (* 2 (logcount (logand format 3)))))
      0))

(defun pair-pos-adjustment (fd sub left right)
  "The x advance adjustment of LEFT before RIGHT from the PairPos subtable
at SUB, or NIL when the subtable does not cover the pair."
  (declare (optimize (speed 3) (safety 0))
           (type runes/otf:font-data fd) (type table-position sub) (type glyph-index left right))
  (let ((index (coverage-index fd (+ sub (gpos-u16 fd (+ sub 2))) left))
        (format1 (gpos-u16 fd (+ sub 4)))
        (format2 (gpos-u16 fd (+ sub 6))))
    (when index
      (case (gpos-u16 fd sub)
        ;; Format 1: for each covered first glyph, a set of records sorted
        ;; by the second glyph, each with two ValueRecords.
        (1 (when (< index (gpos-u16 fd (+ sub 8)))
             (let* ((set (+ sub (gpos-u16 fd (+ sub 10 (* 2 index)))))
                    (record-size (+ 2 (value-record-size format1) (value-record-size format2)))
                    (record (find-glyph-record fd (+ set 2) (gpos-u16 fd set) record-size right)))
               (when record
                 (value-record-x-advance fd (+ record 2) format1)))))
        ;; Format 2: two ValueRecords for each class of the first glyph and
        ;; class of the second.
        (2 (let ((class1 (glyph-class fd (+ sub (gpos-u16 fd (+ sub 8))) left))
                 (class2 (glyph-class fd (+ sub (gpos-u16 fd (+ sub 10))) right))
                 (class2-count (gpos-u16 fd (+ sub 14)))
                 (record-size (+ (value-record-size format1) (value-record-size format2))))
             (when (and (< class1 (gpos-u16 fd (+ sub 12))) (< class2 class2-count))
               (value-record-x-advance
                fd (+ sub 16 (* record-size (+ (* class1 class2-count) class2))) format1))))))))

(defun gpos-kerning (loader left right)
  "The kerning of glyph indices LEFT and RIGHT from LOADER's 'GPOS' table,
in font units."
  (declare (type glyph-index left right))
  (loop with fd = (font-data loader)
        for subtables in (gpos-kern-lookups loader)
        sum (or (loop for sub in subtables
                      thereis (pair-pos-adjustment fd sub left right))
                0)
          of-type fixnum))
