;;;; woff2.lisp — WOFF2 decoding into SFNT font-data
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:runes/otf)

(defconstant +woff2-signature+ #x774F4632) ; 'wOF2'
(defconstant +ttcf-signature+  #x74746366) ; 'ttcf'

(defconstant +tag-glyf+ #x676C7966) ; 'glyf'
(defconstant +tag-loca+ #x6C6F6361) ; 'loca'
(defconstant +tag-hmtx+ #x686D7478) ; 'hmtx'
(defconstant +tag-head+ #x68656164) ; 'head'
(defconstant +tag-hhea+ #x68686561) ; 'hhea'
(defconstant +tag-maxp+ #x6D617870) ; 'maxp'

;;; WOFF2 Known Table Tags (Section 3.2, indices 0..62)
(alexandria:define-constant +woff2-known-tags+
  #(#x636D6170 ;  0 'cmap'
    #x68656164 ;  1 'head'
    #x68686561 ;  2 'hhea'
    #x686D7478 ;  3 'hmtx'
    #x6D617870 ;  4 'maxp'
    #x6E616D65 ;  5 'name'
    #x4F532F32 ;  6 'OS/2'
    #x706F7374 ;  7 'post'
    #x63767420 ;  8 'cvt '
    #x6670676D ;  9 'fpgm'
    #x676C7966 ; 10 'glyf'
    #x6C6F6361 ; 11 'loca'
    #x70726570 ; 12 'prep'
    #x43464620 ; 13 'CFF '
    #x564F5247 ; 14 'VORG'
    #x45424454 ; 15 'EBDT'
    #x45424C43 ; 16 'EBLC'
    #x67617370 ; 17 'gasp'
    #x68646D78 ; 18 'hdmx'
    #x6B65726E ; 19 'kern'
    #x4C545348 ; 20 'LTSH'
    #x50434C54 ; 21 'PCLT'
    #x56444D58 ; 22 'VDMX'
    #x76686561 ; 23 'vhea'
    #x766D7478 ; 24 'vmtx'
    #x42415345 ; 25 'BASE'
    #x47444546 ; 26 'GDEF'
    #x47504F53 ; 27 'GPOS'
    #x47535542 ; 28 'GSUB'
    #x45425343 ; 29 'EBSC'
    #x4A535446 ; 30 'JSTF'
    #x4D415448 ; 31 'MATH'
    #x43424454 ; 32 'CBDT'
    #x43424C43 ; 33 'CBLC'
    #x434F4C52 ; 34 'COLR'
    #x4350414C ; 35 'CPAL'
    #x53564720 ; 36 'SVG '
    #x73626978 ; 37 'sbix'
    #x61636E74 ; 38 'acnt'
    #x61766172 ; 39 'avar'
    #x62646174 ; 40 'bdat'
    #x626C6F63 ; 41 'bloc'
    #x62736C6E ; 42 'bsln'
    #x63766172 ; 43 'cvar'
    #x66647363 ; 44 'fdsc'
    #x66656174 ; 45 'feat'
    #x666D7478 ; 46 'fmtx'
    #x66766172 ; 47 'fvar'
    #x67766172 ; 48 'gvar'
    #x68737479 ; 49 'hsty'
    #x6A757374 ; 50 'just'
    #x6C636172 ; 51 'lcar'
    #x6D6F7274 ; 52 'mort'
    #x6D6F7278 ; 53 'morx'
    #x6F706264 ; 54 'opbd'
    #x70726F70 ; 55 'prop'
    #x7472616B ; 56 'trak'
    #x5A617066 ; 57 'Zapf'
    #x53696C66 ; 58 'Silf'
    #x476C6174 ; 59 'Glat'
    #x476C6F63 ; 60 'Gloc'
    #x46656174 ; 61 'Feat'
    #x53696C6C) ; 62 'Sill'
  :test 'equalp)

(defun woff-fail (table fmt &rest args)
  (error 'font-format-error
         :table table
         :message (apply #'format nil fmt args)))

;;; ============================================================================
;;; Byte Stream Cursor and Big-Endian Writers
;;; ============================================================================

(defstruct (bstream (:constructor make-bstream (bytes pos end)))
  (bytes nil :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum)
  (end 0 :type fixnum))

(declaim (inline bstream-from-font-data bstream-sub bstream-remaining
                 bs-u8 bs-u16 bs-i16 bs-u32 bs-255u16 bs-base128
                 put-u8 put-u16 put-i16 put-u32 round4))

(defun bstream-from-font-data (fd)
  (let ((off (font-data-offset fd)))
    (make-bstream (font-data-bytes fd) off (+ off (font-data-length fd)))))

(defun bstream-sub (bs size &optional (table "WOFF2"))
  (let ((p (bstream-pos bs))
        (end (bstream-end bs)))
    (when (> size (- end p))
      (woff-fail table "substream of ~D bytes exceeds remaining ~D bytes" size (- end p)))
    (setf (bstream-pos bs) (+ p size))
    (make-bstream (bstream-bytes bs) p (+ p size))))

(defun bstream-remaining (bs)
  (- (bstream-end bs) (bstream-pos bs)))

(defun bs-u8 (bs &optional (table "WOFF2"))
  (let ((p (bstream-pos bs)))
    (when (>= p (bstream-end bs))
      (woff-fail table "unexpected end of stream"))
    (setf (bstream-pos bs) (1+ p))
    (aref (bstream-bytes bs) p)))

(defun bs-u16 (bs &optional (table "WOFF2"))
  (let ((p (bstream-pos bs))
        (bytes (bstream-bytes bs)))
    (when (> (+ p 2) (bstream-end bs))
      (woff-fail table "unexpected end of stream reading u16"))
    (setf (bstream-pos bs) (+ p 2))
    (logior (ash (aref bytes p) 8)
            (aref bytes (1+ p)))))

(defun bs-i16 (bs &optional (table "WOFF2"))
  (let ((u (bs-u16 bs table)))
    (if (logbitp 15 u) (- u 65536) u)))

(defun bs-u32 (bs &optional (table "WOFF2"))
  (let ((p (bstream-pos bs))
        (bytes (bstream-bytes bs)))
    (when (> (+ p 4) (bstream-end bs))
      (woff-fail table "unexpected end of stream reading u32"))
    (setf (bstream-pos bs) (+ p 4))
    (logior (ash (aref bytes p) 24)
            (ash (aref bytes (+ p 1)) 16)
            (ash (aref bytes (+ p 2)) 8)
            (aref bytes (+ p 3)))))

(defun bs-255u16 (bs &optional (table "WOFF2"))
  "Read a WOFF2 255UInt16 value."
  (let ((code (bs-u8 bs table)))
    (cond ((= code 253) (bs-u16 bs table))
          ((= code 254) (+ 506 (bs-u8 bs table)))
          ((= code 255) (+ 253 (bs-u8 bs table)))
          (t code))))

(defun bs-base128 (bs &optional (table "WOFF2"))
  "Read a WOFF2 UIntBase128 value."
  (let ((accum 0))
    (declare (type (unsigned-byte 32) accum))
    (dotimes (i 5 (woff-fail table "UIntBase128 exceeds 5 bytes"))
      (let ((b (bs-u8 bs table)))
        (when (and (zerop i) (= b #x80))
          (woff-fail table "leading zero in UIntBase128"))
        (when (logtest accum #xFE000000)
          (woff-fail table "UIntBase128 overflow"))
        (setf accum (logior (ash accum 7) (logand b #x7F)))
        (unless (logbitp 7 b)
          (return accum))))))

(defun put-u8 (buf pos v)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum pos))
  (setf (aref buf pos) (ldb (byte 8 0) v))
  (1+ pos))

(defun put-u16 (buf pos v)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum pos))
  (setf (aref buf pos)       (ldb (byte 8 8) v)
        (aref buf (+ pos 1)) (ldb (byte 8 0) v))
  (+ pos 2))

(defun put-i16 (buf pos v)
  (put-u16 buf pos (ldb (byte 16 0) v)))

(defun put-u32 (buf pos v)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum pos))
  (setf (aref buf pos)       (ldb (byte 8 24) v)
        (aref buf (+ pos 1)) (ldb (byte 8 16) v)
        (aref buf (+ pos 2)) (ldb (byte 8 8) v)
        (aref buf (+ pos 3)) (ldb (byte 8 0) v))
  (+ pos 4))

(defun round4 (n)
  (logand (+ n 3) -4))

(defun tag-to-string (tag)
  (let ((s (make-string 4)))
    (setf (schar s 0) (code-char (ldb (byte 8 24) tag))
          (schar s 1) (code-char (ldb (byte 8 16) tag))
          (schar s 2) (code-char (ldb (byte 8 8) tag))
          (schar s 3) (code-char (ldb (byte 8 0) tag)))
    s))

;;; ============================================================================
;;; Checksums and SFNT Header Construction
;;; ============================================================================

(defun compute-table-checksum (bytes start length)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type fixnum start length)
           (optimize (speed 3) (safety 1)))
  (let ((sum 0)
        (end (+ start (logand length -4)))
        (rem (logand length 3)))
    (declare (type (unsigned-byte 32) sum))
    (loop for p of-type fixnum from start below end by 4
          do (let ((w (logior (ash (aref bytes p) 24)
                              (ash (aref bytes (+ p 1)) 16)
                              (ash (aref bytes (+ p 2)) 8)
                              (aref bytes (+ p 3)))))
               (setf sum (ldb (byte 32 0) (+ sum w)))))
    (when (plusp rem)
      (let ((tail 0))
        (dotimes (i rem)
          (setf tail (logior tail (ash (aref bytes (+ end i)) (* 8 (- 3 i))))))
        (setf sum (ldb (byte 32 0) (+ sum tail)))))
    sum))

(defun sfnt-search-params (num-tables)
  (declare (type (unsigned-byte 16) num-tables))
  (let* ((max-pow2 (if (plusp num-tables)
                       (ash 1 (1- (integer-length num-tables)))
                       1))
         (search-range (* max-pow2 16))
         (entry-selector (if (plusp num-tables)
                             (1- (integer-length max-pow2))
                             0))
         (range-shift (- (* num-tables 16) search-range)))
    (values search-range entry-selector (max 0 range-shift))))

;;; ============================================================================
;;; WOFF2 Directory Structures
;;; ============================================================================

(defstruct woff2-table-entry
  (tag 0 :type (unsigned-byte 32))
  (flags 0 :type (unsigned-byte 8))
  (transform-version 0 :type (integer 0 3))
  (transformed-p nil :type boolean)
  (orig-length 0 :type (unsigned-byte 32))
  (transform-length 0 :type (unsigned-byte 32))
  (src-offset 0 :type (unsigned-byte 32))
  ;; Reconstructed or sliced bytes, and final SFNT placement
  (bytes nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (dst-offset 0 :type (unsigned-byte 32))
  (dst-length 0 :type (unsigned-byte 32))
  (checksum 0 :type (unsigned-byte 32)))

(defstruct woff2-collection-font
  (flavor 0 :type (unsigned-byte 32))
  (table-indices #() :type (simple-array (unsigned-byte 16) (*))))

(defun read-woff2-table-directory (bs num-tables)
  (let ((entries (make-array num-tables))
        (src-offset 0))
    (declare (type (unsigned-byte 64) src-offset))
    (dotimes (i num-tables)
      (let* ((flags (bs-u8 bs "WOFF2"))
             (tag-idx (logand flags #x3F))
             (xform-ver (ash flags -6))
             (tag (if (= tag-idx 63)
                      (bs-u32 bs "WOFF2")
                      (aref +woff2-known-tags+ tag-idx)))
             (glyf-or-loca-p (or (= tag +tag-glyf+) (= tag +tag-loca+)))
             (transformed-p (if glyf-or-loca-p
                                (case xform-ver
                                  (0 t)
                                  (3 nil)
                                  (t (woff-fail (tag-to-string tag)
                                                "invalid transform version ~D" xform-ver)))
                                (case xform-ver
                                  (0 nil)
                                  (1 (if (= tag +tag-hmtx+)
                                         t
                                         (woff-fail (tag-to-string tag)
                                                    "unexpected transform on table")))
                                  (t (woff-fail (tag-to-string tag)
                                                "invalid transform version ~D" xform-ver)))))
             (orig-len (bs-base128 bs "WOFF2"))
             (xform-len (if transformed-p
                            (bs-base128 bs "WOFF2")
                            orig-len)))
        (when (and (= tag +tag-loca+) transformed-p (not (zerop xform-len)))
          (woff-fail "loca" "transformed loca must have zero transformLength"))
        (setf (aref entries i)
              (make-woff2-table-entry :tag tag
                                      :flags flags
                                      :transform-version xform-ver
                                      :transformed-p transformed-p
                                      :orig-length orig-len
                                      :transform-length xform-len
                                      :src-offset src-offset))
        (incf src-offset xform-len)))
    (values entries src-offset)))

(defun read-woff2-collection-directory (bs num-total-tables)
  (let* ((version (bs-u32 bs "WOFF2"))
         (num-fonts (bs-255u16 bs "WOFF2")))
    (unless (or (= version #x00010000) (= version #x00020000))
      (woff-fail "WOFF2" "unsupported TTC header version #x~8,'0X" version))
    (when (zerop num-fonts)
      (woff-fail "WOFF2" "collection has zero fonts"))
    (let ((fonts (make-array num-fonts)))
      (dotimes (f num-fonts (values version fonts))
        (let* ((ntables (bs-255u16 bs "WOFF2"))
               (flavor (bs-u32 bs "WOFF2"))
               (indices (make-array ntables :element-type '(unsigned-byte 16))))
          (when (zerop ntables)
            (woff-fail "WOFF2" "collection font ~D has zero tables" f))
          (dotimes (j ntables)
            (let ((idx (bs-255u16 bs "WOFF2")))
              (when (>= idx num-total-tables)
                (woff-fail "WOFF2" "collection table index ~D out of range" idx))
              (setf (aref indices j) idx)))
          (setf (aref fonts f)
                (make-woff2-collection-font :flavor flavor :table-indices indices)))))))

;;; ============================================================================
;;; WOFF2 Transformed 'glyf' and 'loca' Reconstruction
;;; ============================================================================

(declaim (inline with-sign bitmap-bit-p))
(defun with-sign (flag val)
  (if (logbitp 0 flag) val (- val)))

(defun bitmap-bit-p (bs idx)
  (let ((byte (aref (bstream-bytes bs) (+ (bstream-pos bs) (ash idx -3)))))
    (logbitp (- 7 (logand idx 7)) byte)))

(defun decode-triplet (flag-bs glyph-bs)
  "Decode one point (dx, dy, on-curve-p) from FLAG-BS and GLYPH-BS."
  (let* ((raw (bs-u8 flag-bs "glyf"))
         (on-curve-p (not (logbitp 7 raw)))
         (f (logand raw #x7F))
         (dx 0)
         (dy 0))
    (declare (type (unsigned-byte 8) raw f)
             (type fixnum dx dy))
    (cond
      ((< f 10)
       (setf dy (with-sign f (+ (ash (logand f 14) 7) (bs-u8 glyph-bs "glyf")))))
      ((< f 20)
       (setf dx (with-sign f (+ (ash (logand (- f 10) 14) 7) (bs-u8 glyph-bs "glyf")))))
      ((< f 84)
       (let ((b0 (- f 20))
             (b1 (bs-u8 glyph-bs "glyf")))
         (setf dx (with-sign f (+ 1 (logand b0 #x30) (ash b1 -4)))
               dy (with-sign (ash f -1) (+ 1 (ash (logand b0 #x0C) 2) (logand b1 #x0F))))))
      ((< f 120)
       (multiple-value-bind (q r) (truncate (- f 84) 12)
         (let ((b1 (bs-u8 glyph-bs "glyf"))
               (b2 (bs-u8 glyph-bs "glyf")))
           (setf dx (with-sign f (+ 1 (ash q 8) b1))
                 dy (with-sign (ash f -1) (+ 1 (ash (ash r -2) 8) b2))))))
      ((< f 124)
       (let ((b1 (bs-u8 glyph-bs "glyf"))
             (b2 (bs-u8 glyph-bs "glyf"))
             (b3 (bs-u8 glyph-bs "glyf")))
         (setf dx (with-sign f (+ (ash b1 4) (ash b2 -4)))
               dy (with-sign (ash f -1) (+ (ash (logand b2 #x0F) 8) b3)))))
      (t
       (let ((b1 (bs-u8 glyph-bs "glyf"))
             (b2 (bs-u8 glyph-bs "glyf"))
             (b3 (bs-u8 glyph-bs "glyf"))
             (b4 (bs-u8 glyph-bs "glyf")))
         (setf dx (with-sign f (+ (ash b1 8) b2))
               dy (with-sign (ash f -1) (+ (ash b3 8) b4))))))
    (values dx dy on-curve-p)))

(defun scan-composite-components (comp-bs)
  "Scan composite glyph records starting at COMP-BS's current position.
Return (VALUES byte-length have-instructions-p) and advance COMP-BS."
  (let ((start (bstream-pos comp-bs))
        (have-instructions nil))
    (loop
      (let ((flags (bs-u16 comp-bs "glyf"))
            (_gid  (bs-u16 comp-bs "glyf")))
        (declare (ignore _gid))
        (when (logbitp 8 flags)
          (setf have-instructions t))
        (let ((arg-bytes (if (logbitp 0 flags) 4 2))
              (xform-bytes (cond ((logbitp 3 flags) 2)
                                 ((logbitp 6 flags) 4)
                                 ((logbitp 7 flags) 8)
                                 (t 0))))
          (bstream-sub comp-bs (+ arg-bytes xform-bytes) "glyf"))
        (unless (logbitp 5 flags)
          (return (values (- (bstream-pos comp-bs) start) have-instructions)))))))

(defun ensure-glyf-capacity (buf needed)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum needed))
  (if (<= needed (length buf))
      buf
      (let ((new (make-array (max needed (* 2 (length buf)))
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
        (replace new buf)
        new)))

;;; The streams of a transformed 'glyf' table, and scratch buffers for the
;;; points of one simple glyph, grown when a glyph needs more.
(defstruct (glyf-streams (:conc-name gs-))
  (n-contour nil :type bstream) (n-points nil :type bstream)
  (flag nil :type bstream) (glyph nil :type bstream)
  (composite nil :type bstream) (bbox-bitmap nil :type bstream)
  (bbox nil :type bstream) (instruction nil :type bstream)
  (overlap nil :type (or null bstream))
  (end-pts (make-array 256 :element-type '(unsigned-byte 16))
   :type (simple-array (unsigned-byte 16) (*)))
  (flags (make-array 1024 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (x-coords (make-array 2048 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (y-coords (make-array 2048 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*))))

;;; The reconstructed 'glyf' table, grown as glyphs are added.
(defstruct (glyf-out (:conc-name out-))
  (bytes nil :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum))

(declaim (inline out-u16 out-i16 out-u8 out-copy out-copy-stream s16))

(defun out-reserve (out count)
  (setf (out-bytes out) (ensure-glyf-capacity (out-bytes out) (+ (out-pos out) count))))

(defun out-u16 (out v) (setf (out-pos out) (put-u16 (out-bytes out) (out-pos out) v)))
(defun out-i16 (out v) (setf (out-pos out) (put-i16 (out-bytes out) (out-pos out) v)))
(defun out-u8 (out v)
  (declare (glyf-out out) (type (unsigned-byte 8) v))
  (setf (aref (out-bytes out) (out-pos out)) v)
  (incf (out-pos out)))

(defun out-copy (out bytes start end)
  (replace (out-bytes out) bytes :start1 (out-pos out) :start2 start :end2 end)
  (incf (out-pos out) (- end start)))

(defun out-copy-stream (out bs)
  (out-copy out (bstream-bytes bs) (bstream-pos bs) (bstream-end bs)))

(defun out-pad (out glyph-start)
  "Zero bytes up to a multiple of 4 from GLYPH-START."
  (declare (glyf-out out) (fixnum glyph-start))
  (loop repeat (- (round4 (- (out-pos out) glyph-start)) (- (out-pos out) glyph-start))
        do (out-u8 out 0)))

(defun write-composite-glyph (s out gid has-bbox)
  "Write composite glyph GID; return its xMin."
  (declare (glyf-streams s) (glyf-out out) (type (unsigned-byte 16) gid))
  (unless has-bbox
    (woff-fail "glyf" "composite glyph ~D missing explicit bbox" gid))
  (let* ((bbox (gs-bbox s))
         (xmin (bs-i16 bbox "glyf")) (ymin (bs-i16 bbox "glyf"))
         (xmax (bs-i16 bbox "glyf")) (ymax (bs-i16 bbox "glyf"))
         (composite (gs-composite s))
         (comp-start (bstream-pos composite))
         (start (out-pos out)))
    (multiple-value-bind (comp-len have-instr) (scan-composite-components composite)
      (let* ((instr-len (if have-instr (bs-255u16 (gs-glyph s) "glyf") 0))
             (instr (when have-instr (bstream-sub (gs-instruction s) instr-len "glyf"))))
        (out-reserve out (round4 (+ 10 comp-len (if have-instr (+ 2 instr-len) 0))))
        (dolist (v (list -1 xmin ymin xmax ymax)) (out-i16 out v))
        (out-copy out (bstream-bytes composite) comp-start (+ comp-start comp-len))
        (when have-instr
          (out-u16 out instr-len)
          (out-copy-stream out instr))
        (out-pad out start)
        xmin))))

(defun read-contour-ends (s gid n-contours)
  "Read the point counts of the N-CONTOURS contours of glyph GID into the
end point buffer; return the total number of points."
  (declare (glyf-streams s) (type (integer 1 32767) n-contours))
  (when (> n-contours (length (gs-end-pts s)))
    (setf (gs-end-pts s) (make-array n-contours :element-type '(unsigned-byte 16))))
  (loop with total = 0
        for c below n-contours
        for npts = (bs-255u16 (gs-n-points s) "glyf")
        do (when (zerop npts)
             (woff-fail "glyf" "empty contour in simple glyph ~D" gid))
           (incf total npts)
           (when (> total #xFFFF)
             (woff-fail "glyf" "too many points in simple glyph ~D" gid))
           (setf (aref (gs-end-pts s) c) (1- total))
        finally (return total)))

(defun s16 (v)
  "V wrapped to a signed 16-bit value."
  (declare (fixnum v))
  (let ((u (ldb (byte 16 0) v))) (if (logbitp 15 u) (- u 65536) u)))

(defun decode-simple-points (s gid total-pts)
  "Decode the TOTAL-PTS points of simple glyph GID into TrueType flags and
packed coordinates in the scratch buffers.  Return the lengths of the x and
y coordinates, and the bounding box of the points."
  (declare (glyf-streams s) (fixnum total-pts))
  (when (> total-pts (length (gs-flags s)))
    (setf (gs-flags s) (make-array total-pts :element-type '(unsigned-byte 8))
          (gs-x-coords s) (make-array (* 2 total-pts) :element-type '(unsigned-byte 8))
          (gs-y-coords s) (make-array (* 2 total-pts) :element-type '(unsigned-byte 8))))
  (let ((x 0) (y 0)
        (xmin #x7FFF) (ymin #x7FFF) (xmax (- #x8000)) (ymax (- #x8000))
        (x-len 0) (y-len 0)
        (overlap-p (and (gs-overlap s) (bitmap-bit-p (gs-overlap s) gid)))
        (flags (gs-flags s))
        (x-coords (gs-x-coords s))
        (y-coords (gs-y-coords s))
        (flag-bs (gs-flag s))
        (glyph-bs (gs-glyph s)))
    (declare (type fixnum x y xmin ymin xmax ymax x-len y-len))
    (flet ((coordinate (d coords len short-flags zero-flag)
             (declare (fixnum d len) (type (simple-array (unsigned-byte 8) (*)) coords))
             ;; The flag bits for delta D, after storing it at LEN of COORDS
             ;; as a byte, a word, or not at all; and the new length.
             (cond ((zerop d) (values zero-flag len))
                   ((<= -255 d 255)
                    (setf (aref coords len) (abs d))
                    (values (if (plusp d) (logior short-flags zero-flag) short-flags) (1+ len)))
                   (t (values 0 (put-i16 coords len d))))))
      (dotimes (i total-pts)
        (multiple-value-bind (dx dy on-curve-p) (decode-triplet flag-bs glyph-bs)
          (declare (fixnum dx dy))
          (setf x (s16 (+ x dx))
                y (s16 (+ y dy))
                xmin (min xmin x) xmax (max xmax x)
                ymin (min ymin y) ymax (max ymax y))
          (multiple-value-bind (x-flags new-x-len) (coordinate dx x-coords x-len #x02 #x10)
            (multiple-value-bind (y-flags new-y-len) (coordinate dy y-coords y-len #x04 #x20)
              (setf x-len new-x-len
                    y-len new-y-len
                    (aref flags i)
                    (logior (if on-curve-p #x01 0)
                            (if (and (zerop i) overlap-p) #x40 0)
                            x-flags y-flags)))))))
    (values x-len y-len xmin ymin xmax ymax)))

(defun out-flags (out flags count)
  "Write COUNT point FLAGS, a run of equal flags as one with a repeat count."
  (declare (glyf-out out) (type (simple-array (unsigned-byte 8) (*)) flags) (fixnum count))
  (let ((i 0))
    (declare (fixnum i))
    (loop while (< i count)
          do (let* ((f (aref flags i))
                    (repeats (loop for j from (1+ i) below (min count (+ i 256))
                                   while (= (aref flags j) f)
                                   count t)))
               (cond ((plusp repeats)
                      (out-u8 out (logior f #x08))
                      (out-u8 out repeats))
                     (t (out-u8 out f)))
               (incf i (1+ repeats))))))

(defun write-simple-glyph (s out gid n-contours has-bbox)
  "Write simple glyph GID; return its xMin."
  (declare (glyf-streams s) (glyf-out out) (type (unsigned-byte 16) gid)
           (type (integer 1 32767) n-contours))
  (let ((total-pts (read-contour-ends s gid n-contours))
        (start (out-pos out)))
    (multiple-value-bind (x-len y-len cxmin cymin cxmax cymax)
        (decode-simple-points s gid total-pts)
      (flet ((bbox (computed) (if has-bbox (bs-i16 (gs-bbox s) "glyf") computed)))
        (let* ((xmin (bbox cxmin)) (ymin (bbox cymin))
               (xmax (bbox cxmax)) (ymax (bbox cymax))
               (instr-len (bs-255u16 (gs-glyph s) "glyf"))
               (instr (bstream-sub (gs-instruction s) instr-len "glyf")))
          ;; At most this much: the flags may compress.
          (out-reserve out (round4 (+ 10 (* 2 n-contours) 2 instr-len total-pts x-len y-len)))
          (out-u16 out n-contours)
          (dolist (v (list xmin ymin xmax ymax)) (out-i16 out v))
          (dotimes (c n-contours) (out-u16 out (aref (gs-end-pts s) c)))
          (out-u16 out instr-len)
          (out-copy-stream out instr)
          (out-flags out (gs-flags s) total-pts)
          (out-copy out (gs-x-coords s) 0 x-len)
          (out-copy out (gs-y-coords s) 0 y-len)
          (out-pad out start)
          xmin)))))

(defun loca-bytes (offsets index-format)
  "The 'loca' table for glyph OFFSETS, short (INDEX-FORMAT 0) or long."
  (let* ((short-p (zerop index-format))
         (bytes (make-array (* (length offsets) (if short-p 2 4)) :element-type '(unsigned-byte 8))))
    (loop for offset across offsets
          for i from 0
          do (if short-p
                 (let ((half (ash offset -1)))
                   (when (> half #xFFFF)
                     (woff-fail "loca" "short loca offset overflow"))
                   (put-u16 bytes (* i 2) half))
                 (put-u32 bytes (* i 4) offset)))
    bytes))

(defun reconstruct-glyf-and-loca (unpacked glyf-entry loca-entry)
  "Reconstruct transformed 'glyf' and 'loca' tables from UNPACKED.
Returns an X-MINS vector of (signed-byte 16) for 'hmtx' reconstruction."
  (let* ((bs (make-bstream unpacked
                           (woff2-table-entry-src-offset glyf-entry)
                           (+ (woff2-table-entry-src-offset glyf-entry)
                              (woff2-table-entry-transform-length glyf-entry))))
         (reserved (bs-u16 bs "glyf"))
         (option-flags (bs-u16 bs "glyf"))
         (num-glyphs (bs-u16 bs "glyf"))
         (index-format (bs-u16 bs "glyf"))
         (n-contour-size (bs-u32 bs "glyf"))
         (n-points-size (bs-u32 bs "glyf"))
         (flag-size (bs-u32 bs "glyf"))
         (glyph-size (bs-u32 bs "glyf"))
         (composite-size (bs-u32 bs "glyf"))
         (bbox-size (bs-u32 bs "glyf"))
         (instruction-size (bs-u32 bs "glyf")))
    (unless (zerop reserved)
      (woff-fail "glyf" "non-zero reserved field in transformed glyf header"))
    (unless (or (= index-format 0) (= index-format 1))
      (woff-fail "glyf" "invalid indexFormat ~D in transformed glyf" index-format))
    (unless (= n-contour-size (* 2 num-glyphs))
      (woff-fail "glyf" "nContourStreamSize mismatch"))
    (let* ((n-contour (bstream-sub bs n-contour-size "glyf"))
           (n-points (bstream-sub bs n-points-size "glyf"))
           (flag (bstream-sub bs flag-size "glyf"))
           (glyph (bstream-sub bs glyph-size "glyf"))
           (composite (bstream-sub bs composite-size "glyf"))
           ;; The bbox stream starts with a bitmap of the glyphs that have one.
           (bbox (bstream-sub bs bbox-size "glyf"))
           (bbox-bitmap (bstream-sub bbox (* 4 (ash (+ num-glyphs 31) -5)) "glyf"))
           (instruction (bstream-sub bs instruction-size "glyf"))
           (overlap (when (logbitp 0 option-flags)
                      (bstream-sub bs (ash (+ num-glyphs 7) -3) "glyf")))
           (s (make-glyf-streams :n-contour n-contour :n-points n-points :flag flag
                                 :glyph glyph :composite composite :bbox-bitmap bbox-bitmap
                                 :bbox bbox :instruction instruction :overlap overlap))
           (out (make-glyf-out :bytes (make-array (max 4096 (woff2-table-entry-orig-length glyf-entry))
                                                  :element-type '(unsigned-byte 8)
                                                  :initial-element 0)))
           (loca-offsets (make-array (1+ num-glyphs) :element-type '(unsigned-byte 32)))
           (x-mins (make-array num-glyphs :element-type '(signed-byte 16) :initial-element 0)))
      (dotimes (gid num-glyphs)
        (setf (aref loca-offsets gid) (out-pos out))
        (let ((n-contours (bs-i16 n-contour "glyf"))
              (has-bbox (bitmap-bit-p bbox-bitmap gid)))
          (cond
            ((zerop n-contours)
             (when has-bbox
               (woff-fail "glyf" "empty glyph ~D has explicit bbox bit set" gid)))
            ((= n-contours -1)
             (setf (aref x-mins gid) (write-composite-glyph s out gid has-bbox)))
            ((minusp n-contours)
             (woff-fail "glyf" "invalid nContours ~D for glyph ~D" n-contours gid))
            (t
             (setf (aref x-mins gid) (write-simple-glyph s out gid n-contours has-bbox))))))
      (setf (aref loca-offsets num-glyphs) (out-pos out))
      (let ((glyf-bytes (subseq (out-bytes out) 0 (out-pos out)))
            (loca-bytes (loca-bytes loca-offsets index-format)))
        (setf (woff2-table-entry-bytes glyf-entry) glyf-bytes
              (woff2-table-entry-dst-length glyf-entry) (length glyf-bytes)
              (woff2-table-entry-bytes loca-entry) loca-bytes
              (woff2-table-entry-dst-length loca-entry) (length loca-bytes))
        x-mins))))

;;; ============================================================================
;;; WOFF2 Transformed 'hmtx' Reconstruction
;;; ============================================================================

(defun extract-untransformed-xmins (glyf-bytes loca-bytes index-format num-glyphs)
  "Extract xMin for each glyph from an untransformed 'glyf' and 'loca' pair."
  (let ((x-mins (make-array num-glyphs :element-type '(signed-byte 16) :initial-element 0))
        (glyf-fd (make-font-data glyf-bytes))
        (loca-fd (make-font-data loca-bytes)))
    (dotimes (gid num-glyphs x-mins)
      (let ((off (if (zerop index-format)
                     (* 2 (font-data-read-u16 loca-fd (* gid 2)))
                     (font-data-read-u32 loca-fd (* gid 4))))
            (next (if (zerop index-format)
                      (* 2 (font-data-read-u16 loca-fd (* (1+ gid) 2)))
                      (font-data-read-u32 loca-fd (* (1+ gid) 4)))))
        (when (> next off)
          (setf (aref x-mins gid) (font-data-read-i16 glyf-fd (+ off 2))))))))

(defun reconstruct-hmtx (unpacked hmtx-entry num-glyphs num-h-metrics x-mins)
  (declare (type (unsigned-byte 16) num-glyphs num-h-metrics)
           (type (or null (simple-array (signed-byte 16) (*))) x-mins))
  (when (or (zerop num-h-metrics) (> num-h-metrics num-glyphs))
    (woff-fail "hmtx" "invalid numberOfHMetrics ~D for ~D glyphs" num-h-metrics num-glyphs))
  (let* ((bs (make-bstream unpacked
                           (woff2-table-entry-src-offset hmtx-entry)
                           (+ (woff2-table-entry-src-offset hmtx-entry)
                              (woff2-table-entry-transform-length hmtx-entry))))
         (flags (bs-u8 bs "hmtx"))
         (has-prop-lsbs (not (logbitp 0 flags)))
         (has-mono-lsbs (not (logbitp 1 flags))))
    (unless (zerop (logand flags #xFC))
      (woff-fail "hmtx" "reserved bits set in transformed hmtx flags #x~2,'0X" flags))
    (when (and (or (not has-prop-lsbs) (not has-mono-lsbs)) (null x-mins))
      (woff-fail "hmtx" "transformed hmtx omits LSBs but no glyf xMins are available"))
    (let ((advances (make-array num-h-metrics :element-type '(unsigned-byte 16)))
          (out (make-array (+ (* 4 num-h-metrics) (* 2 (- num-glyphs num-h-metrics)))
                           :element-type '(unsigned-byte 8)))
          (p 0))
      (declare (fixnum p))
      (dotimes (i num-h-metrics)
        (setf (aref advances i) (bs-u16 bs "hmtx")))
      (dotimes (i num-h-metrics)
        (let ((lsb (if has-prop-lsbs (bs-i16 bs "hmtx") (aref x-mins i))))
          (setf p (put-u16 out p (aref advances i))
                p (put-i16 out p lsb))))
      (loop for i from num-h-metrics below num-glyphs
            do (let ((lsb (if has-mono-lsbs (bs-i16 bs "hmtx") (aref x-mins i))))
                 (setf p (put-i16 out p lsb))))
      (setf (woff2-table-entry-bytes hmtx-entry) out
            (woff2-table-entry-dst-length hmtx-entry) (length out)))))

;;; ============================================================================
;;; Resolving Per-Font Tables and Assembling SFNT / TTC Buffers
;;; ============================================================================

(defun prepare-font-tables (unpacked table-list)
  "Materialize table byte arrays for TABLE-LIST (a list of WOFF2-TABLE-ENTRY
belonging to one font), reconstructing transformed 'glyf', 'loca', and 'hmtx'."
  (let ((glyf-entry (find +tag-glyf+ table-list :key #'woff2-table-entry-tag))
        (loca-entry (find +tag-loca+ table-list :key #'woff2-table-entry-tag))
        (hmtx-entry (find +tag-hmtx+ table-list :key #'woff2-table-entry-tag))
        (head-entry (find +tag-head+ table-list :key #'woff2-table-entry-tag))
        (hhea-entry (find +tag-hhea+ table-list :key #'woff2-table-entry-tag))
        (maxp-entry (find +tag-maxp+ table-list :key #'woff2-table-entry-tag))
        (x-mins nil))
    ;; Copy untransformed tables first (if not already materialized by a shared font)
    (dolist (e table-list)
      (when (and (not (woff2-table-entry-transformed-p e))
                 (null (woff2-table-entry-bytes e)))
        (let* ((start (woff2-table-entry-src-offset e))
               (len (woff2-table-entry-transform-length e))
               (slice (subseq unpacked start (+ start len))))
          (when (= (woff2-table-entry-tag e) +tag-head+)
            (when (< len 12)
              (woff-fail "head" "head table too short (~D bytes)" len))
            ;; Zero checkSumAdjustment before computing checksums
            (put-u32 slice 8 0))
          (setf (woff2-table-entry-bytes e) slice
                (woff2-table-entry-dst-length e) len))))
    ;; Reconstruct transformed glyf + loca
    (when (or (and glyf-entry (woff2-table-entry-transformed-p glyf-entry))
              (and loca-entry (woff2-table-entry-transformed-p loca-entry)))
      (unless (and glyf-entry loca-entry
                   (woff2-table-entry-transformed-p glyf-entry)
                   (woff2-table-entry-transformed-p loca-entry))
        (woff-fail "glyf" "glyf and loca must both be transformed or both untransformed"))
      (when (null (woff2-table-entry-bytes glyf-entry))
        (setf x-mins (reconstruct-glyf-and-loca unpacked glyf-entry loca-entry))))
    ;; Reconstruct transformed hmtx
    (when (and hmtx-entry (woff2-table-entry-transformed-p hmtx-entry)
               (null (woff2-table-entry-bytes hmtx-entry)))
      (unless (and hhea-entry maxp-entry)
        (woff-fail "hmtx" "transformed hmtx requires hhea and maxp tables"))
      (let* ((hhea-fd (make-font-data (woff2-table-entry-bytes hhea-entry)))
             (maxp-fd (make-font-data (woff2-table-entry-bytes maxp-entry)))
             (num-h-metrics (font-data-read-u16 hhea-fd 34))
             (num-glyphs (font-data-read-u16 maxp-fd 4)))
        (when (and (null x-mins) glyf-entry loca-entry head-entry)
          (let* ((head-fd (make-font-data (woff2-table-entry-bytes head-entry)))
                 (index-format (font-data-read-i16 head-fd 50)))
            (setf x-mins (extract-untransformed-xmins
                          (woff2-table-entry-bytes glyf-entry)
                          (woff2-table-entry-bytes loca-entry)
                          index-format
                          num-glyphs))))
        (reconstruct-hmtx unpacked hmtx-entry num-glyphs num-h-metrics x-mins)))))

(defun assemble-single-sfnt (flavor entries)
  "Assemble a single SFNT byte vector from ENTRIES (vector of WOFF2-TABLE-ENTRY)."
  (let* ((sorted (sort (copy-seq entries) #'< :key #'woff2-table-entry-tag))
         (num-tables (length sorted))
         (dir-size (+ 12 (* 16 num-tables)))
         (data-offset dir-size)
         (head-entry nil))
    (loop for e across sorted
          do (setf (woff2-table-entry-dst-offset e) data-offset)
             (incf data-offset (round4 (woff2-table-entry-dst-length e)))
             (when (= (woff2-table-entry-tag e) +tag-head+)
               (setf head-entry e)))
    (let ((out (make-array data-offset :element-type '(unsigned-byte 8) :initial-element 0))
          (p 0)
          (total-checksum 0))
      (declare (type (unsigned-byte 32) total-checksum))
      (multiple-value-bind (search-range entry-selector range-shift)
          (sfnt-search-params num-tables)
        (setf p (put-u32 out p flavor)
              p (put-u16 out p num-tables)
              p (put-u16 out p search-range)
              p (put-u16 out p entry-selector)
              p (put-u16 out p range-shift)))
      (loop for e across sorted
            do (let* ((bytes (woff2-table-entry-bytes e))
                      (dst-off (woff2-table-entry-dst-offset e))
                      (dst-len (woff2-table-entry-dst-length e))
                      (csum (compute-table-checksum bytes 0 dst-len)))
                 (setf (woff2-table-entry-checksum e) csum
                       total-checksum (ldb (byte 32 0) (+ total-checksum csum))
                       p (put-u32 out p (woff2-table-entry-tag e))
                       p (put-u32 out p csum)
                       p (put-u32 out p dst-off)
                       p (put-u32 out p dst-len))
                 (replace out bytes :start1 dst-off :end2 dst-len)))
      (let ((header-csum (compute-table-checksum out 0 dir-size)))
        (setf total-checksum (ldb (byte 32 0) (+ total-checksum header-csum)))
        (when (and head-entry (>= (woff2-table-entry-dst-length head-entry) 12))
          (put-u32 out (+ (woff2-table-entry-dst-offset head-entry) 8)
                   (ldb (byte 32 0) (- #xB1B0AFBA total-checksum)))))
      out)))

(defun assemble-ttc-collection (ttc-version fonts entries)
  "Assemble a TrueType Collection (TTC) byte vector from FONTS and ENTRIES."
  (let* ((num-fonts (length fonts))
         ;; Force version 1.0 header layout (or 2.0 with null DSIG fields)
         (ttc-header-size (+ 12 (* 4 num-fonts) (if (= ttc-version #x00020000) 12 0)))
         (font-dir-offsets (make-array num-fonts :element-type '(unsigned-byte 32)))
         (offset ttc-header-size))
    (dotimes (f num-fonts)
      (setf (aref font-dir-offsets f) offset)
      (incf offset (+ 12 (* 16 (length (woff2-collection-font-table-indices (aref fonts f)))))))
    (loop for e across entries
          do (let ((len (woff2-table-entry-dst-length e)))
               (setf (woff2-table-entry-dst-offset e) offset
                     (woff2-table-entry-checksum e)
                     (compute-table-checksum (woff2-table-entry-bytes e) 0 len))
               (incf offset (round4 len))))
    (let ((out (make-array offset :element-type '(unsigned-byte 8) :initial-element 0))
          (p 0))
      (setf p (put-u32 out p +ttcf-signature+)
            p (put-u32 out p ttc-version)
            p (put-u32 out p num-fonts))
      (dotimes (f num-fonts)
        (setf p (put-u32 out p (aref font-dir-offsets f))))
      (when (= ttc-version #x00020000)
        (setf p (put-u32 out p 0)
              p (put-u32 out p 0)
              p (put-u32 out p 0)))
      ;; Write each font's Offset Table and Table Records (sorted by tag)
      (dotimes (f num-fonts)
        (let* ((cfont (aref fonts f))
               (indices (woff2-collection-font-table-indices cfont))
               (f-entries (sort (map 'vector (lambda (idx) (aref entries idx)) indices)
                                #'< :key #'woff2-table-entry-tag))
               (ntables (length f-entries))
               (fp (aref font-dir-offsets f)))
          (multiple-value-bind (search-range entry-selector range-shift)
              (sfnt-search-params ntables)
            (setf fp (put-u32 out fp (woff2-collection-font-flavor cfont))
                  fp (put-u16 out fp ntables)
                  fp (put-u16 out fp search-range)
                  fp (put-u16 out fp entry-selector)
                  fp (put-u16 out fp range-shift)))
          (loop for e across f-entries
                do (setf fp (put-u32 out fp (woff2-table-entry-tag e))
                         fp (put-u32 out fp (woff2-table-entry-checksum e))
                         fp (put-u32 out fp (woff2-table-entry-dst-offset e))
                         fp (put-u32 out fp (woff2-table-entry-dst-length e))))))
      ;; Copy shared table bodies
      (loop for e across entries
            do (replace out (woff2-table-entry-bytes e)
                        :start1 (woff2-table-entry-dst-offset e)
                        :end2 (woff2-table-entry-dst-length e)))
      out)))

;;; ============================================================================
;;; Public WOFF2 and WOFF 1.0 Decoders
;;; ============================================================================

(defun decode-woff2 (source)
  "Decode a WOFF2 font or collection from SOURCE (a FONT-DATA, octet vector,
or pathname) and return a FONT-DATA over the reconstructed SFNT/TTC octets."
  (let* ((fd (if (font-data-p source) source (ensure-font-data source)))
         (bs (bstream-from-font-data fd)))
    (when (< (font-data-length fd) 48)
      (woff-fail "WOFF2" "file shorter than 48-byte WOFF2 header"))
    (let ((signature (bs-u32 bs "WOFF2"))
          (flavor (bs-u32 bs "WOFF2"))
          (total-length (bs-u32 bs "WOFF2"))
          (num-tables (bs-u16 bs "WOFF2"))
          (reserved (bs-u16 bs "WOFF2"))
          (_total-sfnt-size (bs-u32 bs "WOFF2"))
          (total-comp-size (bs-u32 bs "WOFF2")))
      (declare (ignore _total-sfnt-size))
      (unless (= signature +woff2-signature+)
        (woff-fail "WOFF2" "bad WOFF2 signature #x~8,'0X" signature))
      (unless (zerop reserved)
        (woff-fail "WOFF2" "non-zero reserved header field ~D" reserved))
      (when (zerop num-tables)
        (woff-fail "WOFF2" "WOFF2 file has zero tables"))
      (when (> total-length (font-data-length fd))
        (woff-fail "WOFF2" "header length ~D exceeds buffer length ~D"
                   total-length (font-data-length fd)))
      ;; Skip majorVersion(2), minorVersion(2), metaOffset(4), metaLength(4),
      ;; metaOrigLength(4), privOffset(4), privLength(4) = 24 bytes
      (bstream-sub bs 24 "WOFF2")
      (multiple-value-bind (entries uncompressed-size)
          (read-woff2-table-directory bs num-tables)
        (multiple-value-bind (ttc-version collection-fonts)
            (if (= flavor +ttcf-signature+)
                (read-woff2-collection-directory bs num-tables)
                (values nil nil))
          (let* ((comp-sub (bstream-sub bs total-comp-size "WOFF2"))
                 (unpacked (handler-case
                               (decompress:decompress (bstream-bytes comp-sub)
                                                       :size-hint uncompressed-size
                                                       :start (bstream-pos comp-sub)
                                                       :end (bstream-end comp-sub))
                             (decompress:decompression-error (c)
                               (woff-fail "WOFF2" "Brotli decompression failed: ~A" c)))))
            (unless (= (length unpacked) uncompressed-size)
              (woff-fail "WOFF2" "decompressed size ~D does not match expected ~D"
                         (length unpacked) uncompressed-size))
            (if collection-fonts
                (progn
                  (loop for cfont across collection-fonts
                        do (prepare-font-tables
                            unpacked
                            (loop for idx across (woff2-collection-font-table-indices cfont)
                                  collect (aref entries idx))))
                  (make-font-data (assemble-ttc-collection ttc-version collection-fonts entries)))
                (progn
                  (prepare-font-tables unpacked (coerce entries 'list))
                  (make-font-data (assemble-single-sfnt flavor entries))))))))))
