;;;; woff1.lisp — WOFF 1.0 decoding into SFNT font-data
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:runes/otf)

(defconstant +woff1-signature+ #x774F4646) ; 'wOFF'
(defconstant +woff1-tag-head+  #x68656164) ; 'head'

(defstruct woff1-table-entry
  (tag           0 :type (unsigned-byte 32))
  (src-offset    0 :type (unsigned-byte 32))
  (comp-length   0 :type (unsigned-byte 32))
  (orig-length   0 :type (unsigned-byte 32))
  (orig-checksum 0 :type (unsigned-byte 32))
  (dst-offset    0 :type (unsigned-byte 32)))

(declaim (inline woff1-put-u16 woff1-put-u32 woff1-round4))

(defun woff1-put-u16 (buf pos v)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum pos)
           (type (unsigned-byte 16) v))
  (setf (aref buf pos)       (ldb (byte 8 8) v)
        (aref buf (+ pos 1)) (ldb (byte 8 0) v))
  (+ pos 2))

(defun woff1-put-u32 (buf pos v)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf)
           (type fixnum pos)
           (type (unsigned-byte 32) v))
  (setf (aref buf pos)       (ldb (byte 8 24) v)
        (aref buf (+ pos 1)) (ldb (byte 8 16) v)
        (aref buf (+ pos 2)) (ldb (byte 8 8) v)
        (aref buf (+ pos 3)) (ldb (byte 8 0) v))
  (+ pos 4))

(defun woff1-round4 (n)
  (declare (type (unsigned-byte 32) n))
  (logand (+ n 3) -4))

(defun woff1-tag-string (tag)
  (declare (type (unsigned-byte 32) tag))
  (let ((s (make-string 4)))
    (setf (schar s 0) (code-char (ldb (byte 8 24) tag))
          (schar s 1) (code-char (ldb (byte 8 16) tag))
          (schar s 2) (code-char (ldb (byte 8 8) tag))
          (schar s 3) (code-char (ldb (byte 8 0) tag)))
    s))

(defun woff1-fail (table fmt &rest args)
  (error 'font-format-error
         :table table
         :message (apply #'format nil fmt args)))

(defun woff1-checksum (bytes start length)
  "Compute the 32-bit OpenType table checksum over BYTES[START .. START+LENGTH)."
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

(defun woff1-search-params (num-tables)
  (declare (type (unsigned-byte 16) num-tables))
  (let* ((max-pow2 (ash 1 (1- (integer-length num-tables))))
         (search-range (* max-pow2 16))
         (entry-selector (1- (integer-length max-pow2)))
         (range-shift (- (* num-tables 16) search-range)))
    (values search-range entry-selector range-shift)))

(defun read-woff1-directory (fd num-tables dir-end)
  "The table directory of the WOFF font FD, each entry with its offset in
the SFNT, in directory order; and the SFNT's size."
  (declare (type font-data fd) (type (unsigned-byte 16) num-tables) (fixnum dir-end))
  (let ((dst-offset (+ 12 (* 16 num-tables))))
    (declare (type (unsigned-byte 64) dst-offset))
    (values
     (loop for i below num-tables
           for rec-pos = (+ 44 (* i 20))
           collect (let ((tag       (font-data-read-u32 fd rec-pos))
                         (src-off   (font-data-read-u32 fd (+ rec-pos 4)))
                         (comp-len  (font-data-read-u32 fd (+ rec-pos 8)))
                         (orig-len  (font-data-read-u32 fd (+ rec-pos 12)))
                         (orig-csum (font-data-read-u32 fd (+ rec-pos 16))))
                     (when (or (> comp-len orig-len)
                               (< src-off dir-end)
                               (not (font-data-valid-range-p fd src-off comp-len)))
                       (woff1-fail (woff1-tag-string tag)
                                   "invalid table bounds (offset ~D, compLength ~D, origLength ~D)"
                                   src-off comp-len orig-len))
                     (prog1 (make-woff1-table-entry :tag tag :src-offset src-off
                                                    :comp-length comp-len :orig-length orig-len
                                                    :orig-checksum orig-csum :dst-offset dst-offset)
                       (incf dst-offset (woff1-round4 orig-len)))))
     dst-offset)))

(defun woff1-copy-table (fd out e)
  "Copy table entry E of FD into OUT at its SFNT offset, decompressing it
when it is compressed."
  (let* ((tag (woff1-table-entry-tag e))
         (comp-len (woff1-table-entry-comp-length e))
         (orig-len (woff1-table-entry-orig-length e))
         (dst-off (woff1-table-entry-dst-offset e))
         (raw-in (font-data-bytes fd))
         (abs-src (+ (font-data-offset fd) (woff1-table-entry-src-offset e))))
    (if (= comp-len orig-len)
        (replace out raw-in :start1 dst-off :start2 abs-src :end2 (+ abs-src orig-len))
        (let ((decompressed
                (handler-case
                    (decompress:zlib-decompress raw-in :size-hint orig-len
                                                       :start abs-src :end (+ abs-src comp-len))
                  (decompress:decompression-error (c)
                    (woff1-fail (woff1-tag-string tag) "zlib decompression failed: ~A" c)))))
          (unless (= (length decompressed) orig-len)
            (woff1-fail (woff1-tag-string tag)
                        "decompressed size ~D does not match origLength ~D"
                        (length decompressed) orig-len))
          (replace out decompressed :start1 dst-off :end2 orig-len)))))

(defun woff1-verify-checksum (out e)
  "Check the checksum of table entry E, copied into OUT.  A 'head' table's
checkSumAdjustment counts as zero."
  (let* ((tag (woff1-table-entry-tag e))
         (dst-off (woff1-table-entry-dst-offset e))
         (orig-len (woff1-table-entry-orig-length e))
         (head-p (and (= tag +woff1-tag-head+) (>= orig-len 12)))
         (saved (when head-p
                  (prog1 (logior (ash (aref out (+ dst-off 8)) 24) (ash (aref out (+ dst-off 9)) 16)
                                 (ash (aref out (+ dst-off 10)) 8) (aref out (+ dst-off 11)))
                    (woff1-put-u32 out (+ dst-off 8) 0))))
         (actual (woff1-checksum out dst-off orig-len)))
    (when head-p
      (woff1-put-u32 out (+ dst-off 8) saved))
    (unless (= actual (woff1-table-entry-orig-checksum e))
      (woff1-fail (woff1-tag-string tag) "checksum mismatch: expected #x~8,'0X, got #x~8,'0X"
                  (woff1-table-entry-orig-checksum e) actual))))

(defun woff1-set-checksum-adjustment (out head-entry)
  "Recompute head.checkSumAdjustment over the reconstructed SFNT OUT."
  (let ((head-off (woff1-table-entry-dst-offset head-entry))
        (head-len (woff1-table-entry-orig-length head-entry)))
    (when (< head-len 12)
      (woff1-fail "head" "head table too short (~D bytes)" head-len))
    (woff1-put-u32 out (+ head-off 8) 0)
    (woff1-put-u32 out (+ head-off 8)
                   (ldb (byte 32 0) (- #xB1B0AFBA (woff1-checksum out 0 (length out)))))))

(defun decode-woff1 (source &key (verify-checksums nil))
  "Decode a WOFF 1.0 font from SOURCE (a FONT-DATA, byte vector, or pathname)
and return a FONT-DATA over the reconstructed SFNT byte vector.
When VERIFY-CHECKSUMS is true, each table's checksum is checked against the
directory's origChecksum."
  (let* ((fd (ensure-font-data source))
         (buf-len (font-data-length fd)))
    (when (< buf-len 44)
      (woff1-fail "WOFF" "buffer length ~D is shorter than 44-byte WOFF 1.0 header" buf-len))
    (let* ((signature       (font-data-read-u32 fd 0))
           (flavor          (font-data-read-u32 fd 4))
           (total-length    (font-data-read-u32 fd 8))
           (num-tables      (font-data-read-u16 fd 12))
           (reserved        (font-data-read-u16 fd 14))
           (total-sfnt-size (font-data-read-u32 fd 16))
           (dir-end (+ 44 (* 20 num-tables))))
      (unless (= signature +woff1-signature+)
        (woff1-fail "WOFF" "invalid WOFF 1.0 signature #x~8,'0X" signature))
      (unless (zerop reserved)
        (woff1-fail "WOFF" "non-zero reserved field ~D in WOFF 1.0 header" reserved))
      (when (zerop num-tables)
        (woff1-fail "WOFF" "WOFF 1.0 font has zero tables"))
      (when (> total-length buf-len)
        (woff1-fail "WOFF" "WOFF length ~D exceeds buffer length ~D" total-length buf-len))
      (when (> dir-end total-length)
        (woff1-fail "WOFF" "table directory overruns WOFF length"))
      (multiple-value-bind (entries sfnt-size) (read-woff1-directory fd num-tables dir-end)
        (unless (= sfnt-size total-sfnt-size)
          (woff1-fail "WOFF" "computed SFNT size ~D does not match header totalSfntSize ~D"
                      sfnt-size total-sfnt-size))
        (let ((out (make-array sfnt-size :element-type '(unsigned-byte 8) :initial-element 0))
              (p 0))
          ;; The SFNT header, then a table record and the table for each
          ;; entry, in tag order.
          (multiple-value-bind (search-range entry-selector range-shift)
              (woff1-search-params num-tables)
            (setf p (woff1-put-u32 out p flavor)
                  p (woff1-put-u16 out p num-tables)
                  p (woff1-put-u16 out p search-range)
                  p (woff1-put-u16 out p entry-selector)
                  p (woff1-put-u16 out p range-shift)))
          (dolist (e (sort (copy-list entries) #'< :key #'woff1-table-entry-tag))
            (setf p (woff1-put-u32 out p (woff1-table-entry-tag e))
                  p (woff1-put-u32 out p (woff1-table-entry-orig-checksum e))
                  p (woff1-put-u32 out p (woff1-table-entry-dst-offset e))
                  p (woff1-put-u32 out p (woff1-table-entry-orig-length e)))
            (woff1-copy-table fd out e)
            (when verify-checksums
              (woff1-verify-checksum out e)))
          (let ((head (find +woff1-tag-head+ entries :key #'woff1-table-entry-tag)))
            (when head
              (woff1-set-checksum-adjustment out head)))
          (make-font-data out))))))

;;; ============================================================================
;;; Optional WOFF 1.0 Extended Metadata & Private Data Accessors
;;; ============================================================================

(defun decode-woff1-metadata (source)
  "Decompress and return the WOFF 1.0 Extended Metadata XML block as an octet
vector, or NIL if SOURCE has no metadata block."
  (let ((fd (ensure-font-data source)))
    (when (< (font-data-length fd) 44)
      (woff1-fail "WOFF" "buffer shorter than 44-byte WOFF 1.0 header"))
    (unless (= (font-data-read-u32 fd 0) +woff1-signature+)
      (woff1-fail "WOFF" "not a WOFF 1.0 container"))
    (let ((meta-offset   (font-data-read-u32 fd 24))
          (meta-length   (font-data-read-u32 fd 28))
          (meta-orig-len (font-data-read-u32 fd 32)))
      (when (and (plusp meta-offset) (plusp meta-length))
        (unless (font-data-valid-range-p fd meta-offset meta-length)
          (woff1-fail "WOFF" "metadata block out of bounds"))
        (let* ((start (+ (font-data-offset fd) meta-offset))
               (xml-bytes (handler-case
                              (decompress:zlib-decompress (font-data-bytes fd)
                                                           :size-hint meta-orig-len
                                                           :start start
                                                           :end (+ start meta-length))
                            (decompress:decompression-error (c)
                              (woff1-fail "WOFF" "metadata zlib decompression failed: ~A" c)))))
          (unless (= (length xml-bytes) meta-orig-len)
            (woff1-fail "WOFF" "metadata decompressed size ~D does not match metaOrigLength ~D"
                        (length xml-bytes) meta-orig-len))
          xml-bytes)))))

(defun woff1-private-data (source)
  "Return a FONT-DATA slice over the WOFF 1.0 Private Data block, or NIL if none."
  (let ((fd (ensure-font-data source)))
    (when (< (font-data-length fd) 44)
      (woff1-fail "WOFF" "buffer shorter than 44-byte WOFF 1.0 header"))
    (unless (= (font-data-read-u32 fd 0) +woff1-signature+)
      (woff1-fail "WOFF" "not a WOFF 1.0 container"))
    (let ((priv-offset (font-data-read-u32 fd 36))
          (priv-length (font-data-read-u32 fd 40)))
      (when (and (plusp priv-offset) (plusp priv-length))
        (font-data-slice fd priv-offset priv-length)))))
