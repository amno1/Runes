;;;; cff.lisp — CFF and CFF2 fonts: INDEXes, subfonts and glyph evaluation
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; A port of Fontations' read-fonts src/ps/cff/font.rs (CffFontRef and
;;; Subfont, without the hinting parameters and the name metadata), with
;;; index.rs, fd_select.rs and the TransformSink of src/ps/charstring.rs.
;;;
;;; Positions are relative to the start of the CFF or CFF2 table, which is
;;; held as a RUNES/OTF:FONT-DATA window.

(in-package #:runes/otf/charstring)

(declaim (inline cff-u8 cff-uint))
(defun cff-u8 (fd pos) (runes/otf:font-data-read-u8 fd pos))
(defun cff-uint (fd pos size)
  "The SIZE byte big-endian unsigned integer at POS, SIZE 1 to 4."
  (ecase size
    (1 (runes/otf:font-data-read-u8 fd pos))
    (2 (runes/otf:font-data-read-u16 fd pos))
    (3 (runes/otf:font-data-read-u24 fd pos))
    (4 (runes/otf:font-data-read-u32 fd pos))))

;;; INDEX (index.rs)

(defstruct (cff-index (:constructor %make-cff-index))
  (data nil :type runes/otf:font-data)
  (count 0 :type (unsigned-byte 32))
  (off-size 0 :type (integer 0 4))
  (offsets-pos 0 :type fixnum)
  ;; Position of the byte before the first object: offsets are relative to it.
  (base 0))

(defun read-cff-index (fd pos cff2-p)
  "The INDEX at POS. CFF2 INDEXes have a 32-bit count."
  (let* ((count-size (if cff2-p 4 2))
         (count (cff-uint fd pos count-size)))
    (if (zerop count)
        (%make-cff-index :data fd :count 0 :offsets-pos (+ pos count-size) :base (+ pos count-size))
        (let ((off-size (cff-u8 fd (+ pos count-size))))
          (unless (<= 1 off-size 4)
            (fail 'charstring-malformed "invalid INDEX offset size"))
          (let ((offsets-pos (+ pos count-size 1)))
            (%make-cff-index :data fd :count count :off-size off-size :offsets-pos offsets-pos
                             :base (+ offsets-pos (* (1+ count) off-size) -1)))))))

(defun cff-index-offset (index i)
  (declare (cff-index index) (type (unsigned-byte 32) i))
  (let ((offset (cff-uint (cff-index-data index)
                          (+ (cff-index-offsets-pos index) (* i (cff-index-off-size index)))
                          (cff-index-off-size index))))
    ;; The first offset is always 1; a zero is malformed.
    (when (zerop offset)
      (fail 'charstring-malformed "zero INDEX offset"))
    offset))

(defun cff-index-ref (index i)
  "Object I of INDEX as a FONT-DATA window."
  (declare (cff-index index) (fixnum i))
  (unless (< -1 i (cff-index-count index))
    (fail 'charstring-malformed (format nil "INDEX entry ~D out of range" i)))
  (let* ((start (cff-index-offset index i))
         (end (cff-index-offset index (1+ i)))
         (fd (cff-index-data index)))
    (when (or (< end start) (> (+ (cff-index-base index) end) (runes/otf:font-data-length fd)))
      (fail 'charstring-malformed "INDEX entry out of bounds"))
    (runes/otf:font-data-slice fd (+ (cff-index-base index) start) (- end start))))

(defun cff-index-end (index)
  "Position just past INDEX."
  (if (zerop (cff-index-count index))
      (cff-index-base index)
      (+ (cff-index-base index) (cff-index-offset index (cff-index-count index)))))

;;; FDSelect (fd_select.rs)

(defun fd-select-index (fd pos gid)
  "Font DICT index for GID from the FDSelect at POS, or NIL if not covered."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) pos gid))
  (let ((format (cff-u8 fd pos)))
    (case format
      (0 (handler-case (cff-u8 fd (+ pos 1 gid))
           (runes/otf:font-out-of-bounds-error () nil)))
      ((3 4)
       (let* ((w (if (= format 3) 2 4))
              (fd-size (if (= format 3) 1 2))
              (range-size (+ w fd-size))
              (n (cff-uint fd (+ pos 1) w))
              (ranges (+ pos 1 w))
              (sentinel (cff-uint fd (+ ranges (* n range-size)) w)))
         (flet ((first-gid (i) (cff-uint fd (+ ranges (* i range-size)) w)))
           ;; The last range whose first glyph is not above GID.
           (let ((lo 0) (hi n))
             (declare (type (unsigned-byte 32) lo hi))
             (loop while (< lo hi)
                   do (let ((mid (floor (+ lo hi) 2)))
                        (if (<= (first-gid mid) gid) (setf lo (1+ mid)) (setf hi mid))))
             (let ((ix (1- lo)))
               (when (and (>= ix 0) (<= (first-gid ix) gid) (< gid sentinel))
                 (cff-uint fd (+ ranges (* ix range-size) w) fd-size)))))))
      (t nil))))

;;; Fonts and subfonts (font.rs)

(defstruct (cff-font (:constructor %make-cff-font))
  ;; FONT-DATA window over the CFF or CFF2 table.
  (data nil)
  (cff2-p nil)
  (upem 1000)
  (global-subrs nil)
  (charstrings nil)
  (charset-offset nil)
  (encoding-offset nil)
  ;; Normalized top DICT FontMatrix, or NIL.
  (matrix nil)
  (var-store nil)
  ;; :SID, :CID or :CFF2.
  (kind :sid)
  ;; (start . end) of the Private DICT of a :SID font.
  (private-range nil)
  (fd-array nil)
  (fd-select nil))

(defstruct cff-subfont
  ;; Local subroutine INDEX, or NIL.
  (subrs nil)
  (default-width nil)
  (nominal-width 0)
  ;; Font DICT FontMatrix (not normalized), or NIL.
  (matrix nil)
  (vs-index 0)
  ;; Made by CFF-DRAW-GLYPH on first use, for all the subfont's glyphs: the
  ;; charstring context, and the transform as (matrix . scale).
  (context nil)
  (transform nil)
  ;; Hinting parameters from the Private DICT (see hint.lisp).
  (hint-params nil))

(defun open-cff-font (table-data &key upem)
  "Read the CFF or CFF2 table in the FONT-DATA window TABLE-DATA. UPEM is
the font's unitsPerEm from 'head'; without it, the FontMatrix decides."
  (let* ((fd table-data)
         (major (cff-u8 fd 0)))
    (multiple-value-bind (cff2-p top-dict global-subrs)
        (ecase major
          (1
           (let* ((names (read-cff-index fd (cff-u8 fd 2) nil))
                  (tops (read-cff-index fd (cff-index-end names) nil))
                  (strings (read-cff-index fd (cff-index-end tops) nil)))
             (values nil (cff-index-ref tops 0)
                     (read-cff-index fd (cff-index-end strings) nil))))
          (2
           (let ((header-size (cff-u8 fd 2))
                 (top-length (cff-uint fd 3 2)))
             (values t (runes/otf:font-data-slice fd header-size top-length)
                     (read-cff-index fd (+ header-size top-length) t)))))
      (let ((font (%make-cff-font :data fd :cff2-p cff2-p :global-subrs global-subrs
                                  ;; CFF has a default charset and encoding 0,
                                  ;; the standard ones; CFF2 has none.
                                  :charset-offset (if cff2-p nil 0)
                                  :encoding-offset (if cff2-p nil 0)))
            (has-ros nil))
        ;; Unreadable entries are skipped, as in Fontations.
        (loop for (op . value) in (dict-entries top-dict)
              do (case op
                   (:ros (setf has-ros t))
                   (:charstrings (setf (cff-font-charstrings font) (read-cff-index fd value cff2-p)))
                   (:charset (setf (cff-font-charset-offset font) value))
                   (:encoding (setf (cff-font-encoding-offset font) value))
                   (:fd-array (setf (cff-font-fd-array font) (read-cff-index fd value cff2-p)))
                   (:fd-select (setf (cff-font-fd-select font) value))
                   (:private
                    (when (> (cdr value) (runes/otf:font-data-length fd))
                      (fail 'charstring-malformed "Private DICT out of bounds"))
                    (setf (cff-font-private-range font) value))
                   (:font-matrix (setf (cff-font-matrix font) (normalize-font-matrix value)))
                   (:vstore
                    (when cff2-p
                      ;; The store is preceded by a 2 byte length.
                      (setf (cff-font-var-store font)
                            (runes/otf:read-item-variation-store fd (+ value 2)))))))
        (unless (cff-font-charstrings font)
          (fail 'charstring-missing-resource "no CharStrings"))
        (setf (cff-font-kind font)
              (cond ((cff-font-fd-array font) (if cff2-p :cff2 :cid))
                    ((or has-ros cff2-p)
                     (fail 'charstring-missing-resource "no FDArray"))
                    (t :sid)))
        (setf (cff-font-upem font)
              (or upem (if (cff-font-matrix font) (font-matrix-scale (cff-font-matrix font)) 1000)))
        font))))

(defun cff-num-glyphs (font) (cff-index-count (cff-font-charstrings font)))

(defun cff-num-subfonts (font)
  (if (eq (cff-font-kind font) :sid) 1 (cff-index-count (cff-font-fd-array font))))

(defun cff-subfont-index (font gid)
  "Index of the subfont for GID, or NIL if the FDSelect does not cover it."
  (if (or (eq (cff-font-kind font) :sid) (null (cff-font-fd-select font)))
      0
      (fd-select-index (cff-font-data font) (cff-font-fd-select font) gid)))

(defun cff-blend-state (font vs-index coords)
  "Blend state at COORDS, or NIL for a font without variations."
  (let ((store (cff-font-var-store font)))
    (when store
      (handler-case (make-blend-state store coords vs-index)
        (charstring-error () nil)))))

(defun cff-subfont (font index &optional (coords #()))
  "Subfont INDEX, from its Private DICT; COORDS select the instance for a
CFF2 Private DICT that blends."
  (multiple-value-bind (range matrix)
      (if (eq (cff-font-kind font) :sid)
          (values (cff-font-private-range font) nil)
          (let ((entries (dict-entries (cff-index-ref (cff-font-fd-array font) index))))
            (let ((error (assoc :error entries)))
              (when error (error (cdr error))))
            (values (or (dict-entry entries :private)
                        (fail 'charstring-missing-resource "font DICT without a Private DICT"))
                    (dict-entry entries :font-matrix))))
    (let* ((fd (cff-font-data font))
           (subfont (make-cff-subfont :matrix matrix)))
      (unless (and range (<= (cdr range) (runes/otf:font-data-length fd)))
        (fail 'charstring-malformed "Private DICT out of bounds"))
      ;; Every entry, the hinting parameters included, as skrifa's PrivateDict.
      (let ((entries (dict-entries (runes/otf:font-data-slice fd (car range) (- (cdr range) (car range)))
                                   :blend-state (cff-blend-state font 0 coords))))
        (loop for (op . value) in entries
              do (case op
                   (:subrs (setf (cff-subfont-subrs subfont)
                                 (read-cff-index fd (+ (car range) value) (cff-font-cff2-p font))))
                   (:vsindex (setf (cff-subfont-vs-index subfont) value))
                   ;; FreeType truncates the widths to integers.
                   (:default-width-x (setf (cff-subfont-default-width subfont) (logandc2 value #xFFFF)))
                   (:nominal-width-x (setf (cff-subfont-nominal-width subfont) (logandc2 value #xFFFF)))))
        (setf (cff-subfont-hint-params subfont) (private-dict-hint-params entries)))
      subfont)))

;;; Charstring context

(defclass cff-charstring-context (charstring-context)
  ((global-subrs :initarg :global-subrs)
   (local-subrs :initarg :local-subrs :initform nil))
  (:documentation "Subroutines of a CFF or CFF2 subfont. seac is not supported:
it needs the charset and Standard Encoding."))

(defun cff-subr (index i what)
  (unless (and index (plusp (cff-index-count index)))
    (fail 'charstring-missing-resource what))
  (cff-index-ref index (+ i (subr-bias (cff-index-count index)))))

(defmethod context-subr ((c cff-charstring-context) i)
  (cff-subr (slot-value c 'local-subrs) i "no local subroutines"))

(defmethod context-global-subr ((c cff-charstring-context) i)
  (cff-subr (slot-value c 'global-subrs) i "no global subroutines"))

;;; Transform from charstring space to font units (font.rs transform and
;;; the TransformSink of charstring.rs), for unscaled outlines.  The points
;;; are mapped by TRANSFORM-POINT in charstring.lisp.

(defun cff-transform (font subfont)
  "The matrix elements (or NIL for the identity) and the fixed point scale
(or NIL) that map the subfont's charstring coordinates to font units."
  (let* ((top (cff-font-matrix font))
         (sub (cff-subfont-matrix subfont))
         (matrix (cond ((and top sub) (combine-font-matrices top sub))
                       (top top)
                       (sub (normalize-font-matrix sub))))
         (scale nil))
    ;; A matrix implying another units per em than the font's forces a scale.
    (when (and matrix (/= (font-matrix-scale matrix) (cff-font-upem font)))
      (setf scale (fixed-mul-div (fixed-from-int 64) (cff-font-upem font) (font-matrix-scale matrix))))
    (values (when (and matrix (not (equalp (font-matrix-elements matrix) #(#x10000 0 0 #x10000 0 0))))
              (font-matrix-elements matrix))
            scale)))

(defun subfont-context (font subfont)
  "SUBFONT's charstring context, made once."
  (or (cff-subfont-context subfont)
      (setf (cff-subfont-context subfont)
            (make-instance 'cff-charstring-context
                           :global-subrs (cff-font-global-subrs font)
                           :local-subrs (cff-subfont-subrs subfont)))))

(defun cff-draw-glyph (font subfont gid sink &optional (coords #()))
  "Evaluate glyph GID of SUBFONT, sending its outline in font units (fixed
point) to SINK, without degenerate commands. Return the charstring's
advance width in charstring units: the stated width plus nominalWidthX,
or defaultWidthX. COORDS are normalized coordinates of a CFF2 instance."
  (let* ((context (subfont-context font subfont))
         (transform (or (cff-subfont-transform subfont)
                        (setf (cff-subfont-transform subfont)
                              (multiple-value-call #'cons (cff-transform font subfont)))))
         (charstring (cff-index-ref (cff-font-charstrings font) gid))
         (blend (cff-blend-state font (cff-subfont-vs-index subfont) coords))
         ;; As in Fontations' draw: transform, then filter.
         (width (evaluate-charstring context charstring
                                     (make-outline-filter sink :transform-p t
                                                               :matrix (car transform)
                                                               :scale (cdr transform))
                                     :blend-state blend)))
    (if width
        (fixed+ width (cff-subfont-nominal-width subfont))
        (cff-subfont-default-width subfont))))

;;; Hinted outlines

(defun cff-hint-scale (font subfont ppem)
  "The hinting scale of SUBFONT at PPEM pixels per em: charstring units to
pixels, through 26.6 as FreeType, and adjusted for a font matrix of
another units per em (skrifa's subfont and scale_for_hinting)."
  (let ((scale (fixed/ (truncate (* ppem 64)) (cff-font-upem font))))
    (multiple-value-bind (matrix forced-scale) (cff-transform font subfont)
      (declare (ignore matrix))
      (when forced-scale
        (setf scale (fixed-mul-div scale (cff-font-upem font)
                                   (font-matrix-scale (or (cff-subfont-matrix subfont)
                                                          (cff-font-matrix font)))))))
    (values (truncate (min (+ scale 32) +fixed-max+) 64))))

(defun cff-hint-state (font subfont ppem)
  "SUBFONT's hinting state at PPEM pixels per em."
  (make-hint-state-for (cff-subfont-hint-params subfont) (cff-hint-scale font subfont ppem)))

(defun cff-draw-hinted-glyph (font subfont gid sink state &optional (coords #()))
  "As CFF-DRAW-GLYPH, but sending glyph GID hinted with STATE (from
CFF-HINT-STATE), in pixels.  Fonts with a font matrix are not hinted yet."
  (when (car (cff-transform font subfont))
    (fail 'charstring-invalid-operation "hinting a font with a font matrix"))
  (let ((width (evaluate-charstring (subfont-context font subfont)
                                    (cff-index-ref (cff-font-charstrings font) gid)
                                    (make-hinting-sink state (make-outline-filter sink))
                                    :blend-state (cff-blend-state font (cff-subfont-vs-index subfont)
                                                                  coords))))
    (if width
        (fixed+ width (cff-subfont-nominal-width subfont))
        (cff-subfont-default-width subfont))))
