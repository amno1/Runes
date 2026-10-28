;;;; cff.lisp — CFF and CFF2 fonts: INDEXes, subfonts and glyph evaluation
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; A port of Fontations' read-fonts src/ps/cff/font.rs (CffFontRef and
;;; Subfont, without the hinting parameters and the name metadata), with
;;; index.rs, fd_select.rs and the TransformSink of src/ps/charstring.rs.
;;;
;;; Positions are relative to the start of the CFF or CFF2 table, which is
;;; held as a ZPB-OTF:FONT-DATA window.

(in-package #:zpb-otf/charstring)

(defun cff-u8 (fd pos) (zpb-otf:font-data-read-u8 fd pos))
(defun cff-uint (fd pos size)
  (let ((v 0))
    (dotimes (i size v)
      (setf v (logior (ash v 8) (cff-u8 fd (+ pos i)))))))

;;; INDEX (index.rs)

(defstruct (cff-index (:constructor %make-cff-index))
  (data nil) (count 0) (off-size 0) (offsets-pos 0)
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
  (let ((offset (cff-uint (cff-index-data index)
                          (+ (cff-index-offsets-pos index) (* i (cff-index-off-size index)))
                          (cff-index-off-size index))))
    ;; The first offset is always 1; a zero is malformed.
    (when (zerop offset)
      (fail 'charstring-malformed "zero INDEX offset"))
    offset))

(defun cff-index-ref (index i)
  "Object I of INDEX as a FONT-DATA window."
  (unless (< -1 i (cff-index-count index))
    (fail 'charstring-malformed (format nil "INDEX entry ~D out of range" i)))
  (let* ((start (cff-index-offset index i))
         (end (cff-index-offset index (1+ i)))
         (fd (cff-index-data index)))
    (when (or (< end start) (> (+ (cff-index-base index) end) (zpb-otf:font-data-length fd)))
      (fail 'charstring-malformed "INDEX entry out of bounds"))
    (zpb-otf:font-data-slice fd (+ (cff-index-base index) start) (- end start))))

(defun cff-index-end (index)
  "Position just past INDEX."
  (if (zerop (cff-index-count index))
      (cff-index-base index)
      (+ (cff-index-base index) (cff-index-offset index (cff-index-count index)))))

;;; FDSelect (fd_select.rs)

(defun fd-select-index (fd pos gid)
  "Font DICT index for GID from the FDSelect at POS, or NIL if not covered."
  (let ((format (cff-u8 fd pos)))
    (case format
      (0 (handler-case (cff-u8 fd (+ pos 1 gid))
           (zpb-otf:font-out-of-bounds-error () nil)))
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
  (vs-index 0))

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
             (values t (zpb-otf:font-data-slice fd header-size top-length)
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
                    (when (> (cdr value) (zpb-otf:font-data-length fd))
                      (fail 'charstring-malformed "Private DICT out of bounds"))
                    (setf (cff-font-private-range font) value))
                   (:font-matrix (setf (cff-font-matrix font) (normalize-font-matrix value)))
                   (:vstore
                    (when cff2-p
                      ;; The store is preceded by a 2 byte length.
                      (setf (cff-font-var-store font)
                            (zpb-otf:read-item-variation-store fd (+ value 2)))))))
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
      (unless (and range (<= (cdr range) (zpb-otf:font-data-length fd)))
        (fail 'charstring-malformed "Private DICT out of bounds"))
      (loop for (op . value) in (dict-entries (zpb-otf:font-data-slice fd (car range) (- (cdr range) (car range)))
                                              :blend-state (cff-blend-state font 0 coords)
                                              :unhinted-private t)
            do (case op
                 (:subrs (setf (cff-subfont-subrs subfont)
                               (read-cff-index fd (+ (car range) value) (cff-font-cff2-p font))))
                 (:vsindex (setf (cff-subfont-vs-index subfont) value))
                 ;; FreeType truncates the widths to integers.
                 (:default-width-x (setf (cff-subfont-default-width subfont) (logandc2 value #xFFFF)))
                 (:nominal-width-x (setf (cff-subfont-nominal-width subfont) (logandc2 value #xFFFF)))))
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
;;; the TransformSink of charstring.rs), for unscaled outlines.

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

(defclass transform-sink ()
  ((inner :initarg :inner)
   (matrix :initarg :matrix)
   (scale :initarg :scale))
  (:documentation "Sink applying a CFF transform before passing commands on."))

(defun transform-point (sink x y)
  "Map charstring X, Y to font units exactly as FreeType does."
  (with-slots (matrix scale) sink
    ;; Multiply by 1/64 and drop the bottom 10 bits: integer font units.
    (let ((bx (ash (fixed* x #x400) -10))
          (by (ash (fixed* y #x400) -10)))
      (multiple-value-bind (cx cy)
          (if matrix
              (values (fixed+ (fixed+ (fixed* (aref matrix 0) bx) (fixed* (aref matrix 2) by)) (aref matrix 4))
                      (fixed+ (fixed+ (fixed* (aref matrix 1) bx) (fixed* (aref matrix 3) by)) (aref matrix 5)))
              (values bx by))
        (if scale
            ;; To 26.6, then to 16.16.
            (values (s32 (ash (fixed* cx scale) 10)) (s32 (ash (fixed* cy scale) 10)))
            (values (s32 (ash cx 16)) (s32 (ash cy 16))))))))

(defmethod sink-move-to ((s transform-sink) x y)
  (multiple-value-bind (x y) (transform-point s x y)
    (sink-move-to (slot-value s 'inner) x y)))
(defmethod sink-line-to ((s transform-sink) x y)
  (multiple-value-bind (x y) (transform-point s x y)
    (sink-line-to (slot-value s 'inner) x y)))
(defmethod sink-curve-to ((s transform-sink) cx0 cy0 cx1 cy1 x y)
  (multiple-value-bind (cx0 cy0) (transform-point s cx0 cy0)
    (multiple-value-bind (cx1 cy1) (transform-point s cx1 cy1)
      (multiple-value-bind (x y) (transform-point s x y)
        (sink-curve-to (slot-value s 'inner) cx0 cy0 cx1 cy1 x y)))))
(defmethod sink-close ((s transform-sink)) (sink-close (slot-value s 'inner)))
(defmethod sink-finish ((s transform-sink)) (sink-finish (slot-value s 'inner)))

(defun cff-draw-glyph (font subfont gid sink &optional (coords #()))
  "Evaluate glyph GID of SUBFONT, sending its outline in font units (fixed
point) to SINK, without degenerate commands. Return the charstring's
advance width in charstring units: the stated width plus nominalWidthX,
or defaultWidthX. COORDS are normalized coordinates of a CFF2 instance."
  (let* ((context (make-instance 'cff-charstring-context
                                 :global-subrs (cff-font-global-subrs font)
                                 :local-subrs (cff-subfont-subrs subfont)))
         (charstring (cff-index-ref (cff-font-charstrings font) gid))
         (blend (cff-blend-state font (cff-subfont-vs-index subfont) coords))
         (width (multiple-value-bind (matrix scale) (cff-transform font subfont)
                  ;; As in Fontations' draw: transform, then filter.
                  (evaluate-charstring context charstring
                                       (make-instance 'transform-sink
                                                      :inner (make-instance 'nop-filter-sink :inner sink)
                                                      :matrix matrix :scale scale)
                                       :blend-state blend))))
    (if width
        (fixed+ width (cff-subfont-nominal-width subfont))
        (cff-subfont-default-width subfont))))
