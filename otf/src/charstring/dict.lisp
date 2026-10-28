;;;; dict.lisp — CFF and CFF2 DICT parsing
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; A port of Fontations' read-fonts src/ps/cff/dict.rs, with the binary
;;; coded decimal parsing of src/ps/num.rs and the font matrix parsing of
;;; src/ps/transform.rs, all of which follow FreeType.
;;;
;;; Hinting parameters (blue values, stem snaps) are returned as the plain
;;; list of their delta-decoded fixed point values.

(in-package #:runes/otf/charstring)

;;; Binary coded decimal reals (num.rs)

(defconstant +bcd-overflow+ #x7FFFFFFF)
(defconstant +bcd-underflow+ 0)
;; Accumulation of digits stops at this value; further digits raise the exponent.
(defconstant +bcd-number-limit+ #xCCCCCCC)
;; Limit for the integral part of a result.
(defconstant +bcd-integer-limit+ #x7FFF)

(defparameter *power-tens*
  #(1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000))

(defstruct bcd
  "Components of a binary coded decimal number. ERROR is the value to
return when overflow or underflow was found while parsing."
  (error nil) (number 0) (sign 1) (exponent 0) (exponent-add 0)
  (integer-len 0) (fraction-len 0))

(defun parse-bcd (c)
  "Parse the nibbles of a real number after the 30 operand byte."
  (let ((phase :integer) (sign 1) (exponent-sign 1) (number 0) (exponent 0)
        (exponent-add 0) (integer-len 0) (fraction-len 0) (overflow nil))
    (block nibbles
      (loop
        (let ((b (cursor-u8 c)))
          (dolist (nibble (list (ldb (byte 4 4) b) (ldb (byte 4 0) b)))
            (ecase phase
              (:integer
               (cond ((<= nibble 9)
                      (cond ((>= number +bcd-number-limit+) (incf exponent-add))
                            ((or (/= nibble 0) (/= number 0))
                             (setf number (+ (* number 10) nibble))
                             (incf integer-len))))
                     ((= nibble #xE) (setf sign -1))
                     ((= nibble #xA) (setf phase :fraction))
                     ((= nibble #xB) (setf phase :exponent))
                     ((= nibble #xC) (setf phase :exponent exponent-sign -1))
                     (t (return-from nibbles))))
              (:fraction
               (cond ((<= nibble 9)
                      (cond ((and (= nibble 0) (= number 0)) (decf exponent-add))
                            ((and (< number +bcd-number-limit+) (< fraction-len 9))
                             (setf number (+ (* number 10) nibble))
                             (incf fraction-len))))
                     ((= nibble #xB) (setf phase :exponent))
                     ((= nibble #xC) (setf phase :exponent exponent-sign -1))
                     (t (return-from nibbles))))
              (:exponent
               (cond ((<= nibble 9)
                      ;; Keep consuming nibbles past an absurd exponent so the
                      ;; cursor ends up after the number.
                      (if (> exponent 1000)
                          (setf overflow t)
                          (setf exponent (+ (* exponent 10) nibble))))
                     (t (return-from nibbles)))))))))
    (if (and overflow (/= number 0))
        (make-bcd :error (if (= exponent-sign -1)
                             +bcd-underflow+
                             (s32 (* +bcd-overflow+ sign))))
        (make-bcd :number number :sign sign :exponent (* exponent exponent-sign)
                  :exponent-add exponent-add :integer-len integer-len
                  :fraction-len fraction-len))))

(defun bcd-value (bcd &optional scale-by-1000)
  "Fixed point value of BCD. SCALE-BY-1000 computes with an internal factor
of 1000 for more fractional precision, as FreeType does for BlueScale."
  (when (bcd-error bcd)
    (return-from bcd-value (bcd-error bcd)))
  (let ((number (bcd-number bcd))
        (exponent (bcd-exponent bcd))
        (integer-len (bcd-integer-len bcd))
        (fraction-len (bcd-fraction-len bcd)))
    (when (zerop number)
      (return-from bcd-value 0))
    (incf exponent (if scale-by-1000 (+ 3 (bcd-exponent-add bcd)) (bcd-exponent-add bcd)))
    (incf integer-len exponent)
    (decf fraction-len exponent)
    (when (> integer-len 5) (return-from bcd-value +bcd-overflow+))
    (when (< integer-len -5) (return-from bcd-value +bcd-underflow+))
    ;; Remove non-significant digits.
    (when (minusp integer-len)
      (setf number (truncate number (aref *power-tens* (- integer-len))))
      (incf fraction-len integer-len))
    ;; Only possible with a non-zero exponent.
    (when (= fraction-len 10)
      (setf number (truncate number 10))
      (decf fraction-len))
    (let ((result (if (plusp fraction-len)
                      (let ((b (aref *power-tens* fraction-len)))
                        (if (> (truncate number b) +bcd-integer-limit+)
                            0
                            (fixed/ number b)))
                      (progn
                        (setf number (s32 (* number (aref *power-tens* (- fraction-len)))))
                        (when (> number +bcd-integer-limit+)
                          (return-from bcd-value +bcd-overflow+))
                        (ash number 16)))))
      (when scale-by-1000
        (setf result (fixed/ result (fixed-from-int 1000))))
      (s32 (* result (bcd-sign bcd))))))

(defun bcd-dynamic-value (bcd)
  "Fixed point value of BCD with a dynamically chosen power of ten scaling,
as two values; used for FontMatrix components."
  (when (bcd-error bcd)
    (return-from bcd-dynamic-value (values (bcd-error bcd) 0)))
  (let ((number (bcd-number bcd))
        (exponent (+ (bcd-exponent bcd) (bcd-exponent-add bcd)))
        (integer-len (bcd-integer-len bcd))
        (fraction-len (bcd-fraction-len bcd))
        result scaling)
    (when (zerop number)
      (return-from bcd-dynamic-value (values 0 0)))
    (incf fraction-len integer-len)
    (incf exponent integer-len)
    (cond
      ((<= fraction-len 5)
       (cond
         ((> number +bcd-integer-limit+)
          (setf result (fixed/ number 10)
                scaling (+ (- exponent fraction-len) 1)))
         (t
          (if (plusp exponent)
              ;; Make the scaling as small as possible.
              (let* ((new-fraction-len (min exponent 5))
                     (shift (- new-fraction-len fraction-len)))
                (if (plusp shift)
                    (progn
                      (decf exponent new-fraction-len)
                      (setf number (s32 (* number (aref *power-tens* shift))))
                      (when (> number +bcd-integer-limit+)
                        (setf number (truncate number 10))
                        (incf exponent)))
                    (decf exponent fraction-len)))
              (decf exponent fraction-len))
          (setf result (s32 (ash number 16))
                scaling exponent))))
      ((> (truncate number (aref *power-tens* (- fraction-len 5))) +bcd-integer-limit+)
       (setf result (fixed/ number (aref *power-tens* (- fraction-len 4)))
             scaling (- exponent 4)))
      (t
       (setf result (fixed/ number (aref *power-tens* (- fraction-len 5)))
             scaling (- exponent 5))))
    (values (s32 (* result (bcd-sign bcd))) scaling)))

(defun parse-fixed-dynamic (c)
  "Read a DICT number with dynamic scaling, as two values (value scaling)."
  (let ((b0 (cursor-u8 c)))
    (cond
      ((= b0 30) (bcd-dynamic-value (parse-bcd c)))
      ((or (= b0 28) (= b0 29) (<= 32 b0 254))
       (let ((num (parse-int c b0)))
         (if (> num +bcd-integer-limit+)
             (let* ((int-len (or (loop for i from 5 below 10
                                       when (< num (aref *power-tens* i)) return i)
                                 10))
                    (scaling (if (> (- num (aref *power-tens* (- int-len 5))) +bcd-integer-limit+)
                                 (- int-len 4)
                                 (- int-len 5))))
               (values (fixed/ num (aref *power-tens* scaling)) scaling))
             (values (s32 (ash num 16)) 0))))
      (t (fail 'charstring-malformed "not a number")))))

;;; Font matrices (transform.rs)

(defstruct (scaled-font-matrix (:conc-name font-matrix-))
  "An affine matrix #(xx yx xy yy dx dy) of fixed values, and the units per
em it implies."
  (elements (vector #x10000 0 0 #x10000 0 0))
  (scale 1000))

(defun matrix-degenerate-p (elements)
  "True for a matrix that cannot be inverted, by FreeType's test."
  (let* ((xx (aref elements 0)) (yx (aref elements 1))
         (xy (aref elements 2)) (yy (aref elements 3))
         (val (logior (abs xx) (abs yx) (abs xy) (abs yy))))
    (when (or (zerop val) (> val #x7FFFFFFF))
      (return-from matrix-degenerate-p t))
    ;; Scale down to avoid overflowing the products below.
    (let ((shift (- (1- (integer-length val)) 12)))
      (when (plusp shift)
        (setf xx (ash xx (- shift)) xy (ash xy (- shift))
              yx (ash yx (- shift)) yy (ash yy (- shift)))))
    (<= (* 32 (abs (- (* xx yy) (* xy yx))))
        (+ (* xx xx) (* xy xy) (* yx yx) (* yy yy)))))

(defun parse-font-matrix (c)
  "Read the six FontMatrix operands with dynamic scaling. NIL for an
invalid or degenerate matrix."
  (let ((values (make-array 6)) (scalings (make-array 6))
        (max-scaling nil) (min-scaling nil))
    (handler-case
        (dotimes (i 6)
          (multiple-value-bind (v s) (parse-fixed-dynamic c)
            (unless (zerop v)
              (setf max-scaling (if max-scaling (max max-scaling s) s)
                    min-scaling (if min-scaling (min min-scaling s) s)))
            (setf (aref values i) v (aref scalings i) s)))
      (charstring-error () (return-from parse-font-matrix nil)))
    (unless (and max-scaling (<= -9 max-scaling 0) (<= 0 (- max-scaling min-scaling) 9))
      (return-from parse-font-matrix nil))
    (dotimes (i 6)
      (let ((v (aref values i)))
        (unless (zerop v)
          (let* ((divisor (aref *power-tens* (- max-scaling (aref scalings i))))
                 (half (ash divisor -1)))
            (setf (aref values i)
                  (if (minusp v)
                      (if (< (+ (- (ash 1 31)) half) v)
                          (truncate (- v half) divisor)
                          (truncate (- (ash 1 31)) divisor))
                      (if (> (- #x7FFFFFFF half) v)
                          (truncate (+ v half) divisor)
                          (truncate #x7FFFFFFF divisor))))))))
    (unless (matrix-degenerate-p values)
      (make-scaled-font-matrix :elements values
                               :scale (aref *power-tens* (- max-scaling))))))

(defun normalize-font-matrix (m)
  "The matrix rescaled so that its y scale is 1.0, with the units per em
adjusted to match and the fractional bits of the translation dropped."
  (let* ((elements (copy-seq (font-matrix-elements m)))
         (scale (font-matrix-scale m))
         (factor (abs (if (/= (aref elements 3) 0) (aref elements 3) (aref elements 1)))))
    (unless (= factor #x10000)
      (setf scale (fixed/ scale factor))
      (dotimes (i 6)
        (setf (aref elements i) (fixed/ (aref elements i) factor))))
    (setf (aref elements 4) (ash (aref elements 4) -16)
          (aref elements 5) (ash (aref elements 5) -16))
    (make-scaled-font-matrix :elements elements :scale scale)))

(defun combine-scaled (a b scale)
  "Elements of the product of matrix elements A and B with the scaling
factor SCALE; B's translation is transformed by A's upper 2x2, as FreeType
does when concatenating the top and font DICT matrices."
  (let ((val (fixed-from-int scale)))
    (flet ((md (x y) (fixed-mul-div x y val)))
      (let ((x (aref b 4)) (y (aref b 5)))
        (vector (fixed+ (md (aref a 0) (aref b 0)) (md (aref a 2) (aref b 1)))
                (fixed+ (md (aref a 1) (aref b 0)) (md (aref a 3) (aref b 1)))
                (fixed+ (md (aref a 0) (aref b 2)) (md (aref a 2) (aref b 3)))
                (fixed+ (md (aref a 1) (aref b 2)) (md (aref a 3) (aref b 3)))
                (fixed+ (md x (aref a 0)) (md y (aref a 2)))
                (fixed+ (md x (aref a 1)) (md y (aref a 3))))))))

(defun combine-font-matrices (top sub)
  "Concatenate a top DICT matrix with a font DICT matrix and normalize."
  (let ((scaling (if (and (> (font-matrix-scale top) 1) (> (font-matrix-scale sub) 1))
                     (min (font-matrix-scale top) (font-matrix-scale sub))
                     1)))
    (normalize-font-matrix
     (make-scaled-font-matrix
      :elements (combine-scaled (font-matrix-elements top) (font-matrix-elements sub) scaling)
      :scale (fixed-mul-div (font-matrix-scale sub) (font-matrix-scale top) scaling)))))

;;; DICT operators

(defparameter *dict-operators*
  (let ((table (make-hash-table)))
    (loop for (code name) on
          '(0 :version 1 :notice 2 :full-name 3 :family-name 4 :weight 5 :font-bbox
            13 :unique-id 14 :xuid 15 :charset 16 :encoding 17 :charstrings
            18 :private 24 :vstore 6 :blue-values 7 :other-blues 8 :family-blues
            9 :family-other-blues 10 :std-hw 11 :std-vw 19 :subrs 20 :default-width-x
            21 :nominal-width-x 22 :vsindex 23 :blend
            ;; Escaped (12 n) operators are keyed 1200 + n.
            1200 :copyright 1201 :is-fixed-pitch 1202 :italic-angle
            1203 :underline-position 1204 :underline-thickness 1205 :paint-type
            1206 :charstring-type 1207 :font-matrix 1208 :stroke-width
            1220 :synthetic-base 1221 :postscript 1222 :base-font-name
            1223 :base-font-blend 1230 :ros 1231 :cid-font-version
            1232 :cid-font-revision 1233 :cid-font-type 1234 :cid-count
            1235 :uid-base 1236 :fd-array 1237 :fd-select 1238 :font-name
            1209 :blue-scale 1210 :blue-shift 1211 :blue-fuzz 1212 :stem-snap-h
            1213 :stem-snap-v 1214 :force-bold 1217 :language-group
            1218 :expansion-factor 1219 :initial-random-seed)
          by #'cddr
          do (setf (gethash code table) name))
    table))

(defparameter *unhinted-private-operators*
  '(:subrs :vsindex :default-width-x :nominal-width-x))

(defun stack-fixed-values (s)
  (loop for i below (stack-len s) collect (stack-get-fixed s i)))

(defun stack-apply-delta-prefix-sum (s)
  "Decode delta-encoded operands into running sums, wrapping as FreeType does."
  (when (> (stack-len s) 1)
    (let ((sum 0))
      (dotimes (i (stack-len s))
        (setf sum (fixed+ sum (stack-get-fixed s i)))
        (stack-set-fixed s i sum)))))

(defun non-negative (n)
  (if (minusp n) (fail 'charstring-malformed "negative offset") n))

(defun parse-dict-entry (op s)
  "The value of operator OP from the operands on stack S."
  (ecase op
    ((:version :notice :full-name :family-name :weight :copyright :postscript
      :base-font-name :font-name :paint-type :charstring-type :language-group
      :unique-id :synthetic-base :cid-font-type :uid-base :initial-random-seed)
     (stack-pop-int s))
    ((:charstrings :vstore :fd-array :fd-select :subrs :encoding :charset :cid-count)
     (non-negative (stack-pop-int s)))
    (:vsindex (ldb (byte 16 0) (stack-pop-int s)))
    ((:is-fixed-pitch :force-bold) (/= 0 (stack-pop-int s)))
    ((:italic-angle :underline-position :underline-thickness :stroke-width
      :blue-scale :blue-shift :blue-fuzz :expansion-factor :cid-font-version
      :cid-font-revision :std-hw :std-vw :default-width-x :nominal-width-x)
     (stack-pop-fixed s))
    (:font-bbox (stack-fixed-array s 0 4))
    (:private
     ;; Size then offset; the result is (start . end).
     (let ((len (non-negative (stack-get-int s 0)))
           (start (non-negative (stack-get-int s 1))))
       (cons start (+ start len))))
    ((:blue-values :other-blues :family-blues :family-other-blues :stem-snap-h :stem-snap-v)
     (stack-apply-delta-prefix-sum s)
     (stack-fixed-values s))
    (:ros (list (stack-get-int s 0) (stack-get-int s 1) (stack-get-fixed s 2)))
    ((:xuid :base-font-blend) t)))

(defun dict-entries (dict &key blend-state unhinted-private)
  "Entries of the DICT DICT (an octet vector or RUNES/OTF:FONT-DATA), as a list
of (operator . value), in order. An entry that cannot be read appears as
(:error . condition) and the walk continues, as in Fontations.

BLEND-STATE is required for a CFF2 Private DICT that uses blend or vsindex.
With UNHINTED-PRIVATE, only the entries an unhinted subfont needs are
returned, though every operand is still parsed."
  (let* ((c (charstring-cursor dict))
         (start (cursor-pos c))
         (stack (make-stack))
         (last-bcd nil)
         ;; Where the operands of the operator being read begin, for the
         ;; FontMatrix re-parse.
         (operands-pos start)
         (entries '()))
    (flet ((err (condition) (push (cons :error condition) entries)))
      (loop while (plusp (cursor-remaining c))
            do (handler-case
                   (let ((b0 (cursor-u8 c)))
                     (cond
                       ((or (= b0 28) (= b0 29) (<= 32 b0 254))
                        (setf last-bcd nil)
                        (stack-push-int stack (parse-int c b0)))
                       ((= b0 30)
                        (let ((bcd (parse-bcd c)))
                          (stack-push-fixed stack (bcd-value bcd))
                          (setf last-bcd bcd)))
                       (t
                        (let ((op (gethash (if (= b0 12) (+ 1200 (cursor-u8 c)) b0)
                                           *dict-operators*)))
                          (cond
                            ;; FreeType ignores unknown operators and clears the stack.
                            ((null op) (stack-clear stack))
                            ((and (member op '(:blend :vsindex)) (null blend-state))
                             (err (make-condition 'charstring-missing-resource
                                                  :message "blend or vsindex without a blend state")))
                            ((eq op :blend)
                             (stack-apply-blend stack blend-state))
                            (t
                             (when (eq op :vsindex)
                               (blend-set-store-index blend-state
                                                      (ldb (byte 16 0) (stack-get-int stack 0))))
                             (let ((keep (or (not unhinted-private)
                                             (member op *unhinted-private-operators*))))
                               (when (and (eq op :blue-scale) last-bcd)
                                 ;; FreeType computes BlueScale with a factor of 1000.
                                 (stack-pop-fixed stack)
                                 (stack-push-fixed stack (bcd-value last-bcd t)))
                               (cond
                                 ((eq op :font-matrix)
                                  ;; Re-parse the operands with dynamic scaling.
                                  (stack-clear stack)
                                  (setf last-bcd nil)
                                  (let ((matrix (parse-font-matrix
                                                 (make-cursor (cursor-bytes c) operands-pos (cursor-end c)))))
                                    (when (and matrix keep)
                                      (push (cons :font-matrix matrix) entries))))
                                 (t
                                  (setf last-bcd nil)
                                  (unwind-protect
                                       (when keep
                                         (push (cons op (parse-dict-entry op stack)) entries))
                                    (stack-clear stack)
                                    (setf operands-pos (cursor-pos c))))))))))))
                 (charstring-error (e) (err e)))))
    (nreverse entries)))

(defun dict-entry (entries op &optional default)
  "Value of the first entry for OP in ENTRIES."
  (let ((entry (assoc op entries)))
    (if entry (cdr entry) default)))
