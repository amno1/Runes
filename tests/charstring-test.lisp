;;;; charstring-test.lisp — Tests for the charstring evaluator
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; Ported from the tests in Fontations' read-fonts/src/ps/charstring.rs.

(defpackage #:zpb-otf/charstring-test
  (:use #:cl #:zpb-otf/charstring)
  (:export #:run-charstring-tests))

(in-package #:zpb-otf/charstring-test)

(defvar *failures* 0)
(defvar *count* 0)

(defmacro check (desc form)
  `(progn
     (incf *count*)
     (handler-case
         (if ,form
             (format t "  [PASS] ~A~%" ,desc)
             (progn (incf *failures*)
                    (format *error-output* "  [FAIL] ~A~%" ,desc)))
       (error (e)
         (incf *failures*)
         (format *error-output* "  [FAIL] ~A (error: ~A)~%" ,desc e)))))

(defmacro check-error (desc type form)
  `(progn
     (incf *count*)
     (handler-case
         (progn ,form
                (incf *failures*)
                (format *error-output* "  [FAIL] ~A (no error)~%" ,desc))
       (,type ()
         (format t "  [PASS] ~A~%" ,desc))
       (error (e)
         (incf *failures*)
         (format *error-output* "  [FAIL] ~A (wrong error: ~A)~%" ,desc e)))))

;;; Helpers

(defun f (n) "Fixed point bits of N." (float->fixed n))

(defclass capture-sink ()
  ((commands :initform '() :accessor commands)))

(defmethod sink-move-to ((s capture-sink) x y) (push (list :move x y) (commands s)))
(defmethod sink-line-to ((s capture-sink) x y) (push (list :line x y) (commands s)))
(defmethod sink-curve-to ((s capture-sink) cx0 cy0 cx1 cy1 x y)
  (push (list :curve cx0 cy0 cx1 cy1 x y) (commands s)))
;; As in Fontations' tests, a close is a line back to the last move.
(defmethod sink-close ((s capture-sink))
  (let ((last-move (find :move (commands s) :key #'first)))
    (push (list :line (if last-move (second last-move) 0) (if last-move (third last-move) 0))
          (commands s))))

(defun result (s) (reverse (commands s)))

(defun octets (&rest items)
  "Octet vector from ITEMS: bytes, lists of items, and (:u16 n) / (:u32 n)
for big-endian words."
  (let ((out '()))
    (labels ((add (item)
               (cond ((integerp item) (push item out))
                     ((member (first item) '(:u16 :u32))
                      (let ((n (second item)))
                        (loop for shift from (if (eq (first item) :u16) 8 24) downto 0 by 8
                              do (push (ldb (byte 8 shift) n) out))))
                     (t (mapc #'add item)))))
      (mapc #'add items))
    (coerce (nreverse out) '(simple-array (unsigned-byte 8) (*)))))

(defun ctx (&optional (kind :type2))
  (make-instance 'simple-charstring-context :kind kind))

(defun run (charstring &key (context (ctx)) blend-state)
  (let ((sink (make-instance 'capture-sink)))
    (evaluate-charstring context charstring sink :blend-state blend-state)
    (result sink)))

(defun n (v) (assert (<= -107 v 107)) (list (+ v 139)))
(defun esc (op) (list 12 op))

(defun eval-moved (&rest ops)
  "Evaluate OPS followed by rmoveto endchar; the first command."
  (first (run (octets ops 21 14))))

(defun moved-y (&rest ops) (third (apply #'eval-moved ops)))

(defun binary (a b op) (moved-y (n 0) (n a) (n b) (esc op)))

(defun evaluator (kind)
  (zpb-otf/charstring::make-evaluator-for (ctx kind) nil (make-instance 'capture-sink)))

(defun push-all (ev &rest numbers)
  "Push integers, or fixed values for non-integers, as Fontations' tests do."
  (let ((stack (zpb-otf/charstring::ev-stack ev)))
    (dolist (x numbers)
      (if (integerp x)
          (zpb-otf/charstring::stack-push-int stack x)
          (zpb-otf/charstring::stack-push-fixed stack (f x))))))

(defun op (ev operator)
  (zpb-otf/charstring::evaluate-operator ev operator (zpb-otf/charstring::empty-cursor) 0))

(defun stack-floats (ev)
  (let ((stack (zpb-otf/charstring::ev-stack ev)))
    (loop for i below (zpb-otf/charstring::stack-len stack)
          collect (fixed->float (zpb-otf/charstring::stack-get-fixed stack i)))))

;;; Tests

(defparameter *all-path-ops*
  (octets 251 29 253 12 21 173 134 159 133 5 140 141 142 6 251 71 129 142 7
          109 154 161 147 89 165 125 97 98 158 124 164 8 109 154 161 147 27
          147 109 154 161 147 27 163 159 154 180 181 119 153 115 114 120 125 97
          98 158 124 164 31 159 4 119 153 115 114 120 125 143 144 24 119 153
          115 114 120 125 143 144 25 84 116 117 80 30 109 154 161 147 26 147
          109 154 161 147 26 163 159 154 180 181 119 153 115 114 120 125 97 162
          12 35 163 159 154 180 181 119 153 12 34 152 22 180 181 119 153 115
          114 120 125 97 12 36 154 180 181 119 153 115 114 120 125 97 147 12
          37 14))

(defparameter *all-path-ops-expected*
  ;; From font-test-data/test_data/extracted/charstring_path_ops-glyphs.txt
  '((:move -137 -632) (:line -103 -637) (:line -83 -643) (:line -82 -643)
    (:line -82 -641) (:line -79 -641) (:line -79 -820) (:line -89 -820)
    (:line -89 -817)
    (:curve -119 -802 -97 -794 -147 -768) (:curve -161 -810 -202 -791 -217 -766)
    (:curve -247 -766 -232 -744 -224 -744) (:curve -254 -736 -239 -714 -231 -714)
    (:curve -207 -714 -187 -699 -187 -658) (:curve -187 -616 -207 -602 -231 -602)
    (:curve -256 -602 -275 -616 -275 -658) (:curve -275 -699 -256 -714 -231 -714)
    (:line -137 -632) (:move -231 -694)
    (:curve -251 -680 -275 -705 -294 -719) (:line -290 -714) (:line -310 -700)
    (:curve -334 -725 -353 -739 -349 -734) (:curve -349 -789 -372 -811 -431 -811)
    (:curve -431 -841 -416 -819 -416 -811) (:curve -408 -841 -393 -819 -393 -811)
    (:curve -369 -791 -354 -750 -312 -770) (:curve -298 -794 -323 -813 -337 -855)
    (:curve -313 -855 -293 -840 -252 -840) (:curve -210 -840 -230 -855 -216 -855)
    (:line -231 -694) (:move -203 -855)
    (:curve -162 -813 -182 -799 -206 -799) (:curve -231 -799 -250 -813 -292 -855)
    (:curve -277 -814 -235 -834 -221 -858) (:curve -246 -877 -260 -919 -292 -911)
    (:line -203 -855)))

(defun in-units (commands)
  (mapcar (lambda (c) (cons (first c) (mapcar (lambda (v) (/ v 65536)) (rest c)))) commands))

(defun test-paths ()
  (format t "~%--- Charstring path construction ---~%")
  (check "all_path_ops: every path operator"
         (equal (in-units (run *all-path-ops*)) *all-path-ops-expected*))
  (check "cff2_example_subr: blend at the default location"
         ;; 50 50 100 1 blend 0 rmoveto  500 -100 -200 1 blend hlineto
         ;; 500 vlineto  -500 100 200 1 blend hlineto
         (equal (in-units (run (octets 189 189 239 140 16 139 21 248 136 39 251 92 140 16 6
                                       248 136 7 252 136 239 247 92 140 16 6)
                               ;; The example's variation store has two regions.
                               :blend-state (make-instance 'constant-blend-state :scalars #(0 0))))
                '((:move 50 0) (:line 550 0) (:line 550 500) (:line 50 500) (:line 50 0))))
  (check "cff2 blend applies the region scalar"
         ;; 50 50 100 1 blend 0 rmoveto: width 50, dx = 50 + 0.5 * 100
         (equal (first (in-units (run (octets 189 189 239 140 16 139 21)
                                      :blend-state (make-instance 'constant-blend-state
                                                                  :scalars (vector (f 0.5))))))
                '(:move 100 0)))
  (check-error "blend without a blend state" charstring-missing-resource
               (run (octets 189 189 140 16 139 21)))
  (check "ignore_reserved_operators"
         (equal (run (octets 0 32 22 2))
                (list (list :move (f -107) 0) (list :line (f -107) 0))))
  (check "operation_limit: 1,000,000 operations succeed"
         (progn (run (make-array +max-operations+ :element-type '(unsigned-byte 8)
                                                 :initial-element 0))
                t))
  (check-error "operation_limit: one more fails" charstring-limit-exceeded
               (run (make-array (1+ +max-operations+) :element-type '(unsigned-byte 8)
                                                     :initial-element 0)))
  (check "coords_remaining_avoid_overflow: hhcurveto with two operands"
         (let ((ev (evaluator :type2)))
           (push-all ev 0 0)
           (op ev :hhcurveto)
           t))
  (check "vmoveto moves vertically"
         (equal (run (octets (n 0) (n 5) 4))
                (list (list :move 0 (f 5)) (list :line 0 (f 5)))))
  (check "endchar is synthesized and reads the width"
         (= (f 300) (evaluate-width (ctx) (octets 247 192)))))

(defun test-subroutines ()
  (format t "~%--- Charstring subroutines ---~%")
  (check "subr-bias thresholds"
         (equal (mapcar #'subr-bias '(0 1239 1240 33899 33900)) '(107 107 1131 1131 32768)))
  (let ((context (make-instance 'simple-charstring-context
                                :local-subrs (vector (octets (n 10) (n 20) 21 11))
                                :global-subrs (vector (octets (n 1) 6 11)))))
    ;; Subroutine 0 is called as -107 with fewer than 1240 subroutines.
    (check "callsubr and callgsubr apply the bias"
           (equal (in-units (run (octets (n -107) 10 (n -107) 29 14) :context context))
                  '((:move 10 20) (:line 11 20) (:line 10 20))))
    (check-error "callsubr out of range" charstring-malformed
                 (run (octets (n 0) 10 14) :context context)))
  (check-error "callsubr without subroutines" charstring-missing-resource
               (run (octets (n 0) 10 14)))
  (let ((context (make-instance 'simple-charstring-context
                                :local-subrs (vector (octets (n -107) 10)))))
    (check-error "recursion is limited" charstring-limit-exceeded
                 (run (octets (n -107) 10) :context context))))

(defun test-type1 ()
  (format t "~%--- Type 1 operators ---~%")
  (check "op_div" (let ((ev (evaluator :type2)))
                    (push-all ev 512.5 2) (op ev :div)
                    (equal (stack-floats ev) '(256.25d0))))
  (check "op_div_type1_large_int" (let ((ev (evaluator :type1)))
                                    (push-all ev 32001 2) (op ev :div)
                                    (equal (stack-floats ev) '(16000.5d0))))
  (flet ((h-sbw (operator kind)
           (let ((ev (evaluator kind)))
             (apply #'push-all ev (if (eq operator :sbw) '(42.5 0 501 1000) '(42.5 501 1000)))
             (op ev operator)
             (list (zpb-otf/charstring::ev-sbx ev) (zpb-otf/charstring::ev-wx ev)))))
    (check "op_sbw_type1" (equal (h-sbw :sbw :type1) (list (f 42.5) (f 501))))
    (check "op_hsbw_type1" (equal (h-sbw :hsbw :type1) (list (f 42.5) (f 501))))
    (check "op_sbw_type2_no_effect" (equal (h-sbw :sbw :type2) '(0 0)))
    (check "op_hsbw_type2_no_effect" (equal (h-sbw :hsbw :type2) '(0 0))))
  (check "op_callothersubr_flex"
         (let* ((ev (evaluator :type1))
                (sink (zpb-otf/charstring::ev-sink ev)))
           (push-all ev 0 1) (op ev :callothersubr)
           (dotimes (i 7) (push-all ev 1 2) (op ev :rmoveto))
           (push-all ev 0 100 200)
           (push-all ev 3 0) (op ev :callothersubr)
           (op ev :setcurrentpoint)
           (and (= (zpb-otf/charstring::ev-x ev) (f 100))
                (= (zpb-otf/charstring::ev-y ev) (f 200))
                (equal (in-units (result sink))
                       '((:move 0 0) (:curve 2 4 3 6 4 8) (:curve 5 10 6 12 7 14))))))
  (check "roll_by_the_most_negative_shift"
         (equal (first (run (octets (n 1) (n 2) (n 2) 255 #x80 0 0 0 (esc 30) 21 14)
                            :context (ctx :type1)))
                (list :move (f 1) (f 2)))))

(defun mm-context (weights)
  (make-instance 'simple-charstring-context :kind :type1
                                            :weight-vector (map 'vector #'f weights)
                                            :build-char-len 8))

(defun othersubr (ev &rest args)
  (apply #'push-all ev args)
  (op ev :callothersubr)
  (stack-floats ev))

(defun test-multiple-master ()
  (format t "~%--- Multiple master othersubrs ---~%")
  (check "mm_blend"
         (let ((ev (zpb-otf/charstring::make-evaluator-for (mm-context '(0 -0.25 1)) nil
                                                           (make-instance 'capture-sink))))
           (push-all ev 1 0 2 3 4 -8 6 15)
           (op ev :callothersubr)
           (equal (stack-floats ev) '(3.5d0 -9.0d0))))
  (flet ((fresh (weights)
           (zpb-otf/charstring::make-evaluator-for (mm-context weights) nil
                                                   (make-instance 'capture-sink))))
    (check "othersubr_arithmetic"
           (loop for (args expected) in '(((7 2 2 20) 9) ((7 2 2 21) 5) ((7 2 2 22) 14)
                                          ((7 2 2 23) 3.5) ((1 2 3 4 4 27) 1) ((1 2 5 4 4 27) 2))
                 always (equal (apply #'othersubr (fresh '(0 0 0)) args)
                               (list (coerce expected 'double-float)))))
    (check-error "othersubr division by zero" charstring-invalid-operation
                 (othersubr (fresh '(0 0 0)) 7 0 2 23))
    (check-error "othersubr wrong argument count" charstring-invalid-operation
                 (othersubr (fresh '(0 0 0)) 7 2 1 20))
    (let ((ev (fresh '(0.25 0.5 0.25))))
      (check "othersubr_build_char_array: store and read back"
             (and (null (othersubr ev 5 3 2 24))
                  (equal (othersubr ev 3 1 25) '(5.0d0))))
      (zpb-otf/charstring::reset-stack ev)
      (check "othersubr_build_char_array: copy the weights"
             (and (null (othersubr ev 2 1 19))
                  (equal (othersubr ev 3 1 25) '(0.5d0))))
      (zpb-otf/charstring::reset-stack ev)
      (check-error "build char array bounds (24)" charstring-invalid-operation (othersubr ev 1 8 2 24))
      (check-error "build char array bounds (25)" charstring-invalid-operation (othersubr ev 8 1 25))
      (check-error "build char array bounds (19)" charstring-invalid-operation (othersubr ev 6 1 19))))
  (flet ((plain () (evaluator :type1)))
    (check "othersubrs_without_a_blend_fail"
           (loop for args in '((1 2 2 14) (0 1 19) (5 0 2 24) (0 1 25))
                 always (handler-case (progn (apply #'othersubr (plain) args) nil)
                          (charstring-error () t))))
    (check "othersubr arithmetic works without a blend"
           (equal (othersubr (plain) 7 2 2 20) '(9.0d0)))))

(defun test-arithmetic ()
  (format t "~%--- Arithmetic, logic and stack operators ---~%")
  (check "arithmetic: add sub mul div"
         (and (= (binary 7 5 10) (f 12)) (= (binary 7 5 11) (f 2))
              (= (binary 7 5 24) (f 35)) (= (binary 10 4 12) (f 2.5))))
  (check "logic_answers_with_one_or_zero"
         (loop for (a b op expected) in '((1 1 3 1) (1 0 3 0) (0 0 3 0) (1 0 4 1) (0 0 4 0)
                                          (5 5 15 1) (5 6 15 0))
               always (= (binary a b op) (f expected))))
  (check "unary: not abs neg"
         (and (= (moved-y (n 0) (n 0) (esc 5)) (f 1))
              (loop for (v op expected) in '((-5 9 5) (5 9 5) (5 14 -5) (-5 14 5))
                    always (= (moved-y (n 0) (n v) (esc op)) (f expected)))))
  (check "sqrt_of_a_square_is_the_root"
         (and (loop for v in '(1 4 9 16 100)
                    always (<= (abs (- (moved-y (n 0) (n v) (esc 26)) (f (sqrt v)))) 2))
              (loop for v in '(0 -4) always (= (moved-y (n 0) (n v) (esc 26)) 0))))
  (check "the_stack_operators_rearrange_it"
         (and (equal (eval-moved (n 0) (n 7) (esc 27)) (list :move (f 7) (f 7)))
              (equal (eval-moved (n 3) (n 9) (esc 28)) (list :move (f 9) (f 3)))
              (equal (eval-moved (n 4) (n 5) (n 6) (esc 18)) (list :move (f 4) (f 5)))
              (equal (eval-moved (n 0) (n 8) (n 0) (esc 29)) (list :move (f 8) (f 8)))))
  (check "roll_rotates_the_values_it_is_given"
         (equal (eval-moved (n 1) (n 2) (n 3) (n 3) (n 1) (esc 30)) (list :move (f 1) (f 2))))
  (check "roll_leaves_the_values_below_alone"
         (equal (eval-moved (n 99) (n 1) (n 2) (n 2) (n 1) (esc 30)) (list :move (f 2) (f 1))))
  (check "the_transient_array_remembers_between_operators"
         (equal (eval-moved (n 0) (n 42) (n 3) (esc 20) (n 3) (esc 21)) (list :move 0 (f 42))))
  (check "a_slot_the_array_does_not_have_is_ignored"
         (equal (eval-moved (n 3) (n 9) (n 99) (esc 20) (n 4)) (list :move (f 3) (f 4))))
  (check "ifelse_picks_the_first_when_the_conditions_are_ordered"
         (loop for (c1 c2 expected) in '((1 2 11) (2 1 22) (2 2 11))
               always (= (moved-y (n 0) (n 11) (n 22) (n c1) (n c2) (esc 22)) (f expected))))
  (check-error "callsubr with a fixed index fails" charstring-invalid-operation
               (run (octets 255 0 1 0 0 10 14)))
  (check "255 is fixed point in Type 2 and an integer in Type 1"
         (and (equal (first (run (octets 255 0 1 #x80 0 (n 0) 21 14)))
                     (list :move (f 1.5) 0))
              (equal (first (run (octets 255 0 0 0 3 (n 0) 21 14) :context (ctx :type1)))
                     (list :move (f 3) 0)))))

;;; CFF2 blending over an ItemVariationStore

(defparameter *cff2-example*
  ;; The example CFF2 table from the OpenType spec, as in Fontations'
  ;; font-test-data (cff2::EXAMPLE). Its ItemVariationStore is at offset 18
  ;; and a charstring that blends is at #xC8..#xE1.
  (octets
   #x02 #x00 #x05 #x00 #x07 #xCF #x0C #x24 #xC3 #x11 #x9B #x18 #x00 #x00 #x00 #x00
   #x00 #x26 #x00 #x01 #x00 #x00 #x00 #x0C #x00 #x01 #x00 #x00 #x00 #x1C #x00 #x01
   #x00 #x02 #xC0 #x00 #xE0 #x00 #x00 #x00 #xC0 #x00 #xC0 #x00 #xE0 #x00 #x00 #x00
   #x00 #x00 #x00 #x02 #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x02 #x01 #x01 #x03 #x05
   #x20 #x0A #x20 #x0A #x00 #x00 #x00 #x01 #x01 #x01 #x05 #xF7 #x06 #xDA #x12 #x77
   #x9F #xF8 #x6C #x9D #xAE #x9A #xF4 #x9A #x95 #x9F #xB3 #x9F #x8B #x8B #x8B #x8B
   #x85 #x9A #x8B #x8B #x97 #x73 #x8B #x8B #x8C #x80 #x8B #x8B #x8B #x8D #x8B #x8B
   #x8C #x8A #x8B #x8B #x97 #x17 #x06 #xFB #x8E #x95 #x86 #x9D #x8B #x8B #x8D #x17
   #x07 #x77 #x9F #xF8 #x6D #x9D #xAD #x9A #xF3 #x9A #x95 #x9F #xB3 #x9F #x08 #xFB
   #x8D #x95 #x09 #x1E #xA0 #x37 #x5F #x0C #x09 #x8B #x0C #x0B #xC2 #x6E #x9E #x8C
   #x17 #x0A #xDB #x57 #xF7 #x02 #x8C #x17 #x0B #xB3 #x9A #x77 #x9F #x82 #x8A #x8D
   #x17 #x0C #x0C #xDB #x95 #x57 #xF7 #x02 #x85 #x8B #x8D #x17 #x0C #x0D #xF7 #x06
   #x13 #x00 #x00 #x00 #x01 #x01 #x01 #x1B #xBD #xBD #xEF #x8C #x10 #x8B #x15 #xF8
   #x88 #x27 #xFB #x5C #x8C #x10 #x06 #xF8 #x88 #x07 #xFC #x88 #xEF #xF7 #x5C #x8C
   #x10 #x06))

(defun example-blend (&rest coords)
  (make-blend-state (zpb-otf:read-item-variation-store
                     (zpb-otf:make-font-data *cff2-example*) 18)
                    coords 0))

(defun test-blend ()
  (format t "~%--- CFF2 blend state ---~%")
  ;; example_blends in blend.rs: (coordinate, expected scalars)
  (loop for (coord expected) in '((-1 (0 1)) (-0.25 (0.5 0)) (-0.5 (1 0)) (-0.75 (0.5 0.5))
                                  (0 (0 0)) (0.5 (0 0)) (1 (0 0)))
        do (check (format nil "example_blends at ~A" coord)
                  (equalp (blend-scalars (example-blend coord)) (map 'vector #'f expected))))
  (check-error "vsindex past the store" charstring-malformed
               (blend-set-store-index (example-blend 0) 1))
  ;; A store with two ItemVariationData subtables: one axis, region 0
  ;; peaking at +1 and region 1 at -1; subtable 0 uses region 0, subtable 1
  ;; uses regions 1 and 0.
  (let* ((store-bytes (octets '(:u16 1) '(:u32 16) '(:u16 2) '(:u32 32) '(:u32 40)
                              '(:u16 1) '(:u16 2)
                              '(:u16 0) '(:u16 #x4000) '(:u16 #x4000)
                              '(:u16 #xC000) '(:u16 #xC000) '(:u16 0)
                              '(:u16 0) '(:u16 0) '(:u16 1) '(:u16 0)
                              '(:u16 0) '(:u16 0) '(:u16 2) '(:u16 1) '(:u16 0)))
         (store (zpb-otf:read-item-variation-store (zpb-otf:make-font-data store-bytes) 0))
         (state (make-blend-state store '(-0.5) 0)))
    (check "store index 0 at -0.5" (equalp (blend-scalars state) (vector 0)))
    (blend-set-store-index state 1)
    (check "vsindex 1 recomputes the scalars"
           (and (= 1 (blend-store-index state))
                (= 2 (blend-region-count state))
                (equalp (blend-scalars state) (vector (f 0.5) 0))))
    (blend-set-store-index state 0)
    (check "vsindex 0 restores the first scalars"
           (and (= 0 (blend-store-index state))
                (equalp (blend-scalars state) (vector 0))))
    (check "the store index can be chosen up front"
           (equalp (blend-scalars (make-blend-state store '(0.5) 1))
                   (vector 0 (f 0.5)))))
  (check "example charstring at -0.5 (scalars 1, 0)"
         ;; 50 + 1 * 50 rmoveto, 500 + 1 * -100 hlineto, 500 vlineto, -500 + 1 * 100 hlineto
         (equal (in-units (run (subseq *cff2-example* #xC8 #xE2) :blend-state (example-blend -0.5)))
                '((:move 100 0) (:line 500 0) (:line 500 500) (:line 100 500) (:line 100 0))))
  (check "example charstring at the default location"
         (equal (in-units (run (subseq *cff2-example* #xC8 #xE2) :blend-state (example-blend 0)))
                '((:move 50 0) (:line 550 0) (:line 550 500) (:line 50 500) (:line 50 0)))))

(defun run-charstring-tests ()
  (setf *failures* 0 *count* 0)
  (format t "========================================================~%")
  (format t "Charstring Evaluator Tests~%")
  (format t "========================================================~%")
  (test-paths)
  (test-subroutines)
  (test-type1)
  (test-multiple-master)
  (test-arithmetic)
  (test-blend)
  (format t "~%Results: ~D tests run, ~D failures.~%" *count* *failures*)
  (unless (zerop *failures*)
    (error "Charstring test suite failed with ~D failure(s)" *failures*))
  t)
