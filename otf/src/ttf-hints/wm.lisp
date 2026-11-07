;;;; wm.lisp — TrueType hinting virtual machine: value stack, function
;;;; definitions, call stack, engine and program execution
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:runes/otf/tt-hint)

;;; Compiled for speed: the types are declared, and the checks left are the
;;; ones the font can trigger (stack, point and table indices, program bounds).
(declaim (optimize (speed 3) (safety 1) (debug 0)))

(declaim (ftype (function (value-stack) t) vs-dup vs-swap vs-roll)
         (ftype (function (value-stack fixnum) t) vs-copy-index vs-move-index))

;;; ============================================================================
;;; Value Stack (value_stack.rs)
;;; ============================================================================

(defconstant +max-call-stack+ 128)
(defconstant +max-instructions+ 1000000)

(defstruct (value-stack (:conc-name vs-) (:constructor %make-value-stack))
  (values (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (top 0 :type fixnum))

(defun make-value-stack (max-elements)
  (%make-value-stack :values (make-array max-elements :element-type 's32 :initial-element 0)))

(declaim (inline vs-len vs-clear vs-push vs-pop vs-peek))

(defun vs-len (s) (declare (value-stack s)) (vs-top s))
(defun vs-clear (s) (declare (value-stack s)) (setf (vs-top s) 0))

(defun vs-push (s val)
  (declare (value-stack s) (s32 val) (optimize (speed 3) (safety 0)))
  (let ((top (vs-top s))
        (arr (vs-values s)))
    (when (>= top (length arr))
      (hint-fail 'tt-hint-stack-overflow "TrueType value stack overflow"))
    (setf (aref arr top) val
          (vs-top s) (1+ top))))

(defun vs-pop (s)
  (declare (value-stack s) (optimize (speed 3) (safety 0)))
  (let ((top (vs-top s)))
    ;; Like FreeType's interpreter when it is not pedantic, an empty stack
    ;; yields 0 where a hinting program pops too much.  DUP, SWAP, ROLL,
    ;; CINDEX and MINDEX below are still strict: FreeType does not fail on
    ;; those either, but no font tried needs it.
    (when (<= top 0)
      (return-from vs-pop 0))
    (let ((new-top (1- top)))
      (setf (vs-top s) new-top)
      (aref (vs-values s) new-top))))

(defun vs-peek (s)
  (declare (value-stack s) (optimize (speed 3) (safety 0)))
  (let ((top (vs-top s)))
    (if (<= top 0) 0 (aref (vs-values s) (1- top)))))

(defun vs-dup (s)
  (declare (value-stack s))
  (let ((top (vs-top s)))
    (when (<= top 0)
      (hint-fail 'tt-hint-stack-underflow "DUP on empty stack"))
    (vs-push s (aref (vs-values s) (1- top)))))

(defun vs-swap (s)
  (declare (value-stack s))
  (let ((top (vs-top s))
        (arr (vs-values s)))
    (when (< top 2)
      (hint-fail 'tt-hint-stack-underflow "SWAP requires 2 elements"))
    (rotatef (aref arr (1- top)) (aref arr (- top 2)))))

(defun vs-roll (s)
  (declare (value-stack s))
  (let ((top (vs-top s))
        (arr (vs-values s)))
    (when (< top 3)
      (hint-fail 'tt-hint-stack-underflow "ROLL requires 3 elements"))
    (let ((a (aref arr (- top 3)))
          (b (aref arr (- top 2)))
          (c (aref arr (- top 1))))
      (setf (aref arr (- top 3)) b
            (aref arr (- top 2)) c
            (aref arr (- top 1)) a))))

(defun vs-copy-index (s idx)
  (declare (value-stack s) (fixnum idx))
  (let ((top (vs-top s)))
    (when (or (<= idx 0) (> idx top))
      (hint-fail 'tt-hint-stack-underflow "CINDEX index out of bounds"))
    (vs-push s (aref (vs-values s) (- top idx)))))

(defun vs-move-index (s idx)
  (declare (value-stack s) (fixnum idx))
  (let ((top (vs-top s))
        (arr (vs-values s)))
    (when (or (<= idx 0) (> idx top))
      (hint-fail 'tt-hint-stack-underflow "MINDEX index out of bounds"))
    (let* ((pos (- top idx))
           (val (aref arr pos)))
      (loop for i of-type fixnum from pos below (1- top)
            do (setf (aref arr i) (aref arr (1+ i))))
      (setf (aref arr (1- top)) val))))

;;; ============================================================================
;;; Program Definitions (FDEF / IDEF) and Call Stack
;;; ============================================================================

(deftype program-id () '(integer 0 2))

(defstruct (fn-def (:conc-name fdef-))
  (active-p nil :type boolean)
  (key 0 :type s32)       ; full s32 key for FDEF, or u8 opcode for IDEF
  (program 0 :type program-id)
  (start 0 :type fixnum)
  (end 0 :type fixnum))

(defstruct (call-record (:conc-name call-))
  (caller-program 0 :type program-id)
  (return-pc 0 :type fixnum)
  (current-end 0 :type fixnum)
  (loop-count 1 :type fixnum)
  (def-start 0 :type fixnum)
  (def-program 0 :type program-id))

;;; ============================================================================
;;; VM Execution Context (engine.rs)
;;; ============================================================================

(defstruct (tt-engine (:conc-name eng-) (:constructor %make-tt-engine))
  (programs (vector #() #() #()) :type (simple-vector 3))
  (cur-program 0 :type program-id)
  (bytes (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (pc 0 :type fixnum)
  (end 0 :type fixnum)
  (stack nil :type value-stack)
  (call-stack (make-array +max-call-stack+) :type simple-vector)
  (call-top 0 :type fixnum)
  (fdefs (make-array 0) :type simple-vector)
  (idefs (make-array 0) :type simple-vector)
  (storage (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (default-storage (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (cvt (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (default-cvt (make-array 0 :element-type 's32) :type (simple-array s32 (*)))
  (twilight nil :type tt-zone)
  (default-twilight nil :type tt-zone)
  (glyph nil :type tt-zone)
  (gs (make-graphics-state) :type graphics-state)
  (default-gs (make-graphics-state) :type graphics-state)
  (ppem 0 :type fixnum)
  (point-size 0 :type s32)
  (scale 0 :type s32)
  (cvt-scale 0 :type s32)
  ;; Scale of unscaled coordinates to pixels: SCALE for a simple glyph,
  ;; 1.0 for a composite one, whose unscaled points are already scaled.
  (unscaled-scale 0 :type s32)
  (composite-p nil :type boolean)
  ;; Per-engine caches of the scaler: a reusable outline buffer (the engine
  ;; is used by one thread at a time) and the offsets of 'glyf' and 'loca'.
  ;; Where the glyph program starts in its byte array, which is the font's.
  (glyph-base 0 :type fixnum)
  ;; Set when a program writes the CVT or the storage; the glyph scaler
  ;; restores the post-prep copies before the next glyph only if it is.
  (tables-dirty nil :type boolean)
  (scratch nil)
  ;; The phantom points, eight numbers each, that loading a glyph makes, so that
  ;; none is allocated: two for each depth of composite glyph (what the glyph
  ;; starts from, and what it comes to), as many as have been needed.
  (phantoms (vector) :type simple-vector)
  (depth 0 :type fixnum)
  ;; :REQUEST while a caller wants the points of a simple glyph left in the glyph
  ;; zone instead of the hint buffer; :DONE once they are.
  (direct nil)
  (gvar-scratch nil)                    ; work arrays of the variation deltas
  (tables nil)
  (rotated-p nil :type boolean)
  (stretched-p nil :type boolean)
  (axis-coords #() :type simple-vector))

(declaim (sb-ext:freeze-type value-stack fn-def call-record tt-engine))

(defun make-tt-engine (&key (max-stack 256) (max-storage 64) (max-fdefs 64)
                            (max-idefs 8) (max-twilight 32) (cvt-len 0))
  (let ((call-stack (make-array +max-call-stack+))
        (fdefs (make-array (max max-fdefs 64)))
        (idefs (make-array (max max-idefs 8))))
    (dotimes (i +max-call-stack+) (setf (svref call-stack i) (make-call-record)))
    (dotimes (i (length fdefs))   (setf (svref fdefs i) (make-fn-def)))
    (dotimes (i (length idefs))   (setf (svref idefs i) (make-fn-def)))
    (%make-tt-engine
     :stack (make-value-stack (max max-stack 256))
     :call-stack call-stack
     :fdefs fdefs
     :idefs idefs
     :storage (make-array max-storage :element-type 's32 :initial-element 0)
     :default-storage (make-array max-storage :element-type 's32 :initial-element 0)
     :cvt (make-array cvt-len :element-type 's32 :initial-element 0)
     :default-cvt (make-array cvt-len :element-type 's32 :initial-element 0)
     :twilight (allocate-zone max-twilight 1)
     :default-twilight (allocate-zone max-twilight 1)
     :glyph (allocate-zone 128 16))))

(declaim (inline eng-zone eng-zp0 eng-zp1 eng-zp2 fetch-u8 fetch-i16))

(defun eng-zone (eng z)
  (declare (tt-engine eng) (type (integer 0 1) z) (optimize (speed 3) (safety 0)))
  (if (zerop z) (eng-twilight eng) (eng-glyph eng)))

(defun eng-zp0 (eng) (eng-zone eng (gs-zp0 (eng-gs eng))))
(defun eng-zp1 (eng) (eng-zone eng (gs-zp1 (eng-gs eng))))
(defun eng-zp2 (eng) (eng-zone eng (gs-zp2 (eng-gs eng))))

(defun fetch-u8 (eng)
  (declare (tt-engine eng) (optimize (speed 3) (safety 0)))
  (let ((pc (eng-pc eng)))
    (when (>= pc (eng-end eng))
      (hint-fail 'tt-hint-invalid-opcode "Bytecode stream ended unexpectedly"))
    (setf (eng-pc eng) (1+ pc))
    (aref (eng-bytes eng) pc)))

(defun fetch-i16 (eng)
  (declare (tt-engine eng) (optimize (speed 3) (safety 0)))
  (let* ((hi (fetch-u8 eng))
         (lo (fetch-u8 eng))
         (u (logior (ash hi 8) lo)))
    (if (logbitp 15 u) (- u 65536) u)))

(defun opcode-payload-bytes (eng op)
  (declare (tt-engine eng) (type (unsigned-byte 8) op) (optimize (speed 3) (safety 0)))
  (cond
    ((= op #x40) (fetch-u8 eng))
    ((= op #x41) (* 2 (fetch-u8 eng)))
    ((<= #xB0 op #xB7) (+ (- op #xB0) 1))
    ((<= #xB8 op #xBF) (* 2 (+ (- op #xB8) 1)))
    (t 0)))

(defun skip-code-until (eng stop-for-else-p stop-for-endf-p)
  (declare (tt-engine eng) (optimize (speed 3) (safety 0)))
  (let ((if-depth 0))
    (declare (fixnum if-depth))
    (loop
      (let* ((op (fetch-u8 eng))
             (payload (opcode-payload-bytes eng op)))
        (when (> (+ (eng-pc eng) payload) (eng-end eng))
          (hint-fail 'tt-hint-invalid-opcode "Truncated push payload while skipping bytecode"))
        (incf (eng-pc eng) payload)
        (cond
          (stop-for-endf-p
           (when (or (= op #x2C) (= op #x89))
             (hint-fail 'tt-hint-invalid-opcode "Nested FDEF/IDEF"))
           (when (= op #x2D)
             (return :endf)))
          (t
           (case op
             (#x58 (incf if-depth))
             (#x1B (when (and (zerop if-depth) stop-for-else-p)
                     (return :else)))
             (#x59 (if (zerop if-depth)
                       (return :eif)
                       (decf if-depth))))))))))

(defun select-program (eng prog-id start end)
  (declare (tt-engine eng) (type program-id prog-id) (fixnum start end))
  (let ((bytes (svref (eng-programs eng) prog-id)))
    (setf (eng-cur-program eng) prog-id
          (eng-bytes eng) bytes
          (eng-pc eng) start
          (eng-end eng) end)))

(defun find-fdef (eng key)
  "Find active FDEF for function number KEY."
  (declare (tt-engine eng) (s32 key))
  (let ((fdefs (eng-fdefs eng)))
    (or (when (< -1 key (length fdefs))
          (let ((def (svref fdefs key)))
            (when (and (fdef-active-p def) (= (fdef-key def) key))
              def)))
        (loop for def across fdefs
              when (and (fdef-active-p def) (= (fdef-key def) key))
                return def))))

(defun invoke-function (eng def count)
  (declare (tt-engine eng) (fn-def def) (fixnum count))
  (when (> count 0)
    (when (>= (eng-call-top eng) +max-call-stack+)
      (hint-fail 'tt-hint-limit-exceeded "Call stack overflow"))
    (let ((rec (svref (eng-call-stack eng) (eng-call-top eng))))
      (setf (call-caller-program rec) (eng-cur-program eng)
            (call-return-pc rec)      (eng-pc eng)
            (call-current-end rec)    (eng-end eng)
            (call-loop-count rec)     count
            (call-def-start rec)      (fdef-start def)
            (call-def-program rec)    (fdef-program def))
      (incf (eng-call-top eng))
      (select-program eng (fdef-program def) (fdef-start def) (fdef-end def)))))

(defun run-program (eng prog-id &optional (start 0) end)
  "Run program PROG-ID of ENG from START to END (default: all of it)."
  (declare (tt-engine eng) (type program-id prog-id) (fixnum start)
           (optimize (speed 3) (safety 1)))
  (let* ((bytes (svref (eng-programs eng) prog-id))
         (end (or end (length bytes)))
         (executed 0))
    (declare (fixnum end executed))
    (when (> end start)
      (setf (eng-call-top eng) 0
            (eng-glyph-base eng) start)
      (select-program eng prog-id start end)
      (loop while (< (eng-pc eng) (eng-end eng))
            ;; The limit is for programs that never end, which this finds as
            ;; well when it looks once in 1024 instructions (late by up to that
            ;; many).
            do (when (zerop (logand (incf executed) 1023))
                 (when (> executed +max-instructions+)
                   (hint-fail 'tt-hint-limit-exceeded "Exceeded maximum instruction count")))
               (let ((op (fetch-u8 eng)))
                 (dispatch-instruction eng op)
                 (loop while (and (>= (eng-pc eng) (eng-end eng))
                                  (plusp (eng-call-top eng)))
                       do (op-endf eng)))))))
