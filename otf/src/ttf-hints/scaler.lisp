;;;; scaler.lisp — TrueType hinted glyph scaler, engine pool, and FreeType test harness API
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-ttf)

;;; Compiled for speed: the types are declared, and the checks left are the
;;; ones the font can trigger (stack, point and table indices, program bounds).
(declaim (optimize (speed 3) (safety 1) (debug 0)))

(export '(hinted-contours
          hinted-cubic-contours
          hinted-advance-width
          hint-outline-26dot6))

;;; ============================================================================
;;; 1. 'cvar' (Control Value Table Variations)
;;; ============================================================================

(defun read-unscaled-cvt (loader)
  (let ((info (table-info "cvt " loader)))
    (if (null info)
        #()
        (let* ((fd (font-data loader))
               (off (offset info))
               (count (floor (size info) 2))
               (cvt (make-array count :initial-element 0)))
          (dotimes (i count cvt)
            (setf (svref cvt i)
                  (fixed-from-int (runes/otf:font-data-read-i16 fd (+ off (* 2 i))))))))))

(defun apply-cvar-deltas (loader cvt coords)
  (let ((info (table-info "cvar" loader))
        (fvar-info (table-info "fvar" loader)))
    (when (and info fvar-info (plusp (length cvt)) (plusp (length coords)))
      (handler-case
          (let* ((fd (font-data loader))
                 (fvar (runes/otf:read-fvar fd (offset fvar-info) (size fvar-info)))
                 (axis-count (runes/otf:fvar-axis-count fvar))
                 (start (offset info))
                 (stop (+ start (size info)))
                 (tuple-count-raw (runes/otf:font-data-read-u16 fd (+ start 4)))
                 (tuple-count (logand tuple-count-raw #x0FFF))
                 (serialized (+ start (runes/otf:font-data-read-u16 fd (+ start 6))))
                 (shared-points nil))
            (when (logbitp 15 tuple-count-raw)
              (let ((c (make-gvar-cursor :fd fd :pos serialized :end stop)))
                (setf shared-points (read-packed-points c)
                      serialized (gc-pos c))))
            (let ((header-pos (+ start 8))
                  (data-pos serialized)
                  (tuple-bytes (* 2 axis-count)))
              (dotimes (i tuple-count)
                (let* ((data-size (runes/otf:font-data-read-u16 fd header-pos))
                       (tuple-idx (runes/otf:font-data-read-u16 fd (+ header-pos 2)))
                       (p (+ header-pos 4))
                       (peak (when (logbitp 15 tuple-idx)
                               (prog1 (read-tuple fd p axis-count)
                                 (incf p tuple-bytes))))
                       (intermediate (when (logbitp 14 tuple-idx)
                                       (let ((s (read-tuple fd p axis-count))
                                             (e (read-tuple fd (+ p tuple-bytes) axis-count)))
                                         (incf p (* 2 tuple-bytes))
                                         (cons s e))))
                       (data-end (+ data-pos data-size)))
                  (when (> data-end stop) (return))
                  (let ((scalar (and peak (gvar-tuple-scalar peak intermediate coords))))
                    (when scalar
                      (let* ((c (make-gvar-cursor :fd fd :pos data-pos :end data-end))
                             (pts (if (logbitp 13 tuple-idx)
                                      (read-packed-points c)
                                      (or shared-points (gvar-fail)))))
                        (if (eq pts :all)
                            (read-dense c (length cvt)
                                        (lambda (idx d)
                                          (add-scaled-delta cvt idx d scalar)))
                            (read-sparse c pts (length pts)
                                         (lambda (idx d)
                                           (when (< idx (length cvt))
                                             (add-scaled-delta cvt idx d scalar))))))))
                  (setf header-pos p
                        data-pos data-end)))))
        (gvar-data-error () nil))))
  cvt)

;;; ============================================================================
;;; 2. Engine Initialization & Lock-Free Thread-Safe Pool
;;; ============================================================================

(defun table-bytes-slice (loader tag)
  (let ((info (table-info tag loader)))
    (if (null info)
        (make-array 0 :element-type '(unsigned-byte 8))
        (let* ((fd (font-data loader))
               (start (+ (runes/otf:font-data-offset fd) (offset info)))
               (len (size info))
               (out (make-array len :element-type '(unsigned-byte 8))))
          (replace out (runes/otf:font-data-bytes fd) :start2 start :end2 (+ start len))
          out))))

(defun read-maxp-hint-limits (loader)
  (let ((info (table-info "maxp" loader)))
    (if (or (null info) (< (size info) 32))
        (values 256 64 64 8 32)
        (let ((maxp (runes/otf:read-maxp (font-data loader) (offset info) (size info))))
          (values (max 256 (or (runes/otf:maxp-max-stack-elements maxp) 0))
                  (max 64  (or (runes/otf:maxp-max-storage maxp) 0))
                  (max 64  (or (runes/otf:maxp-max-function-defs maxp) 0))
                  (max 8   (or (runes/otf:maxp-max-instruction-defs maxp) 0))
                  (max 32  (or (runes/otf:maxp-max-twilight-points maxp) 0)))))))

(defun compute-tt-scale (ppem upem)
  (let ((ppem-26-6 (round (* ppem 64))))
    (values (runes/otf/tt-hint::mul-div ppem-26-6 #x10000 upem)
            (round ppem)
            ppem-26-6)))

(defun make-tt-hint-engine (loader ppem &optional (coords (or (loader-gvar-coords loader) #())))
  "Create and initialize a TT-ENGINE for LOADER at PPEM and COORDS."
  (multiple-value-bind (max-stack max-storage max-fdefs max-idefs max-twilight)
      (read-maxp-hint-limits loader)
    (multiple-value-bind (scale int-ppem point-size)
        (compute-tt-scale ppem (units/em loader))
      (let* ((unscaled-cvt (apply-cvar-deltas loader (read-unscaled-cvt loader) coords))
             (cvt-len (length unscaled-cvt))
             (eng (runes/otf/tt-hint:make-tt-engine
                   :max-stack max-stack
                   :max-storage max-storage
                   :max-fdefs max-fdefs
                   :max-idefs max-idefs
                   :max-twilight max-twilight
                   :cvt-len cvt-len))
             (fpgm (table-bytes-slice loader "fpgm"))
             (prep (table-bytes-slice loader "prep"))
             (cvt (runes/otf/tt-hint::eng-cvt eng)))
        (dotimes (i cvt-len)
          (let ((funit (fixed-round (svref unscaled-cvt i))))
            (setf (aref cvt i) (runes/otf/tt-hint::mul-fix funit scale))))
        (setf (svref (runes/otf/tt-hint::eng-programs eng) 0) fpgm
              (svref (runes/otf/tt-hint::eng-programs eng) 1) prep
              (runes/otf/tt-hint::eng-ppem eng) int-ppem
              (runes/otf/tt-hint::eng-point-size eng) point-size
              (runes/otf/tt-hint::eng-scale eng) scale
              (runes/otf/tt-hint::eng-cvt-scale eng) scale
              (runes/otf/tt-hint::eng-axis-coords eng) coords)
        (when (plusp (length fpgm))
          (runes/otf/tt-hint:run-program eng 0))
        (when (plusp (length prep))
          (runes/otf/tt-hint::vs-clear (runes/otf/tt-hint::eng-stack eng))
          (runes/otf/tt-hint:run-program eng 1))
        ;; Snapshot post-prep state (GS, CVT, Storage, Twilight)
        (setf (runes/otf/tt-hint::eng-default-gs eng)
              (runes/otf/tt-hint::copy-graphics-state (runes/otf/tt-hint::eng-gs eng)))
        (replace (runes/otf/tt-hint::eng-default-cvt eng) (runes/otf/tt-hint::eng-cvt eng))
        (replace (runes/otf/tt-hint::eng-default-storage eng) (runes/otf/tt-hint::eng-storage eng))
        ;; What fpgm and prep wrote is the snapshot: the engine starts clean.
        (setf (runes/otf/tt-hint::eng-tables-dirty eng) nil)
        (runes/otf/tt-hint:copy-zone-into (runes/otf/tt-hint::eng-default-twilight eng)
                                          (runes/otf/tt-hint::eng-twilight eng))
        eng))))

;; Thread-safe Engine Pool keyed by (loader ppem coords)
;;
;; A caller takes an idle engine off the list of its (font, size, instance)
;; and puts it back when done, so that engines, which cost a font's fpgm and
;; prep to build, are reused; a caller finding the list empty builds one.
;; The sizes a font keeps engines for are bounded: past *MAX-HINTING-SIZES*
;; the ones looked up least recently are dropped.

(defvar *max-hinting-sizes* 32
  "The most sizes (with their variation instances) a font keeps hinting
engines for.  Past it, the least recently used are dropped when a new one is
made; engines in use are not affected, and are dropped with their size when
put back.")

(defvar *tt-engine-pool-lock* (sb-thread:make-mutex :name "tt-engine-pool-lock"))
(defvar *tt-engine-pools* (make-hash-table :test 'eq :weakness :key)
  "Loader -> list of POOL-ENTRYs.")

(defstruct (pool-entry (:conc-name pe-))
  (ppem-key 0 :type fixnum)
  (coords #() :type simple-vector)
  ;; When the entry was last looked up in *TT-ENGINE-POOLS*, by a counter.
  ;; Lookups that hit **TT-LAST-POOL-ENTRY** do not count, which is fine: the
  ;; size in use is the one that cache holds.
  (stamp 0 :type fixnum)
  ;; The idle engines.  Taken and returned with ATOMIC-POP and ATOMIC-PUSH.
  (engines nil :type list))

(sb-ext:defglobal **tt-last-pool-entry** nil
  "(WEAK-POINTER-TO-LOADER PPEM-KEY COORDS . POOL-ENTRY) of the last
ACQUIRE-TT-ENGINE.  A cache only: a stale or racing read is harmless.")

(defvar *tt-pool-tick* 0)

(defun find-tt-pool-entry (loader ppem-key coords)
  "The pool entry for LOADER at PPEM-KEY and COORDS; made if there is none,
dropping the least recently used ones if the font has too many."
  (declare (type fixnum ppem-key))
  (sb-thread:with-mutex (*tt-engine-pool-lock*)
    (let* ((entries (gethash loader *tt-engine-pools*))
           (entry (find-if (lambda (e)
                             (and (= (pe-ppem-key e) ppem-key)
                                  (equalp (pe-coords e) coords)))
                           entries)))
      (unless entry
        (setf entry (make-pool-entry :ppem-key ppem-key :coords (copy-seq coords)))
        (push entry entries))
      (setf (pe-stamp entry) (incf *tt-pool-tick*))
      (when (> (length entries) *max-hinting-sizes*)
        (setf entries (sort entries #'> :key #'pe-stamp))
        (let ((dropped (nthcdr (max 1 *max-hinting-sizes*) entries)))
          (setf entries (ldiff entries dropped))
          ;; Do not leave the cache pointing at one that is gone.
          (let ((last **tt-last-pool-entry**))
            (when (and last (member (cdddr last) dropped))
              (setf **tt-last-pool-entry** nil)))))
      (setf (gethash loader *tt-engine-pools*) entries)
      entry)))

(defun hinting-engine-sizes (loader)
  "The number of sizes LOADER has hinting engines for."
  (sb-thread:with-mutex (*tt-engine-pool-lock*)
    (length (gethash loader *tt-engine-pools*))))

(defun acquire-tt-engine (loader ppem coords)
  (declare (type font-loader loader) (type real ppem) (type simple-vector coords))
  (let* ((ppem-key (if (typep ppem 'fixnum)
                       (the fixnum (* (the (signed-byte 32) ppem) 64))
                       (the fixnum (round (* ppem 64)))))
         ;; The pool entry used last is found without the lock and the
         ;; lookups: text mostly asks for one size.
         (last **tt-last-pool-entry**)
         (pool-entry
           (if (and last
                    (eq (sb-ext:weak-pointer-value (car last)) loader)
                    (= (the fixnum (cadr last)) ppem-key)
                    (or (eq (caddr last) coords) (equalp (caddr last) coords)))
               (cdddr last)
               (let ((entry (find-tt-pool-entry loader ppem-key coords)))
                 (setf **tt-last-pool-entry**
                       (list* (sb-ext:make-weak-pointer loader) ppem-key
                              (pe-coords entry) entry))
                 entry)))
         (eng (sb-ext:atomic-pop (pe-engines pool-entry))))
    (values (or eng (make-tt-hint-engine loader ppem coords))
            pool-entry)))

(defun release-tt-engine (eng pool-entry)
  (sb-ext:atomic-push eng (pe-engines pool-entry)))

(defmacro with-pooled-tt-engine ((eng loader ppem coords) &body body)
  (let ((entry (gensym "POOL-ENTRY")))
    `(multiple-value-bind (,eng ,entry) (acquire-tt-engine ,loader ,ppem ,coords)
       (unwind-protect
            (progn ,@body)
         (release-tt-engine ,eng ,entry)))))

;;; ============================================================================
;;; 3. Hinted Simple & Composite Glyph Loading (Arena Zone Reuse)
;;; ============================================================================

(declaim (ftype (function (runes/otf:font-data (unsigned-byte 32) (unsigned-byte 16))
                          (values (unsigned-byte 33) (unsigned-byte 33)))
                simple-glyph-instruction-range)
         (ftype (function (runes/otf:font-data (unsigned-byte 32))
                          (values (unsigned-byte 33) (unsigned-byte 33)))
                composite-glyph-instruction-range))

(defun simple-glyph-instruction-range (fd pos contour-count)
  "The start and end of the instructions of the simple glyph at POS in the
font's byte array (FONT-DATA-BYTES of FD)."
  (let* ((instr-pos (+ pos (* 2 contour-count)))
         (instr-len (runes/otf:font-data-read-u16 fd instr-pos))
         (abs-start (+ (runes/otf:font-data-offset fd) instr-pos 2)))
    (values abs-start (+ abs-start instr-len))))

(defun composite-glyph-instruction-range (fd pos)
  "As SIMPLE-GLYPH-INSTRUCTION-RANGE, for the composite glyph at POS; 0 and 0
if it has no instructions."
  (let ((p pos)
        (have-instr nil))
    (loop
      (let ((flags (runes/otf:font-data-read-u16 fd p)))
        (when (logbitp 8 flags) (setf have-instr t))
        (incf p (+ 4
                   (if (logbitp 0 flags) 4 2)
                   (cond ((logbitp 3 flags) 2)
                         ((logbitp 6 flags) 4)
                         ((logbitp 7 flags) 8)
                         (t 0))))
        (unless (logbitp 5 flags) (return))))
    (if (not have-instr)
        (values 0 0)
        (let* ((instr-len (runes/otf:font-data-read-u16 fd p))
               (abs-start (+ (runes/otf:font-data-offset fd) p 2)))
          (values abs-start (+ abs-start instr-len))))))

(declaim (inline scale-funit round-26-6))
(defun scale-funit (u scale)
  (declare (type (signed-byte 32) u scale))
  (runes/otf/tt-hint::mul-fix u scale))
(defun round-26-6 (x)
  (declare (type (signed-byte 32) x))
  (logand (+ x 32) -64))

(defmacro with-storage ((data vector) &body body)
  "Run BODY with DATA the simple vector that holds the elements of the
adjustable, non-displaced VECTOR (an OUTLINE-POINTS vector): its elements
are read and written without the checks AREF on VECTOR makes.  DATA is only
good until VECTOR grows."
  (let ((start (gensym "START")) (end (gensym "END")))
    `(sb-kernel:with-array-data ((,data ,vector) (,start) (,end))
       (declare (ignore ,end) (simple-vector ,data) (fixnum ,start))
       (unless (zerop ,start) (error "Displaced outline vector"))
       ,@body)))

(defun run-glyph-bytecode-in-zone (eng bytes start end)
  "Reset per-glyph state and run the instructions START to END of the byte array
BYTES on ENG's already-populated glyph zone."
  (let* ((default-gs (runes/otf/tt-hint::eng-default-gs eng))
         (gs (runes/otf/tt-hint:reset-graphics-state-per-glyph
              (runes/otf/tt-hint::eng-gs eng) default-gs)))
    ;; Restore post-prep CVT and Storage snapshots (in case a previous glyph
    ;; executed WCVTP/WCVTF/WS in its glyph program, as Skrifa does).
    (when (runes/otf/tt-hint::eng-tables-dirty eng)
      (replace (runes/otf/tt-hint::eng-cvt eng) (runes/otf/tt-hint::eng-default-cvt eng))
      (replace (runes/otf/tt-hint::eng-storage eng) (runes/otf/tt-hint::eng-default-storage eng))
      (setf (runes/otf/tt-hint::eng-tables-dirty eng) nil))
    (when (and (> end start)
               (not (logbitp 0 (runes/otf/tt-hint::gs-instruct-control gs))))
      (setf (svref (runes/otf/tt-hint::eng-programs eng) 2) bytes)
      (runes/otf/tt-hint::vs-clear (runes/otf/tt-hint::eng-stack eng))
      (runes/otf/tt-hint:run-program eng 2 start end))))

(declaim (ftype (function (runes/otf/tt-hint:tt-engine) outline-points) engine-outline)
         (ftype (function (runes/otf/tt-hint:tt-engine font-loader) simple-vector)
                engine-tables)
         (ftype (function (runes/otf/tt-hint:tt-engine font-loader (unsigned-byte 32))
                          (values (unsigned-byte 32) (signed-byte 33)
                                  (or null (unsigned-byte 32))))
                engine-glyph-range))

(defun engine-outline (eng)
  (declare (type runes/otf/tt-hint:tt-engine eng))
  "ENG's reusable outline buffer, emptied.  Use it only while ENG is held, and
copy what is wanted out of it."
  (let ((out (runes/otf/tt-hint::eng-scratch eng)))
    (cond (out
           (setf (fill-pointer (op-xs out)) 0
                 (fill-pointer (op-ys out)) 0
                 (fill-pointer (op-on out)) 0
                 (fill-pointer (op-ends out)) 0)
           out)
          (t (setf (runes/otf/tt-hint::eng-scratch eng) (make-outline-points))))))

(defun engine-tables (eng loader)
  "The #(GLYF-OFFSET LOCA-OFFSET SHORT-P) of LOADER, found once per engine;
GLYF-OFFSET is NIL when the font has no 'glyf', LOCA-OFFSET when no 'loca'."
  (or (runes/otf/tt-hint::eng-tables eng)
      (setf (runes/otf/tt-hint::eng-tables eng)
            (let ((glyf (table-info "glyf" loader))
                  (loca (table-info "loca" loader)))
              (vector (and glyf (offset glyf))
                      (and loca (offset loca))
                      (eql (loca-offset-format loader) :short))))))

(defun engine-glyph-range (eng loader gid)
  "Offset and length of glyph GID's data in 'glyf', as GLYPH-LOCA-INFO, and
the offset of 'glyf' (NIL if none)."
  (declare (type (unsigned-byte 32) gid))
  (let* ((tables (engine-tables eng loader))
         (glyf (svref tables 0))
         (loca (svref tables 1))
         (short (svref tables 2)))
    (if (null loca)
        (values 0 0 glyf)
        (let ((fd (font-data loader)))
          (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) loca))
          (if short
              (let ((off1 (* (runes/otf:font-data-read-u16 fd (+ loca (* gid 2))) 2))
                    (off2 (* (runes/otf:font-data-read-u16 fd (+ loca (* (1+ gid) 2))) 2)))
                (values off1 (- off2 off1) glyf))
              (let ((off1 (runes/otf:font-data-read-u32 fd (+ loca (* gid 4))))
                    (off2 (runes/otf:font-data-read-u32 fd (+ loca (* (1+ gid) 4)))))
                (values off1 (- off2 off1) glyf)))))))

(declaim (ftype (function (font-loader runes/otf/tt-hint:tt-engine (unsigned-byte 32)
                           outline-points simple-vector)
                          phantom-array)
                load-hinted-glyph-points))

(defun load-hinted-glyph-points (loader eng gid out coords)
  (declare (type font-loader loader) (type runes/otf/tt-hint:tt-engine eng)
           (type (unsigned-byte 32) gid) (type outline-points out)
           (type simple-vector coords))
  (when (> (length (the list *compound-glyph-stack*)) (the fixnum *max-compound-glyph-depth*))
    (error 'unsupported-value :description "Compound glyph nesting too deep"
                              :location (format nil "glyph ~D" gid)
                              :actual-value (length *compound-glyph-stack*)
                              :expected-values (list *max-compound-glyph-depth*)))
  (when (member gid *compound-glyph-stack*)
    (error 'unsupported-value :description "Recursive compound glyph"
                              :location (format nil "glyph ~D" (first *compound-glyph-stack*))
                              :actual-value gid
                              :expected-values '("a glyph that does not contain itself")))
  (multiple-value-bind (g-off len glyf) (engine-glyph-range eng loader gid)
    (let ((fd (font-data loader))
          (scale (runes/otf/tt-hint::eng-scale eng)))
      (if (or (zerop len) (null glyf))
          (let ((phantom (phantom-array loader gid 0 0))
                (data (and (plusp (length coords)) (gvar-glyph-data loader gid)))
                (result (make-array 8 :element-type '(signed-byte 32))))
            (declare (type phantom-array phantom result))
            (when data
              (multiple-value-bind (dx dy)
                  (simple-glyph-deltas data coords (phantom-column phantom 0) (phantom-column phantom 1) '())
                (when dx
                  ;; Only the horizontal phantom points move.
                  (let ((moved (add-phantom-array-deltas phantom dx dy 0)))
                    (setf phantom (copy-seq phantom))
                    (replace phantom moved :end1 4 :end2 4)))))
            (dotimes (i 8 result)
              (setf (aref result i) (round-26-6 (scale-funit (aref phantom i) scale)))))
          (let* ((pos (+ glyf g-off))
                 (contour-count (runes/otf:font-data-read-i16 fd pos))
                 (phantom (phantom-array loader gid
                                         (runes/otf:font-data-read-i16 fd (+ pos 2))
                                         (runes/otf:font-data-read-i16 fd (+ pos 8)))))
            (if (minusp contour-count)
                (let ((*compound-glyph-stack* (cons gid *compound-glyph-stack*)))
                  (load-hinted-composite-points loader eng gid (+ pos 10) phantom out coords))
                (load-hinted-simple-points loader eng gid fd (+ pos 10) contour-count phantom out coords)))))))

(defun append-hinted-points (out n cx cy zf)
  "Append the first N points of the zone arrays CX, CY and ZF to OUT, in one
go: the vectors of OUT grow once, and are written without the checks that
VECTOR-PUSH-EXTEND and AREF on an adjustable vector make for each element."
  (declare (fixnum n) (type (simple-array (signed-byte 32) (*)) cx cy)
           (type (simple-array (unsigned-byte 8) (*)) zf)
           (optimize (speed 3) (safety 0)))
  (flet ((grow (v)
           (let ((need (+ (fill-pointer v) n)))
             (when (> need (array-dimension v 0))
               (adjust-array v (max need (* 2 (array-dimension v 0)))))
             (setf (fill-pointer v) need)
             (- need n))))
    (declare (inline grow))
    (let ((bx (grow (op-xs out))) (by (grow (op-ys out))) (bo (grow (op-on out))))
      (declare (fixnum bx by bo))
      (sb-kernel:with-array-data ((dx (op-xs out)) (sx) (ex)) 
        (declare (ignore ex) (simple-vector dx) (fixnum sx))
        (dotimes (i n) (setf (svref dx (+ sx bx i)) (aref cx i))))
      (sb-kernel:with-array-data ((dy (op-ys out)) (sy) (ey))
        (declare (ignore ey) (simple-vector dy) (fixnum sy))
        (dotimes (i n) (setf (svref dy (+ sy by i)) (aref cy i))))
      (sb-kernel:with-array-data ((do (op-on out)) (so) (eo))
        (declare (ignore eo) (simple-vector do) (fixnum so))
        (dotimes (i n)
          (setf (svref do (+ so bo i))
                (logtest (aref zf i) runes/otf/tt-hint:+point-on-curve+)))))))

(defun decode-simple-glyph-into-zone (fd pos contour-count z n scale)
  "Read the simple glyph at POS (contour count CONTOUR-COUNT, N points) into
the glyph zone Z: contour ends, the unscaled points, and those scaled by
SCALE as both original and current positions, with the on-curve flags.  As
READ-SIMPLE-POINTS, but straight into the zone's arrays."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) pos)
           (type (unsigned-byte 16) contour-count) (type runes/otf/tt-hint:tt-zone z)
           (type (unsigned-byte 17) n) (type (signed-byte 32) scale)
           (optimize (speed 3) (safety 1)))
  (let ((ux (runes/otf/tt-hint:zone-unscaled-x z))
        (uy (runes/otf/tt-hint:zone-unscaled-y z))
        (ox (runes/otf/tt-hint:zone-orig-x z))
        (oy (runes/otf/tt-hint:zone-orig-y z))
        (cx (runes/otf/tt-hint:zone-cur-x z))
        (cy (runes/otf/tt-hint:zone-cur-y z))
        (zf (runes/otf/tt-hint:zone-flags z))
        (zc (runes/otf/tt-hint:zone-contours z))
        (p pos))
    (declare (type (simple-array (signed-byte 32) (*)) ux uy ox oy cx cy)
             (type (simple-array (unsigned-byte 8) (*)) zf)
             (type (simple-array (unsigned-byte 16) (*)) zc)
             (type (unsigned-byte 32) p))
    (dotimes (c contour-count)
      (setf (aref zc c) (runes/otf:font-data-read-u16 fd (+ pos (* 2 c)))))
    (incf p (* 2 contour-count))
    ;; Skip the instructions.
    (incf p (+ 2 (the (unsigned-byte 16) (runes/otf:font-data-read-u16 fd p))))
    ;; The flags: one byte each, a repeat bit followed by a count.  A repeat
    ;; past the last point is dropped.
    (let ((i 0))
      (declare (type (unsigned-byte 17) i))
      (loop while (< i n)
            do (let ((flag (runes/otf:font-data-read-u8 fd p))
                     (repeats 0))
                 (declare (type (unsigned-byte 8) flag repeats))
                 (incf p)
                 (when (logbitp 3 flag)
                   (setf repeats (runes/otf:font-data-read-u8 fd p))
                   (incf p))
                 (loop repeat (min (1+ repeats) (- n i))
                       do (setf (aref zf i) flag)
                          (incf i)))))
    ;; The coordinates: each a byte (sign from SAME-BIT), the previous value
    ;; (SAME-BIT without SHORT-BIT), or a word.
    (macrolet ((coordinates (out short-bit same-bit)
                 `(let ((current 0))
                    (declare (type (signed-byte 32) current))
                    (dotimes (j n)
                      (let ((flag (aref zf j)))
                        (setf current
                              (+ current
                                 (cond ((logbitp ,short-bit flag)
                                        (let ((v (runes/otf:font-data-read-u8 fd p)))
                                          (incf p)
                                          (if (logbitp ,same-bit flag) v (- v))))
                                       ((logbitp ,same-bit flag) 0)
                                       (t (prog1 (runes/otf:font-data-read-i16 fd p)
                                            (incf p 2))))))
                        (setf (aref ,out j) current))))))
      (coordinates ux 1 4)
      (coordinates uy 2 5))
    (dotimes (i n)
      (let ((sx (scale-funit (aref ux i) scale))
            (sy (scale-funit (aref uy i) scale)))
        (setf (aref ox i) sx (aref oy i) sy
              (aref cx i) sx (aref cy i) sy
              (aref zf i) (logand (aref zf i) runes/otf/tt-hint:+point-on-curve+))))
    z))

(defun fill-zone-from-vectors (z ends xs ys on n scale)
  "Fill the glyph zone Z from the vectors READ-SIMPLE-POINTS returns, with
variation deltas added to XS and YS, as DECODE-SIMPLE-GLYPH-INTO-ZONE does."
  (declare (type runes/otf/tt-hint:tt-zone z) (simple-vector ends xs ys on)
           (type (unsigned-byte 17) n) (type (signed-byte 32) scale))
  (let ((ux (runes/otf/tt-hint:zone-unscaled-x z))
        (uy (runes/otf/tt-hint:zone-unscaled-y z))
        (ox (runes/otf/tt-hint:zone-orig-x z))
        (oy (runes/otf/tt-hint:zone-orig-y z))
        (cx (runes/otf/tt-hint:zone-cur-x z))
        (cy (runes/otf/tt-hint:zone-cur-y z))
        (zf (runes/otf/tt-hint:zone-flags z))
        (zc (runes/otf/tt-hint:zone-contours z)))
    (declare (type (simple-array (signed-byte 32) (*)) ux uy ox oy cx cy)
             (type (simple-array (unsigned-byte 8) (*)) zf)
             (type (simple-array (unsigned-byte 16) (*)) zc))
    (dotimes (c (length ends))
      (setf (aref zc c) (svref ends c)))
    (dotimes (i n)
      (let ((sx (scale-funit (svref xs i) scale))
            (sy (scale-funit (svref ys i) scale)))
        (setf (aref ux i) (svref xs i) (aref uy i) (svref ys i)
              (aref ox i) sx          (aref oy i) sy
              (aref cx i) sx          (aref cy i) sy
              (aref zf i) (if (svref on i) runes/otf/tt-hint:+point-on-curve+ 0))))
    z))

(defun load-hinted-simple-points (loader eng gid fd pos contour-count phantom out coords)
  (declare (type font-loader loader) (type runes/otf/tt-hint:tt-engine eng)
           (type (unsigned-byte 32) gid) (type runes/otf:font-data fd)
           (type (unsigned-byte 32) pos) (type (unsigned-byte 16) contour-count)
           (type phantom-array phantom) (type simple-vector coords)
           (type outline-points out))
  (let* ((scale (runes/otf/tt-hint::eng-scale eng))
         (data (and (plusp (length coords)) (gvar-glyph-data loader gid)))
         (n 0)
         (z nil))
    (declare (type (signed-byte 32) scale) (type (unsigned-byte 17) n))
    (if data
        ;; A variable font's glyph: the deltas need the points as vectors.
        (multiple-value-bind (ends xs ys on) (read-simple-points fd pos contour-count)
          (declare (simple-vector ends xs ys on))
          (setf n (length xs))
          (let ((all-x (concatenate 'vector xs (phantom-column phantom 0)))
                (all-y (concatenate 'vector ys (phantom-column phantom 1))))
            (multiple-value-bind (dx dy) (simple-glyph-deltas data coords all-x all-y (coerce ends 'list))
              (when dx
                (dotimes (i n)
                  (setf (aref xs i) (+ (the fixnum (aref xs i)) (the fixnum (fixed-round (aref dx i))))
                        (aref ys i) (+ (the fixnum (aref ys i)) (the fixnum (fixed-round (aref dy i))))))
                (setf phantom (add-phantom-array-deltas phantom dx dy n)))))
          ;; Reuse the engine's glyph zone arena in place!
          (setf z (runes/otf/tt-hint:ensure-zone-capacity
                   (runes/otf/tt-hint::eng-glyph eng) (+ n 4) contour-count :n-glyph-points n))
          (fill-zone-from-vectors z ends xs ys on n scale))
        (progn
          (setf n (if (zerop contour-count)
                      0
                      (1+ (runes/otf:font-data-read-u16
                           fd (+ pos (* 2 (1- contour-count)))))))
          (setf z (runes/otf/tt-hint:ensure-zone-capacity
                   (runes/otf/tt-hint::eng-glyph eng) (+ n 4) contour-count :n-glyph-points n))
          (decode-simple-glyph-into-zone fd pos contour-count z n scale)))
    (let ((ux (runes/otf/tt-hint:zone-unscaled-x z))
          (uy (runes/otf/tt-hint:zone-unscaled-y z))
          (ox (runes/otf/tt-hint:zone-orig-x z))
          (oy (runes/otf/tt-hint:zone-orig-y z))
          (cx (runes/otf/tt-hint:zone-cur-x z))
          (cy (runes/otf/tt-hint:zone-cur-y z))
          (zf (runes/otf/tt-hint:zone-flags z))
          (zc (runes/otf/tt-hint:zone-contours z)))
      (declare (type (simple-array (signed-byte 32) (*)) ux uy ox oy cx cy)
               (type (simple-array (unsigned-byte 8) (*)) zf)
               (type (simple-array (unsigned-byte 16) (*)) zc))
      (dotimes (k 4)
        (let* ((idx (+ n k))
               (px (aref phantom (* 2 k)))
               (py (aref phantom (1+ (* 2 k))))
               (spx (scale-funit px scale))
               (spy (scale-funit py scale)))
          (setf (aref ux idx) px  (aref uy idx) py
                (aref ox idx) spx (aref oy idx) spy
                (aref cx idx) (round-26-6 spx)
                (aref cy idx) (round-26-6 spy)
                (aref zf idx) 0)))
      ;; The two modes differ here: out of backward compatibility mode the
      ;; program starts from rounded phantom points (and keeps what it makes
      ;; of them), in it from the same ones but leaves the originals as
      ;; scaled, which is what is returned (see SHIFT-OUTLINE-BY-PP1).
      (unless (runes/otf/tt-hint::gs-backward-compatibility
               (runes/otf/tt-hint::eng-default-gs eng))
        (dotimes (k 4)
          (assert (and (zerop (logand 63 (aref cx (+ n k))))
                       (zerop (logand 63 (aref cy (+ n k)))))
                  () "phantom point ~D is not rounded" k)))
      (multiple-value-bind (istart iend) (simple-glyph-instruction-range fd pos contour-count)
        (setf (runes/otf/tt-hint::eng-unscaled-scale eng) scale
              (runes/otf/tt-hint::eng-composite-p eng) nil)
        (run-glyph-bytecode-in-zone eng (runes/otf:font-data-bytes fd) istart iend))
      (let ((base (fill-pointer (op-xs out))))
        (append-hinted-points out n cx cy zf)
        (dotimes (c contour-count)
          (vector-push-extend (+ base (aref zc c)) (op-ends out))))
      ;; In backward compatibility mode the program does not change the
      ;; bearings and advance: the phantom points stay as scaled, not
      ;; rounded (FreeType, skrifa).
      (let ((bc (runes/otf/tt-hint::gs-backward-compatibility (runes/otf/tt-hint::eng-gs eng)))
            (result (make-array 8 :element-type '(signed-byte 32))))
        (dotimes (k 4 result)
          (setf (aref result (* 2 k)) (if bc (aref ox (+ n k)) (aref cx (+ n k)))
                (aref result (1+ (* 2 k))) (if bc (aref oy (+ n k)) (aref cy (+ n k)))))))))

(defun composite-component-count (fd pos)
  "The number of components of the composite glyph at POS."
  (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) pos))
  (let ((p pos) (n 0))
    (declare (type (unsigned-byte 32) p) (fixnum n))
    (loop
      (let ((flags (runes/otf:font-data-read-u16 fd p)))
        (incf n)
        (incf p (+ 4
                   (if (logbitp 0 flags) 4 2)
                   (cond ((logbitp 3 flags) 2)
                         ((logbitp 6 flags) 4)
                         ((logbitp 7 flags) 8)
                         (t 0))))
        (unless (logbitp 5 flags) (return n))))))

(defun load-hinted-composite-points (loader eng gid pos phantom out coords)
  (declare (type font-loader loader) (type runes/otf/tt-hint:tt-engine eng)
           (type (unsigned-byte 32) gid) (type (unsigned-byte 32) pos)
           (type phantom-array phantom) (type simple-vector coords)
           (type outline-points out))
  (let* ((fd (font-data loader))
         (scale (runes/otf/tt-hint::eng-scale eng))
         (component-count (composite-component-count fd pos))
         (count (+ component-count 4))
         (data (and (plusp (length coords)) (gvar-glyph-data loader gid)))
         (dx nil) (dy nil)
         (point-base (fill-pointer (op-xs out)))
         ;; A composite nested in another one starts after its contours.
         (contour-base (length (op-ends out)))
         (p pos))
    (declare (type (signed-byte 32) scale) (fixnum component-count count)
             (type (unsigned-byte 32) p))
    (when data
      (multiple-value-setq (dx dy) (composite-glyph-deltas data coords count))
      (when dx
        (setf phantom (add-phantom-array-deltas phantom dx dy (- count 4)))))
    (let ((scaled-phantom (make-array 8 :element-type '(signed-byte 32))))
      (declare (type phantom-array scaled-phantom))
      ;; Not rounded yet: the program's original positions are these, and
      ;; its current ones the rounded.
      (dotimes (i 8)
        (setf (aref scaled-phantom i) (scale-funit (aref phantom i) scale)))
      (dotimes (i component-count)
        (multiple-value-bind (flags component-gid arg1 arg2 xx yx xy-skew yy next)
            (component-fields fd p)
          (declare (type (unsigned-byte 16) flags component-gid) (fixnum arg1 arg2)
                   (type (unsigned-byte 32) next))
          (setf p next)
          (let* ((start (fill-pointer (op-xs out)))
                 (comp-phantom (load-hinted-glyph-points loader eng component-gid out coords))
                 (end (fill-pointer (op-xs out)))
                 (xs (op-xs out))
                 (ys (op-ys out))
                 (transformed (logtest flags #b11001000)))
            (declare (fixnum start end))
            (when (logbitp 9 flags)
              (setf scaled-phantom comp-phantom))
            (when transformed
              (transform-component-points xs ys start end (vector xx yx xy-skew yy)))
            (multiple-value-bind (ox oy)
                (if (logbitp 1 flags)
                    (let ((ux (+ arg1 (if (and dx (aref dx i)) (the fixnum (fixed-round (aref dx i))) 0)))
                          (uy (+ arg2 (if (and dy (aref dy i)) (the fixnum (fixed-round (aref dy i))) 0))))
                      (declare (type (signed-byte 32) ux uy))
                      (when (and transformed (= (logand flags #x1800) #x0800))
                        (setf ux (the (signed-byte 32) (f* ux (hypot-fixed xx xy-skew)))
                              uy (the (signed-byte 32) (f* uy (hypot-fixed yy yx)))))
                      (let ((sx (scale-funit ux scale))
                            (sy (scale-funit uy scale)))
                        ;; Rounded to the grid in y only: as FreeType's
                        ;; v40 interpreter (and skrifa) places a
                        ;; component horizontally at subpixel precision.
                        (if (logbitp 2 flags)
                            (values sx (round-26-6 sy))
                            (values sx sy))))
                    (let ((base (+ point-base arg1))
                          (child (+ start arg2)))
                      (declare (fixnum base child))
                      (unless (and (< base start) (< child end))
                        (error 'unsupported-value
                               :description "Compound glyph anchor point out of range"
                               :location (format nil "glyph ~D" gid)
                               :actual-value (list arg1 arg2)
                               :expected-values '("existing point indices")))
                      (values (- (the fixnum (aref xs base)) (the fixnum (aref xs child)))
                              (- (the fixnum (aref ys base)) (the fixnum (aref ys child))))))
              (locally (declare (type fixnum ox oy))
                (unless (and (zerop ox) (zerop oy))
                  (with-storage (dxs xs)
                    (with-storage (dys ys)
                      (loop for j of-type fixnum from start below end
                            do (setf (svref dxs j) (+ (the fixnum (svref dxs j)) ox)
                                     (svref dys j) (+ (the fixnum (svref dys j)) oy)))))))))))
      (multiple-value-bind (istart iend) (composite-glyph-instruction-range fd pos)
        (when (and (= iend istart)
                   (not (runes/otf/tt-hint::gs-backward-compatibility
                         (runes/otf/tt-hint::eng-default-gs eng))))
          ;; No program to move them: the phantom points are rounded, as
          ;; they are before one runs.
          (dotimes (i 8)
            (setf (aref scaled-phantom i) (round-26-6 (aref scaled-phantom i)))))
        (when (> iend istart)
          (let* ((n (- (fill-pointer (op-xs out)) point-base))
                 (total (+ n 4))
                 (n-contours (- (length (op-ends out)) contour-base))
                 (z (runes/otf/tt-hint:ensure-zone-capacity
                     (runes/otf/tt-hint::eng-glyph eng) total n-contours :n-glyph-points n))
                 (ux (runes/otf/tt-hint:zone-unscaled-x z))
                 (uy (runes/otf/tt-hint:zone-unscaled-y z))
                 (ox (runes/otf/tt-hint:zone-orig-x z))
                 (oy (runes/otf/tt-hint:zone-orig-y z))
                 (cx (runes/otf/tt-hint:zone-cur-x z))
                 (cy (runes/otf/tt-hint:zone-cur-y z))
                 (zf (runes/otf/tt-hint:zone-flags z))
                 (zc (runes/otf/tt-hint:zone-contours z)))
            (declare (type (simple-array (signed-byte 32) (*)) ux uy ox oy cx cy)
                     (type (simple-array (unsigned-byte 8) (*)) zf)
                     (type (simple-array (unsigned-byte 16) (*)) zc)
                     (fixnum n total n-contours))
            (dotimes (c n-contours)
              (setf (aref zc c) (- (the fixnum (aref (op-ends out) (+ contour-base c))) point-base)))
            (with-storage (dxs (op-xs out))
              (with-storage (dys (op-ys out))
                (with-storage (don (op-on out))
                  (dotimes (k n)
                    (let ((vx (the (signed-byte 32) (svref dxs (+ point-base k))))
                          (vy (the (signed-byte 32) (svref dys (+ point-base k)))))
                      (setf (aref ux k) vx (aref uy k) vy
                            (aref ox k) vx (aref oy k) vy
                            (aref cx k) vx (aref cy k) vy
                            (aref zf k) (if (svref don (+ point-base k))
                                            runes/otf/tt-hint:+point-on-curve+
                                            0)))))))
            (dotimes (k 4)
              (let ((idx (+ n k))
                    (px (aref scaled-phantom (* 2 k)))
                    (py (aref scaled-phantom (1+ (* 2 k)))))
                (setf (aref ux idx) px (aref uy idx) py
                      (aref ox idx) px (aref oy idx) py
                      (aref cx idx) (round-26-6 px) (aref cy idx) (round-26-6 py)
                      (aref zf idx) 0)))
            ;; The points of a composite glyph are scaled already.
            (setf (runes/otf/tt-hint::eng-unscaled-scale eng) #x10000
                  (runes/otf/tt-hint::eng-composite-p eng) t)
            (run-glyph-bytecode-in-zone eng (runes/otf:font-data-bytes fd) istart iend)
            (with-storage (dxs (op-xs out))
              (with-storage (dys (op-ys out))
                (with-storage (don (op-on out))
                  (dotimes (k n)
                    (setf (svref dxs (+ point-base k)) (aref cx k)
                          (svref dys (+ point-base k)) (aref cy k)
                          (svref don (+ point-base k))
                          (logtest (aref zf k) runes/otf/tt-hint:+point-on-curve+))))))
            ;; As for a simple glyph, the program leaves them alone in backward
            ;; compatibility mode.
            (unless (runes/otf/tt-hint::gs-backward-compatibility (runes/otf/tt-hint::eng-gs eng))
              (dotimes (k 4)
                (setf (aref scaled-phantom (* 2 k)) (aref cx (+ n k))
                      (aref scaled-phantom (1+ (* 2 k))) (aref cy (+ n k))))))))
      scaled-phantom)))

;;; ============================================================================
;;; 4. Public High-Level & Test Harness APIs
;;; ============================================================================

(defun copy-growable-s32 (vector)
  "A (SIGNED-BYTE 32) array of the elements of the adjustable VECTOR."
  (declare (type (and vector (not simple-array)) vector))
  (let ((result (make-array (length vector) :element-type '(signed-byte 32))))
    (with-storage (data vector)
      (dotimes (i (length result) result)
        (setf (aref result i) (the (signed-byte 32) (svref data i)))))))

(defun copy-growable-flags (vector)
  "An (UNSIGNED-BYTE 8) array with 1 for each true element of the adjustable
VECTOR and 0 for each false one."
  (declare (type (and vector (not simple-array)) vector))
  (let ((result (make-array (length vector) :element-type '(unsigned-byte 8))))
    (with-storage (data vector)
      (dotimes (i (length result) result)
        (setf (aref result i) (if (svref data i) 1 0))))))

(defun shift-outline-by-pp1 (xs phantom)
  "Move the x coordinates XS (26.6 pixels) of a hinted outline so that its
first phantom point, in PHANTOM, is at 0, as FreeType does.

Every consumer that returns the coordinates of a hinted glyph has to do this:
in backward compatibility mode the phantom points are not rounded, so
pp1.x is usually not 0 (xMin - lsb is not 0 for some glyphs) and the points
as loaded are off by it.  The hinted advance does not need it, as it is the
difference of two phantom points."
  (declare (type (and vector (not simple-array)) xs) (type phantom-array phantom))
  (let ((pp1x (aref phantom 0)))
    (unless (zerop pp1x)
      (with-storage (data xs)
        (dotimes (i (length xs))
          (setf (svref data i) (- (the fixnum (svref data i)) pp1x))))))
  xs)

(defun hint-outline-26dot6 (loader gid ppem &key coords)
  "Hint glyph GID of LOADER at PPEM and return raw F26Dot6 coordinates for
point-by-point comparison against FreeType's FT_Outline:
  (values xs ys on-curve contour-ends phantom-points)
the first, second and fourth as (SIGNED-BYTE 32) arrays, ON-CURVE as an
(UNSIGNED-BYTE 8) array with 1 for on-curve, and the phantom points as
X0 Y0 X1 Y1 X2 Y2 X3 Y3 in an array of eight."
  (let ((coords-vec (if coords
                        (map 'vector #'runes/otf/charstring::f2dot14-from-real
                             (normalize-coordinates loader coords))
                        (or (loader-gvar-coords loader) #()))))
    (with-pooled-tt-engine (eng loader ppem coords-vec)
      (let* ((out (engine-outline eng))
             (phantom (load-hinted-glyph-points loader eng gid out coords-vec)))
        (shift-outline-by-pp1 (op-xs out) phantom)
        (values (copy-growable-s32 (op-xs out))
                (copy-growable-s32 (op-ys out))
                (copy-growable-flags (op-on out))
                (copy-growable-s32 (op-ends out))
                phantom)))))

(defun f26dot6->pixels (v)
  "V, in 26.6 fixed point, in pixels: an integer for a whole number of them,
else a single-float.  Dividing a single-float by 64 is exact, so this is the
value (/ V 64) has, without making a ratio."
  (declare (type (signed-byte 32) v))
  (if (zerop (logand v 63))
      (ash v -6)
      (* (float v 1.0f0) 0.015625f0)))

(defun cff-hinted-outline (loader gid ppem)
  (let* ((font (loader-cff-font loader))
         (index (or (runes/otf/charstring:cff-subfont-index font gid)
                    (error 'unsupported-value :description "Glyph not covered by FDSelect"
                                              :actual-value gid :expected-values '("a covered glyph"))))
         (subfont (loader-cff-subfont loader index))
         (state (runes/otf/charstring::cff-hint-state font subfont ppem))
         (sink (make-instance 'outline-sink)))
    (runes/otf/charstring::cff-draw-hinted-glyph font subfont gid sink state (loader-coords loader))
    (map 'vector #'copy-seq (reverse (outline-contours sink)))))

(defgeneric hinted-contours (glyph ppem)
  (:documentation "Return the grid-fitted contours of GLYPH at PPEM pixels per em,
in pixel coordinates (where 1.0 is one pixel)."))

(defmethod hinted-contours ((glyph glyph) ppem)
  (let ((loader (font-loader glyph))
        (gid (font-index glyph)))
    (if (cff-outlines-p loader)
        (let ((*cubic-tolerance* (/ *cubic-tolerance* (/ (units/em loader) ppem))))
          (outline->contours (cff-hinted-outline loader gid ppem)))
        (let ((coords (or (loader-gvar-coords loader) #())))
          (with-pooled-tt-engine (eng loader ppem coords)
            (let ((out (engine-outline eng)))
              (shift-outline-by-pp1 (op-xs out) (load-hinted-glyph-points loader eng gid out coords))
              (dotimes (i (length (op-xs out)))
                (setf (aref (op-xs out) i) (f26dot6->pixels (aref (op-xs out) i))
                      (aref (op-ys out) i) (f26dot6->pixels (aref (op-ys out) i))))
              (outline-points->contours out)))))))

(defgeneric hinted-cubic-contours (glyph ppem)
  (:documentation "Return the exact grid-fitted outline of GLYPH at PPEM pixels per em
as a vector of contours of (:move x y), (:line x y), and (:curve cx0 cy0 cx1 cy1 x y)
commands in pixel coordinates."))

(defmethod hinted-cubic-contours ((glyph glyph) ppem)
  (let ((loader (font-loader glyph)))
    (if (cff-outlines-p loader)
        (cff-hinted-outline loader (font-index glyph) ppem)
        (map 'vector #'quadratic-contour->cubic (hinted-contours glyph ppem)))))

(defgeneric hinted-advance-width (glyph ppem)
  (:documentation "Return the hinted advance width of GLYPH at PPEM pixels per em,
in whole pixels, from its hinted phantom points."))

(defmethod hinted-advance-width ((glyph glyph) ppem)
  (let ((loader (font-loader glyph))
        (gid (font-index glyph)))
    (if (cff-outlines-p loader)
        (float (* (advance-width glyph) (/ ppem (units/em loader))) 1.0)
        (let ((coords (or (loader-gvar-coords loader) #())))
          (with-pooled-tt-engine (eng loader ppem coords)
            (let ((phantom (load-hinted-glyph-points loader eng gid (engine-outline eng) coords)))
              ;; Whole pixels, as FreeType's hinted advance.
              (f26dot6->pixels (round-26-6 (- (aref phantom 2) (aref phantom 0))))))))))
