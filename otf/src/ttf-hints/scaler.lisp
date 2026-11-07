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
          hint-outline-26dot6
          hint-outline-26dot6-into
          outline-26dot6 make-outline-26dot6
          o26-xs o26-ys o26-on-curve o26-ends o26-phantom o26-n o26-n-ends))

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
        ;; The copy shares the round state; each needs its own, as a glyph's
        ;; program changes the one in the engine's state.
        (setf (runes/otf/tt-hint::gs-round-state (runes/otf/tt-hint::eng-default-gs eng))
              (copy-structure (runes/otf/tt-hint::gs-round-state
                               (runes/otf/tt-hint::eng-default-gs eng))))
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

(declaim (sb-ext:freeze-type pool-entry))

(sb-ext:defglobal **tt-last-pool-entry** nil
  "(WEAK-POINTER-TO-LOADER PPEM-KEY COORDS . POOL-ENTRY) of the last
ACQUIRE-TT-ENGINE.  A cache only: a stale or racing read is harmless.")

(defvar *tt-pool-tick* 0)

(declaim (inline coords-equal))
(defun coords-equal (a b)
  "True if the coordinate vectors A and B hold the same values."
  (declare (type simple-vector a b))
  (and (= (length a) (length b))
       (dotimes (i (length a) t)
         (unless (eql (svref a i) (svref b i)) (return nil)))))

(defun find-tt-pool-entry (loader ppem-key coords)
  "The pool entry for LOADER at PPEM-KEY and COORDS; made if there is none,
dropping the least recently used ones if the font has too many."
  (declare (type fixnum ppem-key))
  (sb-thread:with-mutex (*tt-engine-pool-lock*)
    (let* ((entries (gethash loader *tt-engine-pools*))
           (entry (find-if (lambda (e)
                             (and (= (pe-ppem-key e) ppem-key)
                                  (coords-equal (pe-coords e) coords)))
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
                    (coords-equal (caddr last) coords))
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

(declaim (inline scale-funit scale-unrounded round-26-6))
(defun scale-funit (u scale)
  (declare (type (signed-byte 32) u scale))
  (runes/otf/tt-hint::mul-fix u scale))
(defun scale-unrounded (funits delta scale)
  "FUNITS font units moved by the 16.16 DELTA, scaled by SCALE to 26.6 the way
FreeType scales the points of a variation instance."
  (declare (type (signed-byte 32) funits delta scale))
  (let ((u (+ (* 64 funits) (ash (+ delta #x200) -10))))
    (declare (type (signed-byte 40) u))
    (ash (+ (runes/otf/tt-hint::mul-fix (runes/otf/tt-hint::s32 u) scale) 32) -6)))
(defun round-26-6 (x)
  (declare (type (signed-byte 32) x))
  (logand (+ x 32) -64))

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

(defstruct (tt-tables (:conc-name tt-))
  "What the per-glyph path reads from a font loader, found once per engine:
the loader's accessors are generic functions, and a glyph asks for ten."
  (font-data nil :type runes/otf:font-data)
  (glyf-offset nil :type (or null (unsigned-byte 32)))
  (loca-offset nil :type (or null (unsigned-byte 32)))
  (short-loca-p nil :type boolean)
  (lsb nil :type (or null simple-vector))
  (advance-widths nil :type (or null simple-vector))
  (top-side-bearings nil :type (or null simple-vector))
  (advance-heights nil :type (or null simple-vector))
  (vmtx-missing-p nil :type boolean)
  (ascender 0 :type fixnum)
  (descender 0 :type fixnum))

(declaim (sb-ext:freeze-type tt-tables))

(defstruct (hint-buffer (:conc-name hb-))
  "The points of a hinted outline as it is loaded: x, y and flag arrays with
N points in them, the first flag bit being the on-curve bit, and the end
point of each of the N-ENDS contours.  One is kept for each engine."
  (xs (make-array 128 :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (ys (make-array 128 :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (flags (make-array 128 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (ends (make-array 32 :element-type '(signed-byte 32))
   :type (simple-array (signed-byte 32) (*)))
  (n 0 :type fixnum)
  (n-ends 0 :type fixnum))

(declaim (sb-ext:freeze-type hint-buffer))

(defun hb-reserve (buffer points ends)
  "Make room in BUFFER for POINTS more points and ENDS more contours."
  (declare (type hint-buffer buffer) (fixnum points ends))
  (let ((need-points (+ (hb-n buffer) points))
        (need-ends (+ (hb-n-ends buffer) ends)))
    (when (> need-points (length (hb-xs buffer)))
      (let ((capacity (max need-points (* 2 (length (hb-xs buffer))))))
        (flet ((grown (old element-type)
                 (let ((new (make-array capacity :element-type element-type)))
                   (replace new old :end2 (hb-n buffer))
                   new)))
          (setf (hb-xs buffer) (grown (hb-xs buffer) '(signed-byte 32))
                (hb-ys buffer) (grown (hb-ys buffer) '(signed-byte 32))
                (hb-flags buffer) (grown (hb-flags buffer) '(unsigned-byte 8))))))
    (when (> need-ends (length (hb-ends buffer)))
      (let ((new (make-array (max need-ends (* 2 (length (hb-ends buffer))))
                             :element-type '(signed-byte 32))))
        (replace new (hb-ends buffer) :end2 (hb-n-ends buffer))
        (setf (hb-ends buffer) new)))
    buffer))

(declaim (ftype (function (runes/otf/tt-hint:tt-engine) hint-buffer) engine-outline)
         (ftype (function (runes/otf/tt-hint:tt-engine font-loader) tt-tables)
                engine-tables)
         (ftype (function (runes/otf/tt-hint:tt-engine font-loader (unsigned-byte 32))
                          (values (unsigned-byte 32) (signed-byte 33)
                                  (or null (unsigned-byte 32))))
                engine-glyph-range))

(defun engine-outline (eng)
  "ENG's reusable outline buffer, emptied.  Use it only while ENG is held, and
copy what is wanted out of it."
  (declare (type runes/otf/tt-hint:tt-engine eng))
  (let ((buffer (runes/otf/tt-hint::eng-scratch eng)))
    (cond (buffer
           (setf (hb-n buffer) 0
                 (hb-n-ends buffer) 0)
           buffer)
          (t (setf (runes/otf/tt-hint::eng-scratch eng) (make-hint-buffer))))))

(declaim (inline engine-tables))
(defun engine-tables (eng loader)
  "The TT-TABLES of LOADER, found once per engine."
  (declare (type runes/otf/tt-hint:tt-engine eng) (type font-loader loader))
  (or (runes/otf/tt-hint::eng-tables eng)
      (setf (runes/otf/tt-hint::eng-tables eng) (make-engine-tables loader))))

(defun make-engine-tables (loader)
  (let ((glyf (table-info "glyf" loader))
        (loca (table-info "loca" loader)))
    (make-tt-tables :font-data (font-data loader)
                    :glyf-offset (and glyf (offset glyf))
                    :loca-offset (and loca (offset loca))
                    :short-loca-p (eql (loca-offset-format loader) :short)
                    :lsb (left-side-bearings loader)
                    :advance-widths (advance-widths loader)
                    :top-side-bearings (and (not (vmtx-missing-p loader))
                                            (top-side-bearings loader))
                    :advance-heights (and (not (vmtx-missing-p loader))
                                          (advance-heights loader))
                    :vmtx-missing-p (and (vmtx-missing-p loader) t)
                    :ascender (slot-value loader 'ascender)
                    :descender (slot-value loader 'descender))))

(defun tables-phantom-array (tables gid x-min y-max)
  "PHANTOM-ARRAY from the cached TABLES."
  (declare (type tt-tables tables) (type (unsigned-byte 32) gid)
           (type (signed-byte 32) x-min y-max))
  (let* ((lsb (bounded-aref (tt-lsb tables) gid))
         (advance (bounded-aref (tt-advance-widths tables) gid))
         (ascent (tt-ascender tables))
         (descent (tt-descender tables))
         (result (make-array 8 :element-type '(signed-byte 32))))
    (declare (fixnum lsb advance ascent descent))
    (multiple-value-bind (tsb vadvance)
        (if (tt-vmtx-missing-p tables)
            (values (- ascent y-max) (- ascent descent))
            (values (bounded-aref (tt-top-side-bearings tables) gid)
                    (bounded-aref (tt-advance-heights tables) gid)))
      (declare (fixnum tsb vadvance))
      (let ((pp1x (- x-min lsb)))
        (setf (aref result 0) pp1x
              (aref result 1) 0
              (aref result 2) (+ pp1x advance)
              (aref result 3) 0
              (aref result 4) 0
              (aref result 5) (+ y-max tsb)
              (aref result 6) 0
              (aref result 7) (- (+ y-max tsb) vadvance))))
    result))

(defun engine-glyph-range (eng loader gid)
  "Offset and length of glyph GID's data in 'glyf', as GLYPH-LOCA-INFO, and
the offset of 'glyf' (NIL if none)."
  ;; The reads of 'loca' check themselves; the declared result type is not
  ;; checked again on the way out.
  (declare (type (unsigned-byte 32) gid) (optimize (safety 0)))
  ;; A glyph count is 16 bits: so the offsets below cannot overflow.
  (unless (< gid 65536)
    (error 'unsupported-value :description "Glyph index out of range"
                              :actual-value gid :expected-values '("a glyph index below 65536")))
  (let* ((tables (engine-tables eng loader))
         (glyf (tt-glyf-offset tables))
         (loca (tt-loca-offset tables)))
    (if (null loca)
        (values 0 0 glyf)
        (let ((fd (tt-font-data tables)))
          (declare (type runes/otf:font-data fd) (type (unsigned-byte 32) loca))
          (if (tt-short-loca-p tables)
              (let ((off1 (* (runes/otf:font-data-read-u16 fd (+ loca (* gid 2))) 2))
                    (off2 (* (runes/otf:font-data-read-u16 fd (+ loca (* (1+ gid) 2))) 2)))
                (values off1 (- off2 off1) glyf))
              (let ((off1 (runes/otf:font-data-read-u32 fd (+ loca (* gid 4))))
                    (off2 (runes/otf:font-data-read-u32 fd (+ loca (* (1+ gid) 4)))))
                (values off1 (- off2 off1) glyf)))))))

(declaim (ftype (function (font-loader runes/otf/tt-hint:tt-engine (unsigned-byte 32)
                           hint-buffer simple-vector)
                          phantom-array)
                load-hinted-glyph-points))

(defun load-hinted-glyph-points (loader eng gid out coords)
  (declare (type font-loader loader) (type runes/otf/tt-hint:tt-engine eng)
           (type (unsigned-byte 32) gid) (type hint-buffer out)
           (type simple-vector coords))
  (multiple-value-bind (g-off len glyf) (engine-glyph-range eng loader gid)
    (let* ((tables (engine-tables eng loader))
           (fd (tt-font-data tables))
           (scale (runes/otf/tt-hint::eng-scale eng)))
      (if (or (zerop len) (null glyf))
          (let ((phantom (tables-phantom-array tables gid 0 0))
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
                 (phantom (tables-phantom-array tables gid
                                                (runes/otf:font-data-read-i16 fd (+ pos 2))
                                                (runes/otf:font-data-read-i16 fd (+ pos 8)))))
            (if (minusp contour-count)
                ;; Only a composite glyph can contain itself, or nest too deep.
                (let ((*compound-glyph-stack* (cons gid *compound-glyph-stack*)))
                  (when (> (length (the list *compound-glyph-stack*)) (the fixnum *max-compound-glyph-depth*))
                    (error 'unsupported-value :description "Compound glyph nesting too deep"
                                              :location (format nil "glyph ~D" gid)
                                              :actual-value (length *compound-glyph-stack*)
                                              :expected-values (list *max-compound-glyph-depth*)))
                  (when (member gid (the list (rest *compound-glyph-stack*)))
                    (error 'unsupported-value :description "Recursive compound glyph"
                                              :location (format nil "glyph ~D" (first (rest *compound-glyph-stack*)))
                                              :actual-value gid
                                              :expected-values '("a glyph that does not contain itself")))
                  (load-hinted-composite-points loader eng gid (+ pos 10) phantom out coords))
                (load-hinted-simple-points loader eng gid fd (+ pos 10) contour-count phantom out coords)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun glyph-byte-form (checked at)
    "The form that reads the byte at AT of the font data FD, checked or not."
    (if checked
        `(runes/otf:font-data-read-u8 fd ,at)
        `(aref bytes (+ base ,at))))

  (defun glyph-word-form (checked at)
    "The form that reads the signed 16-bit word at AT of the font data FD."
    (if checked
        `(runes/otf:font-data-read-i16 fd ,at)
        `(let ((u (logior (ash (aref bytes (+ base ,at)) 8)
                          (aref bytes (+ base ,at 1)))))
           (if (logbitp 15 u) (- u 65536) u))))

  (defun glyph-coordinates-form (checked out scaled-1 scaled-2 short-bit same-bit mask-flags)
    "The form that reads one axis of the coordinates into the array OUT, and
scaled into SCALED-1 and SCALED-2: each a byte (sign from SAME-BIT), the
previous value (SAME-BIT without SHORT-BIT), or a word.  With MASK-FLAGS, the
flags are cut to the on-curve bit, which nothing after the y pass reads
beyond."
    `(let ((current 0)
           (start 0))
       (declare (type (signed-byte 32) current) (type (unsigned-byte 17) start))
       ,@(when (not checked)
           ;; Sixteen points at a time with AVX-512, while 64 bytes of the font
           ;; data are there to be loaded; the points after go one by one.
           `((when vdecode
               (loop while (and (<= (+ start 16) n) (<= (+ base p 64) (length bytes)))
                     do (decode-axis-block bytes (+ base p) zf start ,out current cell
                                           ,short-bit ,same-bit ,mask-flags)
                        (setf current (aref ,out (+ start 15)))
                        (incf p (aref cell 15))
                        (incf start 16)))))
       (loop for j of-type (unsigned-byte 17) from start below n
             do
         (let ((flag (locally (declare (optimize (safety 0))) (aref zf j))))
           (setf current
                 (+ current
                    (cond ((logbitp ,short-bit flag)
                           (let ((v ,(glyph-byte-form checked 'p)))
                             (incf p)
                             (if (logbitp ,same-bit flag) v (- v))))
                          ((logbitp ,same-bit flag) 0)
                          (t (prog1 ,(glyph-word-form checked 'p)
                               (incf p 2))))))
           (locally (declare (optimize (safety 0)))
             (setf (aref ,out j) current)
             ;; With AVX-512 the scaling is done afterwards, sixteen at a time.
             (unless vdecode
               (let ((scaled (scale-funit current scale)))
                 (setf (aref ,scaled-1 j) scaled
                       (aref ,scaled-2 j) scaled)))
             ,@(when mask-flags
                 `((setf (aref zf j)
                         (logand flag runes/otf/tt-hint:+point-on-curve+))))))))))

(defmacro decode-glyph-flags-and-coordinates (checked)
  "Read the flags and the coordinates of a simple glyph into ZF, UX, UY and
the scaled arrays, with P at the flags; CHECKED reads with the font data's
bounds checks."
  `(progn
     ;; The flags: one byte each, a repeat bit followed by a count.  A repeat
     ;; past the last point is dropped.
     (let ((i 0))
       (declare (type (unsigned-byte 17) i))
       (loop while (< i n)
             do (let ((flag ,(glyph-byte-form checked 'p))
                      (repeats 0))
                  (declare (type (unsigned-byte 8) flag repeats))
                  (incf p)
                  (when (logbitp 3 flag)
                    (setf repeats ,(glyph-byte-form checked 'p))
                    (incf p))
                  (loop repeat (min (1+ repeats) (- n i))
                        do (locally (declare (optimize (safety 0)))
                             (setf (aref zf i) flag))
                           (incf i)))))
     ,(glyph-coordinates-form checked 'ux 'ox 'cx 1 4 nil)
     ,(glyph-coordinates-form checked 'uy 'oy 'cy 2 5 t)
     (when vdecode
       (mul-fix-into ox cx ux scale n)
       (mul-fix-into oy cy uy scale n))))

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
    ;; The zone was sized for N points and CONTOUR-COUNT contours, so these
    ;; hold: with them said once, the loops below need no check on the arrays.
    (unless (and (<= (+ n 4) (length ux)) (<= (+ n 4) (length uy))
                 (<= (+ n 4) (length ox)) (<= (+ n 4) (length oy))
                 (<= (+ n 4) (length cx)) (<= (+ n 4) (length cy))
                 (<= (+ n 4) (length zf)) (<= contour-count (length zc)))
      (error "The glyph zone is too small for ~D points" n))
    (dotimes (c contour-count)
      (setf (aref zc c) (runes/otf:font-data-read-u16 fd (+ pos (* 2 c)))))
    (incf p (* 2 contour-count))
    ;; Skip the instructions.
    (incf p (+ 2 (the (unsigned-byte 16) (runes/otf:font-data-read-u16 fd p))))
    ;; Each point takes at most six bytes of flags and coordinates (a flag and
    ;; its repeat count, two words), so if that many are left in the font data
    ;; they can be read without a check each; a glyph at the very end of the
    ;; data is read with the checks.
    (let ((bytes (runes/otf:font-data-bytes fd))
          (base (runes/otf:font-data-offset fd))
          ;; Read with AVX-512, if the processor has it and the glyph has
          ;; points enough for it to pay.  CELL: the byte offsets of a block.
          (vdecode (and **avx512-decode** (>= n 16)))
          (cell (make-array 16 :element-type '(signed-byte 32))))
      (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
               (type (unsigned-byte 32) base)
               (dynamic-extent cell)
               (ignorable cell vdecode))
      (if (<= (+ p (* 6 n)) (runes/otf:font-data-length fd))
          (locally (declare (optimize (safety 0)))
            (decode-glyph-flags-and-coordinates nil))
          (decode-glyph-flags-and-coordinates t)))
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
           (type hint-buffer out))
  (let* ((scale (runes/otf/tt-hint::eng-scale eng))
         (data (and (plusp (length coords)) (gvar-glyph-data loader gid)))
         (n 0)
         (z nil))
    (declare (type (signed-byte 32) scale) (type (unsigned-byte 17) n))
    (setf n (if (zerop contour-count)
                0
                (1+ (runes/otf:font-data-read-u16
                     fd (+ pos (* 2 (1- contour-count)))))))
    (setf z (runes/otf/tt-hint:ensure-zone-capacity
             (runes/otf/tt-hint::eng-glyph eng) (+ n 4) contour-count :n-glyph-points n))
    (decode-simple-glyph-into-zone fd pos contour-count z n scale)
    (when (plusp (length coords))
      ;; A glyph of a variable font's instance: the deltas, if it has any, move
      ;; the font unit points, which are scaled again.  The phantom points go
      ;; after the glyph's own.
      (let ((ux (runes/otf/tt-hint:zone-unscaled-x z))
            (uy (runes/otf/tt-hint:zone-unscaled-y z))
            (ox (runes/otf/tt-hint:zone-orig-x z))
            (oy (runes/otf/tt-hint:zone-orig-y z))
            (cx (runes/otf/tt-hint:zone-cur-x z))
            (cy (runes/otf/tt-hint:zone-cur-y z))
            (sc nil))
        (declare (type fixed-array ux uy ox oy cx cy))
        (when data
          (dotimes (k 4)
            (setf (aref ux (+ n k)) (aref phantom (* 2 k))
                  (aref uy (+ n k)) (aref phantom (1+ (* 2 k)))))
          (setf sc (gvar-scratch-for (runes/otf/tt-hint::eng-gvar-scratch eng) (+ n 4)))
          (setf (runes/otf/tt-hint::eng-gvar-scratch eng) sc)
          (unless (zone-glyph-deltas data coords ux uy (+ n 4)
                                     (runes/otf/tt-hint:zone-contours z) contour-count sc)
            (setf sc nil)))
        ;; As FreeType's TT_Vary_Apply_Glyph_Deltas: the unscaled points the
        ;; program sees move by the deltas rounded to whole font units, but
        ;; the scaled ones come from the font units with the deltas to a 64th
        ;; (its `unrounded' points), for every glyph of a variation instance.
        (if sc
            (let ((dx (gs-dx sc)) (dy (gs-dy sc)))
              (declare (type fixed-array dx dy))
              (dotimes (i n)
                (let ((x0 (aref ux i)) (y0 (aref uy i)))
                  (setf (aref ux i) (+ x0 (the fixnum (fixed-round (aref dx i))))
                        (aref uy i) (+ y0 (the fixnum (fixed-round (aref dy i))))
                        (aref ox i) (scale-unrounded x0 (aref dx i) scale)
                        (aref oy i) (scale-unrounded y0 (aref dy i) scale)
                        (aref cx i) (aref ox i)
                        (aref cy i) (aref oy i))))
              (setf phantom (add-phantom-array-deltas phantom dx dy n)))
            (dotimes (i n)
              (setf (aref ox i) (scale-unrounded (aref ux i) 0 scale)
                    (aref oy i) (scale-unrounded (aref uy i) 0 scale)
                    (aref cx i) (aref ox i)
                    (aref cy i) (aref oy i))))))
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
      ;; When the caller asked (and this is the glyph asked for), the points stay
      ;; in the zone for it to take: no copy into the hint buffer.
      (if (eq (runes/otf/tt-hint::eng-direct eng) :request)
          (setf (runes/otf/tt-hint::eng-direct eng) :done)
        (let ((base (hb-n out))
              (first-end (hb-n-ends out)))
          (hb-reserve out n contour-count)
          ;; Short runs: a loop beats the call into the generic block copy.
          (let ((oxs (hb-xs out)) (oys (hb-ys out)) (ofl (hb-flags out)) (oends (hb-ends out)))
            (declare (optimize (safety 0)))
            (assert (and (<= (+ base n) (length oxs)) (<= (+ base n) (length oys))
                         (<= (+ base n) (length ofl)) (<= n (length cx)) (<= n (length zf))
                         (<= (+ first-end contour-count) (length oends))
                         (<= contour-count (length zc))))
            (dotimes (i n)
              (setf (aref oxs (+ base i)) (aref cx i)
                    (aref oys (+ base i)) (aref cy i)
                    (aref ofl (+ base i)) (aref zf i)))
            (dotimes (c contour-count)
              (setf (aref oends (+ first-end c)) (+ base (aref zc c)))))
          (setf (hb-n out) (+ base n)
                (hb-n-ends out) (+ first-end contour-count))))
      ;; In backward compatibility mode the program does not change the
      ;; bearings and advance: the phantom points stay as scaled, not
      ;; rounded (FreeType, skrifa).
      (let ((bc (runes/otf/tt-hint::gs-backward-compatibility (runes/otf/tt-hint::eng-gs eng)))
            (result (make-array 8 :element-type '(signed-byte 32))))
        (dotimes (k 4 result)
          (setf (aref result (* 2 k)) (if bc (aref ox (+ n k)) (aref cx (+ n k)))
                (aref result (1+ (* 2 k))) (if bc (aref oy (+ n k)) (aref cy (+ n k)))))))))

(defun transform-buffer-points (xs ys start end xx yx xy-skew yy)
  "Apply the 16.16 matrix XX YX XY-SKEW YY to the points START below END of
the arrays XS and YS, as TRANSFORM-COMPONENT-POINTS does."
  (declare (type (simple-array (signed-byte 32) (*)) xs ys) (fixnum start end))
  (loop for j of-type fixnum from start below end
        for x = (aref xs j)
        for y = (aref ys j)
        ;; Font units taken as raw 16.16, as FreeType does.
        do (setf (aref xs j) (runes/otf/tt-hint::s32 (f+ (f* x xx) (f* y xy-skew)))
                 (aref ys j) (runes/otf/tt-hint::s32 (f+ (f* x yx) (f* y yy))))))

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
           (type hint-buffer out))
  ;; The points of a composite glyph are put together in the hint buffer: a
  ;; request to leave a simple glyph's in the zone is not for its components.
  (setf (runes/otf/tt-hint::eng-direct eng) nil)
  (let* ((fd (tt-font-data (engine-tables eng loader)))
         (scale (runes/otf/tt-hint::eng-scale eng))
         (component-count (composite-component-count fd pos))
         (count (+ component-count 4))
         (data (and (plusp (length coords)) (gvar-glyph-data loader gid)))
         (dx nil) (dy nil)
         (point-base (hb-n out))
         ;; A composite nested in another one starts after its contours.
         (contour-base (hb-n-ends out))
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
          (let* ((start (hb-n out))
                 (comp-phantom (load-hinted-glyph-points loader eng component-gid out coords))
                 (end (hb-n out))
                 ;; The arrays after the component grew them, if it did.
                 (xs (hb-xs out))
                 (ys (hb-ys out))
                 (transformed (logtest flags #b11001000)))
            (declare (fixnum start end))
            (when (logbitp 9 flags)
              (setf scaled-phantom comp-phantom))
            (when transformed
              (transform-buffer-points xs ys start end xx yx xy-skew yy))
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
                      (values (- (aref xs base) (aref xs child))
                              (- (aref ys base) (aref ys child)))))
              (locally (declare (type fixnum ox oy))
                (unless (and (zerop ox) (zerop oy))
                  (let ((ox (runes/otf/tt-hint::s32 ox))
                        (oy (runes/otf/tt-hint::s32 oy)))
                    (loop for j of-type fixnum from start below end
                          do (setf (aref xs j) (runes/otf/tt-hint::f26+ (aref xs j) ox)
                                   (aref ys j) (runes/otf/tt-hint::f26+ (aref ys j) oy))))))))))
      (multiple-value-bind (istart iend) (composite-glyph-instruction-range fd pos)
        (when (and (= iend istart)
                   (not (runes/otf/tt-hint::gs-backward-compatibility
                         (runes/otf/tt-hint::eng-default-gs eng))))
          ;; No program to move them: the phantom points are rounded, as
          ;; they are before one runs.
          (dotimes (i 8)
            (setf (aref scaled-phantom i) (round-26-6 (aref scaled-phantom i)))))
        (when (> iend istart)
          (let* ((n (- (hb-n out) point-base))
                 (total (+ n 4))
                 (n-contours (- (hb-n-ends out) contour-base))
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
              (setf (aref zc c) (- (aref (hb-ends out) (+ contour-base c)) point-base)))
            (let ((xs (hb-xs out)) (ys (hb-ys out)) (flags (hb-flags out)))
              (dotimes (k n)
                (let ((vx (aref xs (+ point-base k)))
                      (vy (aref ys (+ point-base k))))
                  (setf (aref ux k) vx (aref uy k) vy
                        (aref ox k) vx (aref oy k) vy
                        (aref cx k) vx (aref cy k) vy
                        ;; Untouched, whatever the component left.
                        (aref zf k) (logand (aref flags (+ point-base k))
                                            runes/otf/tt-hint:+point-on-curve+)))))
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
            (replace (hb-xs out) cx :start1 point-base :end2 n)
            (replace (hb-ys out) cy :start1 point-base :end2 n)
            (let ((flags (hb-flags out)))
              (dotimes (k n)
                (setf (aref flags (+ point-base k))
                      (logand (aref zf k) runes/otf/tt-hint:+point-on-curve+))))
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

(defun shift-outline-by-pp1 (buffer phantom)
  "Move the x coordinates (26.6 pixels) of the hinted outline in BUFFER so that
its first phantom point, in PHANTOM, is at 0, as FreeType does.

Every consumer that returns the coordinates of a hinted glyph has to do this:
in backward compatibility mode the phantom points are not rounded, so
pp1.x is usually not 0 (xMin - lsb is not 0 for some glyphs) and the points
as loaded are off by it.  The hinted advance does not need it, as it is the
difference of two phantom points."
  (declare (type hint-buffer buffer) (type phantom-array phantom))
  (let ((pp1x (aref phantom 0))
        (xs (hb-xs buffer)))
    (unless (zerop pp1x)
      (dotimes (i (hb-n buffer))
        (setf (aref xs i) (runes/otf/tt-hint::f26- (aref xs i) pp1x)))))
  buffer)

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
             (phantom (load-hinted-glyph-points loader eng gid out coords-vec))
             (n (hb-n out))
             (on-curve (make-array n :element-type '(unsigned-byte 8))))
        (shift-outline-by-pp1 out phantom)
        (dotimes (i n)
          (setf (aref on-curve i) (logand (aref (hb-flags out) i) 1)))
        (values (subseq (hb-xs out) 0 n)
                (subseq (hb-ys out) 0 n)
                on-curve
                (subseq (hb-ends out) 0 (hb-n-ends out))
                phantom)))))

(defstruct (outline-26dot6 (:conc-name o26-))
  "A hinted outline in F26Dot6 pixels that HINT-OUTLINE-26DOT6-INTO fills: the
first N elements of XS, YS and ON-CURVE are the points (ON-CURVE is 1 for
on-curve ones), the first N-ENDS of ENDS the contours' last points, and PHANTOM
the phantom points.  The arrays grow as the outlines need, and are kept."
  (xs (make-array 128 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (*)))
  (ys (make-array 128 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (*)))
  (on-curve (make-array 128 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (ends (make-array 32 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (*)))
  (phantom (make-array 8 :element-type '(signed-byte 32)) :type (simple-array (signed-byte 32) (8)))
  (n 0 :type fixnum)
  (n-ends 0 :type fixnum))

(defun hint-outline-26dot6-into (loader gid ppem outline &key coords)
  "As HINT-OUTLINE-26DOT6, but into the OUTLINE-26DOT6 OUTLINE, which is the
value: nothing is allocated for an outline that fits in what it holds.  The
points of a simple glyph are copied once, from the engine's glyph zone."
  (declare (type outline-26dot6 outline))
  (let ((coords-vec (if coords
                        (map 'vector #'runes/otf/charstring::f2dot14-from-real
                             (normalize-coordinates loader coords))
                        (or (loader-gvar-coords loader) #()))))
    (with-pooled-tt-engine (eng loader ppem coords-vec)
      (let ((out (engine-outline eng)))
        (setf (runes/otf/tt-hint::eng-direct eng) :request)
        (let* ((phantom (load-hinted-glyph-points loader eng gid out coords-vec))
               (in-zone (eq (runes/otf/tt-hint::eng-direct eng) :done)))
          (declare (type phantom-array phantom))
          (setf (runes/otf/tt-hint::eng-direct eng) nil)
          (let* ((zone (runes/otf/tt-hint::eng-glyph eng))
                 (n (if in-zone (runes/otf/tt-hint:zone-n-glyph-points zone) (hb-n out)))
                 (n-ends (if in-zone (runes/otf/tt-hint:zone-n-contours zone) (hb-n-ends out))))
            (declare (fixnum n n-ends))
            (unless in-zone (shift-outline-by-pp1 out phantom))
            (when (> n (length (o26-xs outline)))
              (let ((size (max n (* 2 (length (o26-xs outline))))))
                (setf (o26-xs outline) (make-array size :element-type '(signed-byte 32))
                      (o26-ys outline) (make-array size :element-type '(signed-byte 32))
                      (o26-on-curve outline) (make-array size :element-type '(unsigned-byte 8)))))
            (when (> n-ends (length (o26-ends outline)))
              (setf (o26-ends outline)
                    (make-array (max n-ends (* 2 (length (o26-ends outline))))
                                :element-type '(signed-byte 32))))
            (let ((xs (o26-xs outline)) (ys (o26-ys outline)) (on (o26-on-curve outline))
                  (ends (o26-ends outline)))
              (locally (declare (optimize (safety 0)))
                (if in-zone
                    ;; One copy, from the zone, with the shift by the first phantom point.
                    (let ((cx (runes/otf/tt-hint:zone-cur-x zone)) (cy (runes/otf/tt-hint:zone-cur-y zone))
                          (zf (runes/otf/tt-hint:zone-flags zone)) (zc (runes/otf/tt-hint:zone-contours zone))
                          (pp1x (aref phantom 0)))
                      (assert (and (<= n (length cx)) (<= n (length cy)) (<= n (length zf))
                                   (<= n-ends (length zc))))
                      (dotimes (i n)
                        (setf (aref xs i) (runes/otf/tt-hint::f26- (aref cx i) pp1x)
                              (aref ys i) (aref cy i)
                              (aref on i) (logand (aref zf i) 1)))
                      (dotimes (i n-ends)
                        (setf (aref ends i) (aref zc i))))
                    (let ((hxs (hb-xs out)) (hys (hb-ys out)) (hflags (hb-flags out)) (hends (hb-ends out)))
                      (dotimes (i n)
                        (setf (aref xs i) (aref hxs i)
                              (aref ys i) (aref hys i)
                              (aref on i) (logand (aref hflags i) 1)))
                      (dotimes (i n-ends)
                        (setf (aref ends i) (aref hends i)))))))
            (replace (o26-phantom outline) phantom)
            (setf (o26-n outline) n
                  (o26-n-ends outline) n-ends)
            outline))))))

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

(declaim (inline f26dot6->pixels))
(defun f26dot6->pixels (v)
  "V, in 26.6 fixed point, in pixels: an integer for a whole number of them,
else a single-float.  Dividing a single-float by 64 is exact, so this is the
value (/ V 64) has, without making a ratio."
  (declare (type (signed-byte 32) v))
  (if (zerop (logand v 63))
      (ash v -6)
      (* (float v 1.0f0) 0.015625f0)))

(defun hint-buffer->contours (buffer)
  "The contours of the hinted outline in BUFFER as vectors of control points,
in pixels."
  (declare (type hint-buffer buffer))
  (let ((xs (hb-xs buffer)) (ys (hb-ys buffer)) (flags (hb-flags buffer))
        (ends (hb-ends buffer))
        (contours (make-array (hb-n-ends buffer)))
        (start 0))
    (declare (fixnum start))
    (dotimes (c (hb-n-ends buffer) contours)
      (let* ((end (aref ends c))
             (contour (make-array (max 0 (- (1+ end) start)))))
        (loop for i of-type fixnum from start to end
              for j of-type fixnum from 0
              do (setf (svref contour j)
                       (make-control-point (f26dot6->pixels (aref xs i))
                                           (f26dot6->pixels (aref ys i))
                                           (logbitp 0 (aref flags i)))))
        (setf (svref contours c) contour
              start (1+ end))))))

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
              (shift-outline-by-pp1 out (load-hinted-glyph-points loader eng gid out coords))
              (hint-buffer->contours out)))))))

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
