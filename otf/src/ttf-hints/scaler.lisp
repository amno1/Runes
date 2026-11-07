;;;; scaler.lisp — TrueType hinted glyph scaler, engine pool, and FreeType test harness API
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-ttf)

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
        (runes/otf/tt-hint:copy-zone-into (runes/otf/tt-hint::eng-default-twilight eng)
                                          (runes/otf/tt-hint::eng-twilight eng))
        eng))))

;; Thread-safe Engine Pool keyed by (loader ppem coords)
(defvar *tt-engine-pool-lock* (sb-thread:make-mutex :name "tt-engine-pool-lock"))
(defvar *tt-engine-pools* (make-hash-table :test 'eq :weakness :key))

(defun acquire-tt-engine (loader ppem coords)
  (let* ((ppem-key (round (* ppem 64)))
         (pool-entry
           (sb-thread:with-mutex (*tt-engine-pool-lock*)
             (let ((alist (gethash loader *tt-engine-pools*)))
               (or (find-if (lambda (entry)
                              (and (= (caar entry) ppem-key)
                                   (equalp (cdar entry) coords)))
                            alist)
                   (let ((new-entry (cons (cons ppem-key (copy-seq coords)) nil)))
                     (setf (gethash loader *tt-engine-pools*) (cons new-entry alist))
                     new-entry)))))
         (eng (sb-ext:atomic-pop (cdr pool-entry))))
    (values (or eng (make-tt-hint-engine loader ppem coords))
            pool-entry)))

(defun release-tt-engine (eng pool-entry)
  (sb-ext:atomic-push eng (cdr pool-entry)))

(defmacro with-pooled-tt-engine ((eng loader ppem coords) &body body)
  (let ((entry (gensym "POOL-ENTRY")))
    `(multiple-value-bind (,eng ,entry) (acquire-tt-engine ,loader ,ppem ,coords)
       (unwind-protect
            (progn ,@body)
         (release-tt-engine ,eng ,entry)))))

;;; ============================================================================
;;; 3. Hinted Simple & Composite Glyph Loading (Arena Zone Reuse)
;;; ============================================================================

(defun read-simple-glyph-instructions (fd pos contour-count)
  (let* ((instr-pos (+ pos (* 2 contour-count)))
         (instr-len (runes/otf:font-data-read-u16 fd instr-pos))
         (bytes (make-array instr-len :element-type '(unsigned-byte 8)))
         (abs-start (+ (runes/otf:font-data-offset fd) instr-pos 2)))
    (replace bytes (runes/otf:font-data-bytes fd) :start2 abs-start :end2 (+ abs-start instr-len))
    bytes))

(defun read-composite-glyph-instructions (fd pos)
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
        (make-array 0 :element-type '(unsigned-byte 8))
        (let* ((instr-len (runes/otf:font-data-read-u16 fd p))
               (bytes (make-array instr-len :element-type '(unsigned-byte 8)))
               (abs-start (+ (runes/otf:font-data-offset fd) p 2)))
          (replace bytes (runes/otf:font-data-bytes fd) :start2 abs-start :end2 (+ abs-start instr-len))
          bytes))))

(declaim (inline scale-funit round-26-6))
(defun scale-funit (u scale) (runes/otf/tt-hint::mul-fix u scale))
(defun round-26-6 (x) (logand (+ x 32) -64))

(defun run-glyph-bytecode-in-zone (eng instr)
  "Reset per-glyph state and run INSTR on ENG's already-populated glyph zone."
  (let* ((default-gs (runes/otf/tt-hint::eng-default-gs eng))
         (gs (runes/otf/tt-hint:reset-graphics-state-per-glyph
              (runes/otf/tt-hint::eng-gs eng) default-gs)))
    ;; Restore post-prep CVT and Storage snapshots (in case a previous glyph
    ;; executed WCVTP/WCVTF/WS in its glyph program, as Skrifa does).
    (replace (runes/otf/tt-hint::eng-cvt eng) (runes/otf/tt-hint::eng-default-cvt eng))
    (replace (runes/otf/tt-hint::eng-storage eng) (runes/otf/tt-hint::eng-default-storage eng))
    (when (and (plusp (length instr))
               (not (logbitp 0 (runes/otf/tt-hint::gs-instruct-control gs))))
      (setf (svref (runes/otf/tt-hint::eng-programs eng) 2) instr)
      (runes/otf/tt-hint::vs-clear (runes/otf/tt-hint::eng-stack eng))
      (runes/otf/tt-hint:run-program eng 2))))

(defun load-hinted-glyph-points (loader eng gid out coords)
  (when (> (length *compound-glyph-stack*) *max-compound-glyph-depth*)
    (error 'unsupported-value :description "Compound glyph nesting too deep"
                              :location (format nil "glyph ~D" gid)
                              :actual-value (length *compound-glyph-stack*)
                              :expected-values (list *max-compound-glyph-depth*)))
  (when (member gid *compound-glyph-stack*)
    (error 'unsupported-value :description "Recursive compound glyph"
                              :location (format nil "glyph ~D" (first *compound-glyph-stack*))
                              :actual-value gid
                              :expected-values '("a glyph that does not contain itself")))
  (multiple-value-bind (g-off len) (glyph-loca-info loader gid)
    (let ((glyf (table-info "glyf" loader))
          (fd (font-data loader))
          (scale (runes/otf/tt-hint::eng-scale eng)))
      (if (or (zerop len) (null glyf))
          (let ((phantom (phantom-points loader gid 0 0))
                (data (and (plusp (length coords)) (gvar-glyph-data loader gid))))
            (when data
              (multiple-value-bind (dx dy)
                  (simple-glyph-deltas data coords (map 'vector #'car phantom) (map 'vector #'cdr phantom) '())
                (when dx
                  (let ((moved (add-phantom-deltas phantom dx dy 0)))
                    (setf phantom (vector (aref moved 0) (aref moved 1) (aref phantom 2) (aref phantom 3)))))))
            (map 'vector (lambda (p)
                           (cons (round-26-6 (scale-funit (car p) scale))
                                 (round-26-6 (scale-funit (cdr p) scale))))
                 phantom))
          (let* ((pos (+ (offset glyf) g-off))
                 (contour-count (runes/otf:font-data-read-i16 fd pos))
                 (phantom (phantom-points loader gid
                                          (runes/otf:font-data-read-i16 fd (+ pos 2))
                                          (runes/otf:font-data-read-i16 fd (+ pos 8)))))
            (if (minusp contour-count)
                (let ((*compound-glyph-stack* (cons gid *compound-glyph-stack*)))
                  (load-hinted-composite-points loader eng gid (+ pos 10) phantom out coords))
                (load-hinted-simple-points loader eng gid fd (+ pos 10) contour-count phantom out coords)))))))

(defun load-hinted-simple-points (loader eng gid fd pos contour-count phantom out coords)
  (multiple-value-bind (ends xs ys on) (read-simple-points fd pos contour-count)
    (let* ((n (length xs))
           (total (+ n 4))
           (scale (runes/otf/tt-hint::eng-scale eng))
           (data (and (plusp (length coords)) (gvar-glyph-data loader gid))))
      (when data
        (let ((all-x (concatenate 'vector xs (map 'vector #'car phantom)))
              (all-y (concatenate 'vector ys (map 'vector #'cdr phantom))))
          (multiple-value-bind (dx dy) (simple-glyph-deltas data coords all-x all-y (coerce ends 'list))
            (when dx
              (dotimes (i n)
                (setf (aref xs i) (+ (aref xs i) (fixed-round (aref dx i)))
                      (aref ys i) (+ (aref ys i) (fixed-round (aref dy i)))))
              (setf phantom (add-phantom-deltas phantom dx dy n))))))
      ;; Reuse the engine's glyph zone arena in place!
      (let* ((z (runes/otf/tt-hint:ensure-zone-capacity
                 (runes/otf/tt-hint::eng-glyph eng) total contour-count :n-glyph-points n))
             (ux (runes/otf/tt-hint:zone-unscaled-x z))
             (uy (runes/otf/tt-hint:zone-unscaled-y z))
             (ox (runes/otf/tt-hint:zone-orig-x z))
             (oy (runes/otf/tt-hint:zone-orig-y z))
             (cx (runes/otf/tt-hint:zone-cur-x z))
             (cy (runes/otf/tt-hint:zone-cur-y z))
             (zf (runes/otf/tt-hint:zone-flags z))
             (zc (runes/otf/tt-hint:zone-contours z)))
        (dotimes (c contour-count)
          (setf (aref zc c) (aref ends c)))
        (dotimes (i n)
          (let ((sx (scale-funit (aref xs i) scale))
                (sy (scale-funit (aref ys i) scale)))
            (setf (aref ux i) (aref xs i) (aref uy i) (aref ys i)
                  (aref ox i) sx          (aref oy i) sy
                  (aref cx i) sx          (aref cy i) sy
                  (aref zf i) (if (aref on i) runes/otf/tt-hint:+point-on-curve+ 0))))
        (dotimes (k 4)
          (let* ((idx (+ n k))
                 (px (car (aref phantom k)))
                 (py (cdr (aref phantom k)))
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
        (let ((instr (read-simple-glyph-instructions fd pos contour-count)))
          (setf (runes/otf/tt-hint::eng-unscaled-scale eng) scale
                (runes/otf/tt-hint::eng-composite-p eng) nil)
          (run-glyph-bytecode-in-zone eng instr))
        (let ((base (fill-pointer (op-xs out))))
          (dotimes (i n)
            (vector-push-extend (aref cx i) (op-xs out))
            (vector-push-extend (aref cy i) (op-ys out))
            (vector-push-extend (logtest (aref zf i) runes/otf/tt-hint:+point-on-curve+) (op-on out)))
          (loop for end across ends
                do (vector-push-extend (+ base end) (op-ends out))))
        ;; In backward compatibility mode the program does not change the
        ;; bearings and advance: the phantom points stay as scaled, not
        ;; rounded (FreeType, skrifa).
        (let ((bc (runes/otf/tt-hint::gs-backward-compatibility (runes/otf/tt-hint::eng-gs eng))))
          (flet ((phantom-point (k)
                   (if bc
                       (cons (aref ox (+ n k)) (aref oy (+ n k)))
                       (cons (aref cx (+ n k)) (aref cy (+ n k))))))
            (vector (phantom-point 0) (phantom-point 1) (phantom-point 2) (phantom-point 3))))))))

(defun load-hinted-composite-points (loader eng gid pos phantom out coords)
  (let* ((fd (font-data loader))
         (scale (runes/otf/tt-hint::eng-scale eng))
         (components (read-components fd pos))
         (count (+ (length components) 4))
         (data (and (plusp (length coords)) (gvar-glyph-data loader gid)))
         (dx nil) (dy nil)
         (point-base (fill-pointer (op-xs out)))
         ;; A composite nested in another one starts after its contours.
         (contour-base (length (op-ends out))))
    (when data
      (multiple-value-setq (dx dy) (composite-glyph-deltas data coords count))
      (when dx
        (setf phantom (add-phantom-deltas phantom dx dy (- count 4)))))
    (let ((scaled-phantom
            ;; Not rounded yet: the program's original positions are these,
            ;; and its current ones the rounded.
            (map 'vector (lambda (p)
                           (cons (scale-funit (car p) scale)
                                 (scale-funit (cdr p) scale)))
                 phantom)))
      (loop for component in components
            for i from 0
            do (let* ((start (fill-pointer (op-xs out)))
                      (comp-phantom (load-hinted-glyph-points
                                     loader eng (getf component :glyph) out coords))
                      (end (fill-pointer (op-xs out)))
                      (xs (op-xs out))
                      (ys (op-ys out))
                      (flags (getf component :flags)))
                 (when (logbitp 9 flags)
                   (setf scaled-phantom comp-phantom))
                 (when (getf component :transformed)
                   (transform-component-points xs ys start end (getf component :transform)))
                 (multiple-value-bind (ox oy)
                     (if (getf component :xy)
                         (let* ((ux (+ (getf component :arg1)
                                       (if (and dx (aref dx i)) (fixed-round (aref dx i)) 0)))
                                (uy (+ (getf component :arg2)
                                       (if (and dy (aref dy i)) (fixed-round (aref dy i)) 0)))
                                (m (getf component :transform)))
                           (when (and (getf component :transformed)
                                      (= (logand flags #x1800) #x0800))
                             (setf ux (f* ux (hypot-fixed (aref m 0) (aref m 2)))
                                   uy (f* uy (hypot-fixed (aref m 3) (aref m 1)))))
                           (let ((sx (scale-funit ux scale))
                                 (sy (scale-funit uy scale)))
                             ;; Rounded to the grid in y only: as FreeType's
                             ;; v40 interpreter (and skrifa) places a
                             ;; component horizontally at subpixel precision.
                             (if (logbitp 2 flags)
                                 (values sx (round-26-6 sy))
                                 (values sx sy))))
                         (let ((base (+ point-base (getf component :arg1)))
                               (child (+ start (getf component :arg2))))
                           (unless (and (< base start) (< child end))
                             (error 'unsupported-value
                                    :description "Compound glyph anchor point out of range"
                                    :location (format nil "glyph ~D" gid)
                                    :actual-value (list (getf component :arg1) (getf component :arg2))
                                    :expected-values '("existing point indices")))
                           (values (- (aref xs base) (aref xs child))
                                   (- (aref ys base) (aref ys child)))))
                   (unless (and (zerop ox) (zerop oy))
                     (loop for j from start below end
                           do (incf (aref xs j) ox)
                              (incf (aref ys j) oy))))))
      (let ((instr (read-composite-glyph-instructions fd pos)))
        (when (and (zerop (length instr))
                   (not (runes/otf/tt-hint::gs-backward-compatibility
                         (runes/otf/tt-hint::eng-default-gs eng))))
          ;; No program to move them: the phantom points are rounded, as
          ;; they are before one runs.
          (map-into scaled-phantom
                    (lambda (p) (cons (round-26-6 (car p)) (round-26-6 (cdr p))))
                    scaled-phantom))
        (when (plusp (length instr))
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
            (dotimes (c n-contours)
              (setf (aref zc c) (- (aref (op-ends out) (+ contour-base c)) point-base)))
            (dotimes (k n)
              (let ((vx (aref (op-xs out) (+ point-base k)))
                    (vy (aref (op-ys out) (+ point-base k))))
                (setf (aref ux k) vx (aref uy k) vy
                      (aref ox k) vx (aref oy k) vy
                      (aref cx k) vx (aref cy k) vy
                      (aref zf k) (if (aref (op-on out) (+ point-base k))
                                      runes/otf/tt-hint:+point-on-curve+
                                      0))))
            (dotimes (k 4)
              (let ((idx (+ n k))
                    (px (car (aref scaled-phantom k)))
                    (py (cdr (aref scaled-phantom k))))
                (setf (aref ux idx) px (aref uy idx) py
                      (aref ox idx) px (aref oy idx) py
                      (aref cx idx) (round-26-6 px) (aref cy idx) (round-26-6 py)
                      (aref zf idx) 0)))
            ;; The points of a composite glyph are scaled already.
            (setf (runes/otf/tt-hint::eng-unscaled-scale eng) #x10000
                  (runes/otf/tt-hint::eng-composite-p eng) t)
            (run-glyph-bytecode-in-zone eng instr)
            (dotimes (k n)
              (setf (aref (op-xs out) (+ point-base k)) (aref cx k)
                    (aref (op-ys out) (+ point-base k)) (aref cy k)
                    (aref (op-on out) (+ point-base k))
                    (logtest (aref zf k) runes/otf/tt-hint:+point-on-curve+)))
            ;; As for a simple glyph, the program leaves them alone in backward
            ;; compatibility mode.
            (unless (runes/otf/tt-hint::gs-backward-compatibility (runes/otf/tt-hint::eng-gs eng))
              (dotimes (k 4)
                (setf (aref scaled-phantom k)
                      (cons (aref cx (+ n k)) (aref cy (+ n k)))))))))
      scaled-phantom)))

;;; ============================================================================
;;; 4. Public High-Level & Test Harness APIs
;;; ============================================================================

(defun shift-outline-by-pp1 (xs phantom)
  "Move the x coordinates XS (26.6 pixels) of a hinted outline so that its
first phantom point, in PHANTOM, is at 0, as FreeType does.

Every consumer that returns the coordinates of a hinted glyph has to do this:
in backward compatibility mode the phantom points are not rounded, so
pp1.x is usually not 0 (xMin - lsb is not 0 for some glyphs) and the points
as loaded are off by it.  The hinted advance does not need it, as it is the
difference of two phantom points."
  (let ((pp1x (car (aref phantom 0))))
    (unless (zerop pp1x)
      (dotimes (i (length xs))
        (decf (aref xs i) pp1x))))
  xs)

(defun hint-outline-26dot6 (loader gid ppem &key coords)
  "Hint glyph GID of LOADER at PPEM and return raw F26Dot6 coordinates for
point-by-point comparison against FreeType's FT_Outline:
  (values xs ys on-curve contour-ends phantom-points)"
  (let ((coords-vec (if coords
                        (map 'vector #'runes/otf/charstring::f2dot14-from-real
                             (normalize-coordinates loader coords))
                        (or (loader-gvar-coords loader) #())))
        (out (make-outline-points)))
    (with-pooled-tt-engine (eng loader ppem coords-vec)
      (let ((phantom (load-hinted-glyph-points loader eng gid out coords-vec)))
        (shift-outline-by-pp1 (op-xs out) phantom)
        (values (copy-seq (op-xs out))
                (copy-seq (op-ys out))
                (copy-seq (op-on out))
                (copy-seq (op-ends out))
                phantom)))))

(defun f26dot6->pixels (v)
  (let ((r (/ v 64)))
    (if (integerp r) r (float r 1.0))))

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
        (let ((coords (or (loader-gvar-coords loader) #()))
              (out (make-outline-points)))
          (with-pooled-tt-engine (eng loader ppem coords)
            (shift-outline-by-pp1 (op-xs out) (load-hinted-glyph-points loader eng gid out coords)))
          (dotimes (i (length (op-xs out)))
            (setf (aref (op-xs out) i) (f26dot6->pixels (aref (op-xs out) i))
                  (aref (op-ys out) i) (f26dot6->pixels (aref (op-ys out) i))))
          (outline-points->contours out)))))

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
        (let ((coords (or (loader-gvar-coords loader) #()))
              (out (make-outline-points)))
          (with-pooled-tt-engine (eng loader ppem coords)
            (let ((phantom (load-hinted-glyph-points loader eng gid out coords)))
              ;; Whole pixels, as FreeType's hinted advance.
              (f26dot6->pixels (round-26-6 (- (car (aref phantom 1)) (car (aref phantom 0)))))))))))
