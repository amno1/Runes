;;;; src/inflate-vop.lisp — the deflate symbol loop as one VOP (x86-64, BMI2).
;;;;
;;;; INFLATE-BLOCK's loop, written out in assembler so that the bit buffer,
;;;; the positions and the table bases stay raw words in registers, with no
;;;; tagging and no stack traffic.  The same steps as the Lisp loop: a refill
;;;; with one unaligned load, three literals per refill with the first root
;;;; lookup issued before the refill, one shift for a length code and its
;;;; extra bits (from SAVED), and the same copy classes as COPY-MATCH.
;;;;
;;;; It only runs where no check is needed: it stops at the top of an
;;;; iteration when the output is within 258 bytes + slack of its end (:FULL)
;;;; or fewer than 16 input bytes remain (the Lisp loop finishes the block,
;;;; with its checked refills).  An invalid code or distance stops it with a
;;;; status; INFLATE-BLOCK signals the error.
;;;;
;;;; Needs SHRX/SHLX/BZHI (BMI2, on every AVX-512 CPU) and the sb-simd-512
;;;; assembler, which has them: compiled only with :DECOMPRESS-AVX512.
;;;; The numbers below are inflate.lisp's: a 10-bit literal root, an 8-bit
;;;; distance root, +MATCH-FLAG+ = #x100000, entries sym<<5 | nbits with
;;;; nbits >= 16 a subtable link.
(in-package #:sb-vm)

#+decompress-avx512
(progn
;; at compile time too: INFLATE-FAST below is compiled with the VOP
(eval-when (:compile-toplevel :load-toplevel :execute)
(sb-c:defknown decompress::%inflate-fast
    ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64)
     system-area-pointer system-area-pointer system-area-pointer system-area-pointer
     (unsigned-byte 64) (unsigned-byte 64))
    (values (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64)
            (unsigned-byte 64))
    ()
  :overwrite-fndb-silently t)

;;; (%inflate-fast val bits pos fp in out ltab dtab in-limit cap)
;;;   => val bits pos fp status
;;; IN, OUT, LTAB, DTAB: the input, output and table bases.  IN-LIMIT: the
;;; last POS at which an iteration may start (input length - 16).  CAP: the
;;; last FP at which one may start.  Status: 0 end of block, 1 output full,
;;; 2 near the input's end, 3 invalid length code, 4 invalid distance code,
;;; 5 distance too far back.
(define-vop (decompress::%inflate-fast)
  (:translate decompress::%inflate-fast)
  (:policy :fast-safe)
  (:args (val-in :scs (unsigned-reg) :target val)
         (bits-in :scs (unsigned-reg) :target bits)
         (pos-in :scs (unsigned-reg) :target pos)
         (fp-in :scs (unsigned-reg) :target fp)
         (in :scs (sap-reg) :to :result)
         (out :scs (sap-reg) :to :result)
         (ltab :scs (sap-reg) :to :result)
         (dtab :scs (sap-reg) :to :result)
         (in-limit :scs (unsigned-reg unsigned-stack) :load-if nil :to :result)
         (cap :scs (unsigned-reg unsigned-stack) :load-if nil :to :result))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num
              system-area-pointer system-area-pointer system-area-pointer system-area-pointer
              unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg) tt saved u)
  (:results (val :scs (unsigned-reg) :from (:argument 0))
            (bits :scs (unsigned-reg) :from (:argument 1))
            (pos :scs (unsigned-reg) :from (:argument 2))
            (fp :scs (unsigned-reg) :from (:argument 3))
            (e :scs (unsigned-reg) :from :load))   ; the entry / symbol, then the status
  (:result-types unsigned-num unsigned-num unsigned-num unsigned-num unsigned-num)
  (:generator 200
    (let ((top (gen-label)) (other (gen-label)) (done (gen-label))
          (out-of-line '()))
      (move val val-in) (move bits bits-in) (move pos pos-in) (move fp fp-in)
      (flet ((refill ()
               ;; val |= load64(in + pos) << bits; pos += (63 - bits) >> 3;
               ;; bits |= 56
               (inst mov tt (ea in pos))
               (inst shlx tt tt bits)
               (inst or val tt)
               (inst mov tt 63)
               (inst sub tt bits)
               (inst shr tt 3)
               (inst add pos tt)
               (inst or bits 56))
             (drop (n)
               (inst shrx val val n)
               (inst sub bits n))
             (leave (status)
               (let ((l (gen-label)))
                 (push (lambda () (emit-label l) (inst mov e status) (inst jmp done))
                       out-of-line)
                 l)))
        (flet ((lit-lookup ()
                 (inst mov tt val)
                 (inst and tt 1023)
                 (inst mov :dword e (ea ltab tt 4)))
               (finish-literal ()
                 ;; E is a root entry: follow a subtable link (out of line),
                 ;; consume the code, store a literal or leave for OTHER
                 (let ((sub (gen-label)) (back (gen-label)))
                   (inst mov tt e)
                   (inst and tt 31)
                   (inst cmp tt 16)
                   (inst jmp :ae sub)
                   (emit-label back)
                   (inst mov saved val)
                   (drop tt)
                   (inst shr e 5)
                   (inst cmp e 256)
                   (inst jmp :ae other)
                   (inst mov :byte (ea out fp) e)
                   (inst inc fp)
                   (push (lambda ()
                           (emit-label sub)
                           (inst shr val 10)
                           (inst sub bits 10)
                           (inst sub tt 16)
                           (inst bzhi saved val tt)
                           (inst shr e 5)
                           (inst add saved e)
                           (inst mov :dword e (ea ltab saved 4))
                           (inst mov tt e)
                           (inst and tt 31)
                           (inst jmp back))
                         out-of-line))))
          ;; entry: the first root lookup needs >= 10 valid bits
          (inst cmp pos in-limit)
          (inst jmp :a (leave 2))
          (refill)
          (emit-label top)
          (inst cmp fp cap)
          (inst jmp :a (leave 1))
          (inst cmp pos in-limit)
          (inst jmp :a (leave 2))
          (lit-lookup)
          (refill)
          (finish-literal)
          (lit-lookup)
          (finish-literal)
          (lit-lookup)
          (finish-literal)
          (inst jmp top)

          ;; E: the end code, a length entry or an invalid symbol; SAVED: VAL
          ;; before the code's shift
          (emit-label other)
          (inst cmp e 256)
          (inst jmp :e (leave 0))
          (inst cmp e #x100000)
          (inst jmp :b (leave 3))
          (let ((have (gen-label)))
            (inst cmp bits 38)
            (inst jmp :ae have)
            (refill)
            (emit-label have))
          ;; len = base + the extra bits above the code in SAVED
          (inst mov tt e)
          (inst and tt 15)
          (inst shrx saved saved tt)
          (inst mov tt e)
          (inst shr tt 16)
          (inst and tt 7)
          (inst bzhi saved saved tt)
          (inst mov tt e)
          (inst shr tt 4)
          (inst and tt 511)
          (inst add saved tt)               ; SAVED = len
          ;; the distance code
          (let ((sub (gen-label)) (back (gen-label)))
            (inst mov tt val)
            (inst and tt 255)
            (inst mov :dword e (ea dtab tt 4))
            (inst mov tt e)
            (inst and tt 31)
            (inst cmp tt 16)
            (inst jmp :ae sub)
            (emit-label back)
            (drop tt)
            (inst shr e 5)
            (push (lambda ()
                    (emit-label sub)
                    (inst shr val 8)
                    (inst sub bits 8)
                    (inst sub tt 16)
                    (inst bzhi u val tt)
                    (inst shr e 5)
                    (inst add u e)
                    (inst mov :dword e (ea dtab u 4))
                    (inst mov tt e)
                    (inst and tt 31)
                    (inst jmp back))
                  out-of-line))
          (inst cmp e #x100000)
          (inst jmp :b (leave 4))
          (inst mov tt e)
          (inst and tt 15)
          (inst bzhi u val tt)
          (drop tt)
          (inst shr e 4)
          (inst and e 32767)
          (inst add e u)                    ; E = dist
          (inst cmp e fp)
          (inst jmp :a (leave 5))
          (inst mov u fp)
          (inst sub u e)                    ; U = source position
          ;; the copy, overshooting into the output slack (at most 39 bytes)
          (let ((long (gen-label)) (by8 (gen-label)) (near (gen-label)))
            (inst cmp e 8)
            (inst jmp :b near)
            ;; dist >= 8: three words (24 bytes) whatever the length (as
            ;; libdeflate, which copies five), so most matches take no
            ;; length-dependent branch;
            ;; each word is loaded after the previous store (DIST >= 8)
            (dotimes (k 3)
              (inst mov tt (ea (* 8 k) out u))
              (inst mov (ea (* 8 k) out fp) tt))
            (inst cmp saved 24)
            (inst jmp :a long)
            (inst add fp saved)
            (inst jmp top)
            (emit-label long)
            (inst add u 24)
            (inst add fp 24)
            (inst sub saved 24)
            (emit-label by8)
            (inst mov tt (ea out u))
            (inst mov (ea out fp) tt)
            (inst add u 8)
            (inst add fp 8)
            (inst sub saved 8)
            (inst jmp :g by8)
            (inst add fp saved)             ; back by the overshoot
            (inst jmp top)
            ;; dist < 8: 8-byte stores advancing DIST bytes (libdeflate)
            (emit-label near)
            (inst mov tt (ea out u))
            (inst mov (ea out fp) tt)
            (inst add u e)
            (inst add fp e)
            (inst sub saved e)
            (inst jmp :g near)
            (inst add fp saved)
            (inst jmp top))
          (mapc #'funcall (reverse out-of-line))
          (emit-label done)))))))

;;; The VOP needs twelve registers, so it is called from this function of its
;;; own, where nothing else is live; INFLATE-BLOCK calls it once per block.
;;; (Inside INFLATE-BLOCK it packs only while that function's own register
;;; use around the call happens to leave room.)
(defun decompress::inflate-fast (val bits pos fp in buf ltab dtab in-limit cap)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 64) val bits pos fp in-limit cap)
           (type (simple-array (unsigned-byte 8) (*)) in buf)
           (type (simple-array (unsigned-byte 32) (*)) ltab dtab))
  (sb-sys:with-pinned-objects (in buf ltab dtab)
    (decompress::%inflate-fast val bits pos fp
                               (sb-sys:vector-sap in) (sb-sys:vector-sap buf)
                               (sb-sys:vector-sap ltab) (sb-sys:vector-sap dtab)
                               in-limit cap))))
