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
;;;; The table entries are inflate.lisp's, libdeflate's layout (below): a 10-bit
;;;; literal root and an 8-bit distance root.
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
            (e :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num unsigned-num unsigned-num unsigned-num unsigned-num)
  (:generator 200
    (let ((top (gen-label)) (other (gen-label)) (done (gen-label))
          (out-of-line '()))
      (move val val-in) (move bits bits-in) (move pos pos-in) (move fp fp-in)
      (flet ((refill ()
               ;; val |= load64(in + pos) << bits; pos += ((63 - bits) >> 3) & 7;
               ;; bits |= 56            (only BITS's low byte counts)
               (inst mov tt (ea in pos))
               (inst shlx tt tt bits)
               (inst or val tt)
               (inst mov tt 63)
               (inst sub tt bits)
               (inst shr tt 3)
               (inst and tt 7)
               (inst add pos tt)
               (inst or bits 56))
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
                 ;; E is a root entry: not a literal goes to OTHER with VAL
                 ;; unshifted; a literal is consumed and stored
                 (inst test :dword e e)
                 (inst jmp :ns other)
                 (inst shrx val val e)
                 (inst sub bits e)
                 (inst mov tt e)
                 (inst shr tt 16)
                 (inst mov :byte (ea out fp) tt)
                 (inst inc fp)))
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

          ;; OTHER: E is a length entry, a subtable link, the end code or
          ;; invalid; VAL is positioned at its code
          (emit-label other)
          (let ((exc (gen-label)) (have (gen-label)) (dexc (gen-label)) (dsub (gen-label))
                (dback (gen-label)))
            (inst test e #x8000)
            (inst jmp :nz exc)
            ;; length: SAVED = VAL before the code; the entry's bit count
            ;; covers the code and the extra bits
            (inst mov saved val)
            (inst shrx val val e)
            (inst sub bits e)
            (inst bzhi saved saved e)
            (inst mov u e)
            (inst shr u 8)
            (inst shrx saved saved u)
            (inst shr e 16)
            (inst add saved e)                ; SAVED = len
            (inst cmp :byte bits 38)
            (inst jmp :ae have)
            (refill)
            (emit-label have)
            ;; the distance code
            (inst mov tt val)
            (inst and tt 255)
            (inst mov :dword e (ea dtab tt 4))
            (inst test e #x8000)
            (inst jmp :nz dexc)
            (emit-label dback)
            (inst mov u val)
            (inst shrx val val e)
            (inst sub bits e)
            (inst bzhi u u e)
            (inst mov tt e)
            (inst shr tt 8)
            (inst shrx u u tt)
            (inst shr e 16)
            (inst add e u)                    ; E = dist
            (inst cmp e fp)
            (inst jmp :a (leave 5))
            (inst mov u fp)
            (inst sub u e)                    ; U = source position
            (let ((long (gen-label)) (by8 (gen-label)) (near (gen-label)))
              (inst cmp e 8)
              (inst jmp :b near)
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
              (inst add fp saved)
              (inst jmp top)
              (emit-label near)
              (inst mov tt (ea out u))
              (inst mov (ea out fp) tt)
              (inst add u e)
              (inst add fp e)
              (inst sub saved e)
              (inst jmp :g near)
              (inst add fp saved)
              (inst jmp top))
            ;; out of line: exceptional literal/length entries
            (let ((lsub (gen-label)) (eob (gen-label)))
              (push (lambda ()
                      (emit-label exc)
                      (inst test e #x4000)
                      (inst jmp :nz lsub)
                      (inst test e #x2000)
                      (inst jmp :nz eob)
                      (inst mov e 3)         ; invalid length code
                      (inst jmp done)
                      (emit-label eob)
                      (inst shrx val val e)
                      (inst sub bits e)
                      (inst mov e 0)
                      (inst jmp done)
                      ;; a subtable: consume the root, index with the rest
                      (emit-label lsub)
                      (inst shrx val val e)
                      (inst sub bits e)
                      (inst mov tt e)
                      (inst shr tt 8)
                      (inst and tt 63)
                      (inst bzhi tt val tt)
                      (inst shr e 16)
                      (inst add e tt)
                      (inst mov :dword e (ea ltab e 4))
                      (inst test :dword e e)
                      (inst jmp :ns other)
                      ;; a literal from the subtable
                      (inst shrx val val e)
                      (inst sub bits e)
                      (inst mov tt e)
                      (inst shr tt 16)
                      (inst mov :byte (ea out fp) tt)
                      (inst inc fp)
                      (inst jmp top))
                    out-of-line))
            ;; out of line: exceptional distance entries
            (push (lambda ()
                    (emit-label dexc)
                    (inst test e #x4000)
                    (inst jmp :nz dsub)
                    (inst mov e 4)              ; invalid distance code
                    (inst jmp done)
                    (emit-label dsub)
                    (inst shrx val val e)
                    (inst sub bits e)
                    (inst mov tt e)
                    (inst shr tt 8)
                    (inst and tt 63)
                    (inst bzhi tt val tt)
                    (inst shr e 16)
                    (inst add e tt)
                    (inst mov :dword e (ea dtab e 4))
                    (inst test e #x8000)
                    (inst jmp :z dback)
                    (inst mov e 4)
                    (inst jmp done))
                  out-of-line))
          (mapc #'funcall (reverse out-of-line))
          (emit-label done)
          (inst and bits 63)))))))

;;; The VOP needs twelve registers at once, so it is called from this small
;;; function, where nothing else needs a register; INFLATE-BLOCK calls it once
;;; per block.
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
