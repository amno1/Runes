;;;; src/brotli-vop.lisp — the Brotli command loop as one VOP (x86-64, BMI2).
;;;;
;;;; COMMAND-LOOP's common commands, written out in assembler so that the bit
;;;; buffer, the positions and the last two bytes stay raw words in registers:
;;;; the insert-and-copy symbol and lengths, the literal run, the distance
;;;; (ring, direct or computed) and the back-reference copy.  What does not fit
;;;; in twelve registers lives in ST, a word vector the Lisp side fills: block
;;;; counts, the current decode tables (tagged pointers to pinned objects), the
;;;; distance ring and the per-stream constants.
;;;;
;;;; It stops, with a status, wherever Lisp has work the VOP does not do: a
;;;; block switch (insert at a command's top, literal inside a run, distance
;;;; before the distance code), a static dictionary word, the meta-block's end,
;;;; or an error.  Mid-command stops leave the rest of the command (CMD, ILEN,
;;;; CLEN, DIST) in ST for COMMAND-LOOP to finish.  Refills are checked (the
;;;; last 8 input bytes come from the reader's tail), so the input's end never
;;;; stops it.
;;;;
;;;; Needs SHRX/SHLX/BZHI and the sb-simd-512 assembler: compiled only with
;;;; :DECOMPRESS-AVX512.  Decode tables as in huffman.lisp: an 8-bit root,
;;;; entries sym<<5 | nbits, nbits >= 16 a subtable link.
(in-package #:decompress)

;;; Word indices into ST.  The VOP reaches the distance tables and the ring
;;; by index from +ST-DTAB+ and +ST-RB+, so each run of four must stay
;;; contiguous after its base (the numbered names only reserve the slots).
(macrolet ((def (&rest names)
             `(progn ,@(loop for n in names for i from 0
                             collect `(defconstant ,n ,i)))))
  (def +st-icount+ +st-lcount+ +st-dcount+ +st-target+ +st-inlim+ +st-inend+
       +st-tbase+ +st-tail+ +st-itab+ +st-cmdins+ +st-cmdcpy+ +st-slice+ +st-ctx+
       +st-dtab+ +st-dtab1+ +st-dtab2+ +st-dtab3+ +st-window+ +st-ndirect+
       +st-npostfix+ +st-pmask+ +st-rb+ +st-rb1+ +st-rb2+ +st-rb3+ +st-rbidx+
       +st-ilen+ +st-clen+ +st-cmd+ +st-dist+ +st-end+ +st-dsci+))
(defconstant +st-dscv+ (+ +st-dsci+ 16))
(defconstant +st-size+ (+ +st-dscv+ 16))

(in-package #:sb-vm)

#+decompress-avx512
(progn
;; at compile time too: BROTLI-FAST below is compiled with the VOP
(eval-when (:compile-toplevel :load-toplevel :execute)
(sb-c:defknown decompress::%brotli-fast
    ((unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64)
     system-area-pointer system-area-pointer system-area-pointer)
    (values (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64) (unsigned-byte 64)
            (unsigned-byte 64))
    ()
  :overwrite-fndb-silently t)

;;; (%brotli-fast val bits pos fp in buf st) => val bits pos fp status
;;; Status: 0 FP reached the target, 1 insert block switch (at a command's
;;; top), 2 literal block switch (in a run), 3 distance block switch, 4 a
;;; dictionary word (DIST in ST); 10 insert overrun, 11 copy overrun, 12
;;; invalid distance, 13 end of input.
(define-vop (decompress::%brotli-fast)
  (:translate decompress::%brotli-fast)
  (:policy :fast-safe)
  (:args (val-in :scs (unsigned-reg) :target val)
         (bits-in :scs (unsigned-reg) :target bits)
         (pos-in :scs (unsigned-reg) :target pos)
         (fp-in :scs (unsigned-reg) :target fp)
         (in :scs (sap-reg) :to :result)
         (buf :scs (sap-reg) :to :result)
         (st :scs (sap-reg) :to :result))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num
              system-area-pointer system-area-pointer system-area-pointer)
  (:temporary (:sc unsigned-reg) p1 p2 tt u)
  (:results (val :scs (unsigned-reg) :from (:argument 0))
            (bits :scs (unsigned-reg) :from (:argument 1))
            (pos :scs (unsigned-reg) :from (:argument 2))
            (fp :scs (unsigned-reg) :from (:argument 3))
            (e :scs (unsigned-reg) :from :load))   ; entries, then the status
  (:result-types unsigned-num unsigned-num unsigned-num unsigned-num unsigned-num)
  (:generator 300
    (let ((top (gen-label)) (litrun (gen-label)) (litdone (gen-label))
          (implicit (gen-label)) (have (gen-label)) (copy (gen-label))
          (tail (gen-label)) (done (gen-label))
          (out-of-line '())
          (vd (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
      (move val val-in) (move bits bits-in) (move pos pos-in) (move fp fp-in)
      (let ((eoi (gen-label)))           ; the refills' stubs jump here
      (push (lambda () (emit-label eoi) (inst mov e 13) (inst jmp done)) out-of-line)
      (labels ((stf (k) (ea (* 8 k) st))
               (leave (status)
                 (let ((l (gen-label)))
                   (push (lambda () (emit-label l) (inst mov e status) (inst jmp done))
                         out-of-line)
                   l))
               (need (n tmp)
                 ;; at least N valid bits: a refill, from the tail near the end
                 (let ((skip (gen-label)) (slow (gen-label)) (back (gen-label)))
                   (inst cmp bits n)
                   (inst jmp :ae skip)
                   (inst cmp pos (stf decompress::+st-inlim+))
                   (inst jmp :g slow)
                   (inst mov tmp (ea in pos))
                   (emit-label back)
                   (inst shlx tmp tmp bits)
                   (inst or val tmp)
                   (inst mov tmp bits)          ; pos += (63 - bits) >> 3
                   (inst xor tmp 63)
                   (inst shr tmp 3)
                   (inst add pos tmp)
                   (inst or bits 56)
                   (emit-label skip)
                   (push (lambda ()
                           (emit-label slow)
                           (inst cmp pos (stf decompress::+st-inend+))
                           (inst jmp :g eoi)
                           (inst mov tmp pos)
                           (inst sub tmp (stf decompress::+st-tbase+))
                           (inst add tmp (stf decompress::+st-tail+))
                           (inst mov tmp (ea tmp))
                           (inst jmp back))
                         out-of-line)))
               (sym (tab)
                 ;; E = the next symbol of TAB (a tagged u32 vector); >= 15
                 ;; valid bits; uses U
                 (let ((sub (gen-label)) (back (gen-label)))
                   (inst mov u val)
                   (inst and u 255)
                   (inst mov :dword e (ea vd tab u 4))
                   (inst mov u e)
                   (inst and u 31)
                   (inst cmp u 16)
                   (inst jmp :ae sub)
                   (emit-label back)
                   (inst shrx val val u)
                   (inst sub bits u)
                   (inst shr e 5)
                   (push (lambda ()
                           (emit-label sub)
                           (inst shr val 8)
                           (inst sub bits 8)
                           (inst sub u 16)
                           (inst bzhi u val u)
                           (inst shr e 5)
                           (inst add u e)
                           (inst mov :dword e (ea vd tab u 4))
                           (inst mov u e)
                           (inst and u 31)
                           (inst jmp back))
                         out-of-line)))
               (take (dst n)
                 ;; DST = the next N (a register) bits
                 (inst bzhi dst val n)
                 (inst shrx val val n)
                 (inst sub bits n)))
        ;; P1, P2: the last two output bytes, 0 before the start
        (let ((go (gen-label)))
          (zeroize p1) (zeroize p2)
          (inst test fp fp)
          (inst jmp :z go)
          (inst movzx '(:byte :dword) p1 (ea -1 buf fp))
          (inst cmp fp 1)
          (inst jmp :e go)
          (inst movzx '(:byte :dword) p2 (ea -2 buf fp))
          (emit-label go))

        ;; ---- a command's top: the insert-and-copy symbol and lengths
        (emit-label top)
        (inst cmp fp (stf decompress::+st-target+))
        (inst jmp :ae (leave 0))
        (inst mov u (stf decompress::+st-icount+))
        (inst test u u)
        (inst jmp :z (leave 1))
        (inst sub u 1)
        (inst mov (stf decompress::+st-icount+) u)
        (need 15 u)
        (inst mov tt (stf decompress::+st-itab+))
        (sym tt)
        (inst mov (stf decompress::+st-cmd+) e)
        (need 48 u)                       ; both lengths' extra bits (<= 24 each)
        (inst mov tt (stf decompress::+st-cmdins+))
        (inst mov :dword tt (ea vd tt e 4))
        (inst mov u tt)
        (inst and u 31)
        (take e u)
        (inst shr tt 5)
        (inst add tt e)
        (inst mov (stf decompress::+st-ilen+) tt)
        (inst mov e (stf decompress::+st-cmd+))
        (inst mov u (stf decompress::+st-cmdcpy+))
        (inst mov :dword e (ea vd u e 4))
        (inst mov u e)
        (inst and u 31)
        (take tt u)
        (inst shr e 5)
        (inst add e tt)
        (inst mov (stf decompress::+st-clen+) e)
        (inst mov tt (stf decompress::+st-target+))
        (inst sub tt fp)
        (inst cmp tt (stf decompress::+st-ilen+))
        (inst jmp :b (leave 10))

        ;; ---- the literal run, one block's share at a time
        (emit-label litrun)
        (inst mov tt (stf decompress::+st-ilen+))
        (inst test tt tt)
        (inst jmp :z litdone)
        (inst mov u (stf decompress::+st-lcount+))
        (inst test u u)
        (inst jmp :z (leave 2))
        (inst cmp u tt)
        (inst cmov :a u tt)               ; U = min(lcount, ilen)
        (inst sub (stf decompress::+st-ilen+) u)
        (inst sub (stf decompress::+st-lcount+) u)
        (inst add u fp)
        (inst mov (stf decompress::+st-end+) u)
        (let ((lit (gen-label)))
          (emit-label lit)
          (need 15 u)
          ;; context id from P1, P2; its decode table from the block's slice
          (inst mov tt (stf decompress::+st-ctx+))
          (inst movzx '(:byte :dword) e (ea vd tt p1))
          (inst movzx '(:byte :dword) u (ea (+ vd 256) tt p2))
          (inst or e u)
          (inst mov tt (stf decompress::+st-slice+))
          (inst mov tt (ea vd tt e 8))
          (sym tt)
          (inst mov :byte (ea buf fp) e)
          (inst mov p2 p1)
          (inst mov p1 e)
          (inst add fp 1)
          (inst cmp fp (stf decompress::+st-end+))
          (inst jmp :b lit))
        (inst jmp litrun)

        ;; ---- the distance: P1 and P2 are free until the copy reloads them
        (emit-label litdone)
        (inst cmp fp (stf decompress::+st-target+))
        (inst jmp :ae (leave 0))
        (inst mov e (stf decompress::+st-cmd+))
        (inst cmp e 128)
        (inst jmp :b implicit)
        (inst mov u (stf decompress::+st-dcount+))
        (inst test u u)
        (inst jmp :z (leave 3))
        (inst sub u 1)
        (inst mov (stf decompress::+st-dcount+) u)
        ;; the table for the copy length's context: min(clen - 2, 3)
        (inst mov u (stf decompress::+st-clen+))
        (inst sub u 2)
        (inst mov tt 3)
        (inst cmp u tt)
        (inst cmov :a u tt)
        (inst mov tt (ea (* 8 decompress::+st-dtab+) st u 8))
        (need 15 u)
        (sym tt)                          ; E = distance code
        (let ((not-short (gen-label)) (long (gen-label)))
          (inst cmp e 16)
          (inst jmp :ae not-short)
          ;; ring slot + delta; a push unless the code is 0
          (inst mov p1 e)
          (inst mov u (stf decompress::+st-rbidx+))
          (inst add u (ea (* 8 decompress::+st-dsci+) st e 8))
          (inst and u 3)
          (inst mov tt (ea (* 8 decompress::+st-rb+) st u 8))
          (inst add tt (ea (* 8 decompress::+st-dscv+) st e 8))
          (inst jmp have)
          (emit-label not-short)
          (inst mov p1 1)
          (inst mov tt e)
          (inst sub tt 16)
          (inst cmp tt (stf decompress::+st-ndirect+))
          (inst jmp :ae long)
          (inst add tt 1)                   ; direct: dcode - 15
          (inst jmp have)
          (emit-label long)
          (inst sub tt (stf decompress::+st-ndirect+))   ; TT = d
          (inst mov e (stf decompress::+st-npostfix+))
          (inst lea u (ea 1 e))
          (inst shrx u tt u)
          (inst add u 1)                    ; U = ndistbits (<= 24)
          (need 24 p2)
          (take p1 u)                       ; P1 = the extra bits
          (inst shrx p2 tt e)               ; offset = ((2 + (hcode & 1)) << ndistbits) - 4
          (inst and p2 1)
          (inst add p2 2)
          (inst shlx p2 p2 u)
          (inst sub p2 4)
          (inst add p2 p1)
          (inst shlx p2 p2 e)
          (inst mov u (stf decompress::+st-pmask+))
          (inst and u tt)
          (inst add p2 u)
          (inst add p2 (stf decompress::+st-ndirect+))
          (inst lea tt (ea 1 p2))
          (inst mov p1 1)
          (inst jmp have))
        ;; the last distance, no push
        (emit-label implicit)
        (zeroize p1)
        (inst mov u (stf decompress::+st-rbidx+))
        (inst add u 3)
        (inst and u 3)
        (inst mov tt (ea (* 8 decompress::+st-rb+) st u 8))
        ;; TT = distance; P1 = 0 for no push.  Past min(window, fp): a word
        (emit-label have)
        (inst test tt tt)
        (inst jmp :le (leave 12))
        (inst mov u fp)
        (inst cmp u (stf decompress::+st-window+))
        (inst cmov :a u (stf decompress::+st-window+))
        (let ((dict (gen-label)))
          (inst cmp tt u)
          (inst jmp :a dict)
          (push (lambda ()
                  (emit-label dict)
                  (inst mov (stf decompress::+st-dist+) tt)
                  (inst mov e 4)
                  (inst jmp done))
                out-of-line))
        (inst test p1 p1)
        (inst jmp :z copy)
        (inst mov u (stf decompress::+st-rbidx+))
        (inst mov p2 u)
        (inst and u 3)
        (inst mov (ea (* 8 decompress::+st-rb+) st u 8) tt)
        (inst add p2 1)
        (inst mov (stf decompress::+st-rbidx+) p2)

        ;; ---- the copy, overshooting by up to 15 bytes into the slack
        (emit-label copy)
        (inst mov e (stf decompress::+st-clen+))
        (inst mov u (stf decompress::+st-target+))
        (inst sub u fp)
        (inst cmp u e)
        (inst jmp :b (leave 11))
        (inst mov u fp)
        (inst sub u tt)                   ; U = source
        (let ((by8 (gen-label)) (by16 (gen-label)) (near (gen-label)))
          (inst cmp tt 8)
          (inst jmp :b near)
          (inst cmp tt 16)
          (inst jmp :b by8)
          ;; dist >= 16: both loads of a step before its stores
          (emit-label by16)
          (inst mov p1 (ea buf u))
          (inst mov p2 (ea 8 buf u))
          (inst mov (ea buf fp) p1)
          (inst mov (ea 8 buf fp) p2)
          (inst add u 16)
          (inst add fp 16)
          (inst sub e 16)
          (inst jmp :g by16)
          (inst add fp e)                   ; back by the overshoot
          (inst jmp tail)
          ;; 8 <= dist < 16: each word loaded after the previous store
          (emit-label by8)
          (inst mov p1 (ea buf u))
          (inst mov (ea buf fp) p1)
          (inst add u 8)
          (inst add fp 8)
          (inst sub e 8)
          (inst jmp :g by8)
          (inst add fp e)
          (inst jmp tail)
          ;; dist < 8: 8-byte stores advancing DIST bytes
          (emit-label near)
          (inst mov p1 (ea buf u))
          (inst mov (ea buf fp) p1)
          (inst add u tt)
          (inst add fp tt)
          (inst sub e tt)
          (inst jmp :g near)
          (inst add fp e))
        (emit-label tail)
        (inst movzx '(:byte :dword) p1 (ea -1 buf fp))
        (inst movzx '(:byte :dword) p2 (ea -2 buf fp))
        (inst jmp top)
        (mapc #'funcall (reverse out-of-line))
        (emit-label done))))))))

;;; As INFLATE-FAST: the VOP's twelve registers pack only where little else is
;;; live, so it gets a function of its own.  It pins the objects whose
;;; addresses go into ST for this call.
#+decompress-avx512
(defun decompress::brotli-fast (val bits pos fp in buf tail st itab slice ctx d0 d1 d2 d3)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 64) val bits pos fp)
           (type (simple-array (unsigned-byte 8) (*)) in buf tail ctx)
           (type (simple-array (unsigned-byte 64) (*)) st)
           (type (simple-array (unsigned-byte 32) (*)) itab d0 d1 d2 d3)
           (type simple-vector slice))
  (let ((cmdins decompress::+cmd-ins-tab+) (cmdcpy decompress::+cmd-cpy-tab+))
    (sb-sys:with-pinned-objects (in buf tail itab slice ctx d0 d1 d2 d3 cmdins cmdcpy)
      (macrolet ((put (k x) `(setf (aref st ,k) (sb-kernel:get-lisp-obj-address ,x))))
        (put decompress::+st-itab+ itab)
        (put decompress::+st-slice+ slice)
        (put decompress::+st-ctx+ ctx)
        (put decompress::+st-dtab+ d0)
        (put decompress::+st-dtab1+ d1)
        (put decompress::+st-dtab2+ d2)
        (put decompress::+st-dtab3+ d3)
        (put decompress::+st-cmdins+ cmdins)
        (put decompress::+st-cmdcpy+ cmdcpy))
      (setf (aref st decompress::+st-tail+) (sb-sys:sap-int (sb-sys:vector-sap tail)))
      (decompress::%brotli-fast val bits pos fp
                                (sb-sys:vector-sap in) (sb-sys:vector-sap buf)
                                (sb-sys:vector-sap st)))))
