;;;; src/decoder.lisp — meta-blocks, the command loop, and DECOMPRESS.
(in-package #:decompress)

(defconstant +out-slack+ 128
  "Spare bytes after the output: copies move 16 (or 64) bytes at a time and may
write past the end of a back-reference.")

;;; ---- meta-block header (RFC 7932 sec. 9.2) ------------------------------

(declaim (inline read-nbltypes read-block-count))
(defun read-nbltypes (r)
  "A count 1..256 (block types, trees) in the variable-length code of sec. 9.2."
  (if (zerop (read-bit r))
      1
      (let ((n (read-bits r 3)))
        (if (zerop n) 2 (+ (1+ (ash 1 n)) (read-bits r n))))))

(defun read-block-count (r ctab)
  (let ((c (aref +blen-tab+ (read-sym r ctab))))
    (+ (ash c -5) (read-bits r (logand c 31)))))

(sb-ext:define-load-time-global +zero-table+
  (make-array 256 :element-type '(unsigned-byte 32) :initial-element 0))
(declaim (type u32vec +zero-table+))

(defun read-block-switch-codes (r ntypes)
  "Block-type code, block-count code and first count of one category.  With a
single block type there is no switching: the count never runs out."
  (if (>= ntypes 2)
      (let* ((btab (read-prefix-code r (+ ntypes 2)))
             (ctab (read-prefix-code r 26)))
        (values btab ctab (read-block-count r ctab)))
      (values +zero-table+ +zero-table+ most-positive-fixnum)))

(defun read-context-map (r size ntrees)
  "Context map (sec. 7.3): SIZE tree indices, run-length coded zeros and an
optional inverse move-to-front."
  (let* ((rlemax (if (zerop (read-bit r)) 0 (1+ (read-bits r 4))))
         (tab (read-prefix-code r (+ ntrees rlemax)))
         (cmap (make-array size :element-type '(unsigned-byte 8) :initial-element 0))
         (i 0))
    (loop while (< i size) do
      (let ((sym (read-sym r tab)))
        (cond ((zerop sym) (incf i))
              ((<= sym rlemax)
               (incf i (+ (ash 1 sym) (read-bits r sym)))
               (when (> i size) (berr "context map run past its end")))
              (t (setf (aref cmap i) (- sym rlemax)) (incf i)))))
    (when (= 1 (read-bit r))
      (let ((mtf (make-array 256 :element-type '(unsigned-byte 8))))
        (dotimes (k 256) (setf (aref mtf k) k))
        (dotimes (k size)
          (let* ((idx (aref cmap k)) (v (aref mtf idx)))
            (setf (aref cmap k) v)
            (replace mtf mtf :start1 1 :end2 idx)
            (setf (aref mtf 0) v)))))
    (dotimes (k size cmap)
      (when (>= (aref cmap k) ntrees) (berr "context map entry out of range")))))

;;; ---- copies and dictionary words --------------------------------------------

;;; For a distance 1..7: the smallest multiple of it that is >= 8 (a table,
;;; since (* d (ceiling 8 d)) compiles to a division).
(defglobal-table +copy-period+ u8vec
  (coerce #(0 8 8 9 8 10 12 14) 'u8vec))

;;; One classification of (DIST, LEN) per back-reference, then a straight-line
;;; path per class that also sets P1/P2 (the last two output bytes, Brotli's
;;; literal context) the cheapest way it can; deflate passes no P1/P2.  May write up to 63 bytes past the end; BUF is pinned.
(defmacro copy-back-reference (buf fp dist len &optional p1 p2)
  `(let* ((%src (- ,fp ,dist)) (%end (+ %src ,len)))
     (declare (type index %src %end) (ignorable %end))
     (flet ((words16 (%from)
              ;; 16 bytes per step; reads only bytes already written
              (declare (type index %from))
              (loop for %i of-type index from 0 below ,len by 16
                    do (store64 ,buf (+ ,fp %i) (load64 ,buf (+ %from %i)))
                       (store64 ,buf (+ ,fp %i 8) (load64 ,buf (+ %from %i 8))))))
       (declare (inline words16))
       (cond
         ;; short, no overlap: both loads before either store, so nothing
         ;; waits on a just-issued store; last two bytes from the source
         ((and (>= ,dist 16) (<= ,len 16))
          (let ((%a (load64 ,buf %src)) (%b (load64 ,buf (+ %src 8))))
            ,@(when p1 `((setf ,p1 (aref ,buf (- %end 1)) ,p2 (aref ,buf (- %end 2)))))
            (store64 ,buf ,fp %a)
            (store64 ,buf (+ ,fp 8) %b)))
         ;; longer, no overlap
         ((>= ,dist ,len)
          ,@(when p1 `((setf ,p1 (aref ,buf (- %end 1)) ,p2 (aref ,buf (- %end 2)))))
          #+decompress-avx512
          (if (>= ,dist 64)
              (let ((%sap (sb-sys:vector-sap ,buf)))
                (loop for %i of-type index from 0 below ,len by 64
                      do (setf (sb-simd-avx512bw:u8.64-sap-ref %sap (+ ,fp %i))
                               (sb-simd-avx512bw:u8.64-sap-ref %sap (+ %src %i)))))
              (words16 %src))
          #-decompress-avx512
          (words16 %src))
         ;; overlapping (DIST < LEN): the output repeats every DIST bytes
         (t
          (if (>= ,dist 8)
              (words16 %src)
              ;; after the first P bytes (P = smallest multiple of DIST >= 8)
              ;; copy words at distance P
              (let ((%p (aref +copy-period+ ,dist)))
                (declare (type index %p))
                (dotimes (%i (min ,len %p)) (setf (aref ,buf (+ ,fp %i)) (aref ,buf (+ %src %i))))
                (loop for %i of-type index from %p below ,len by 8
                      do (store64 ,buf (+ ,fp %i) (load64 ,buf (+ ,fp (- %i %p)))))))
          ,@(when p1 `((setf ,p1 (aref ,buf (+ ,fp ,len -1)) ,p2 (aref ,buf (+ ,fp ,len -2))))))))
     (incf ,fp ,len)))

(declaim (inline uppercase-step))
(defun uppercase-step (buf p end)
  "Upper-case the UTF-8 character at P (sec. 8, 'ferment'); return its length."
  (declare (type u8vec buf) (type index p end))
  (let ((c (aref buf p)))
    (cond ((< c 192)
           (when (<= 97 c 122) (setf (aref buf p) (logxor c 32)))
           1)
          ((< c 224)
           (when (< (1+ p) end) (setf (aref buf (1+ p)) (logxor (aref buf (1+ p)) 32)))
           2)
          (t
           (when (< (+ p 2) end) (setf (aref buf (+ p 2)) (logxor (aref buf (+ p 2)) 5)))
           3))))

(defun dictionary-word (buf fp len dist max-dist target)
  "Write the transformed static-dictionary word for (LEN, DIST) at FP, straight
into BUF; return the new FP."
  (declare (type u8vec buf) (type index fp len dist max-dist target))
  (unless (<= 4 len 24) (berr "dictionary copy length ~d out of range" len))
  (let* ((nbits (svref +ndbits+ len))
         (word-id (- dist max-dist 1))
         (tid (ash word-id (- nbits)))
         (s (+ (svref +doffset+ len) (* (logand word-id (1- (ash 1 nbits))) len)))
         (e (+ s len)))
    (declare (type index s e))
    (when (> tid 120) (berr "transform id ~d > 120" tid))
    (let* ((tr (svref +transforms+ tid))
           (prefix (svref tr 0)) (kind (svref tr 1)) (k (svref tr 2)) (suffix (svref tr 3)))
      (declare (type u8vec prefix suffix) (type fixnum kind k))
      (case kind
        (3 (setf s (min e (+ s k))))
        (4 (setf e (max s (- e k)))))
      (let* ((w0 (+ fp (length prefix))) (w1 (+ w0 (- e s))) (end (+ w1 (length suffix))))
        (when (> end target) (berr "dictionary word overruns meta-block"))
        (replace buf prefix :start1 fp)
        (replace buf (dictionary) :start1 w0 :start2 s :end2 e)
        (replace buf suffix :start1 w1)
        (case kind
          (1 (when (< w0 w1) (uppercase-step buf w0 w1)))
          (2 (let ((i w0)) (loop while (< i w1) do (incf i (uppercase-step buf i w1))))))
        end))))

;;; ---- the command loop (sec. 9.3) ----------------------------------------

(defmacro define-loop-variants ((&rest variants) defun-form)
  "Compile DEFUN-FORM once per (NAME . BINDINGS), renamed to NAME and with each
constant symbol in BINDINGS replaced by its value, so each copy keeps only its
own code."
  (let ((name0 (second defun-form)))
    `(progn
       ,@(loop for (name . bindings) in variants
               collect (let ((form (subst name name0 defun-form :test #'eq)))
                         (loop for (sym val) on bindings by #'cddr
                               do (setf form (subst val sym form :test #'eq)))
                         form)))))

(define-loop-variants ((command-loop +big-window+ nil)
                       (command-loop/big-window +big-window+ t))
(defun command-loop (r buf fp target window rb npostfix ndirect cmode cmapl cmapd
                     htreel htreei htreed
                     nbl-l lbt lct lcount nbl-i ibt ict icount nbl-d dbt dct dcount)
  "Decode commands into BUF from FP up to TARGET.  The variant made with
+BIG-WINDOW+ = T is used when the window covers all output, so no distance
needs the window test."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type reader r)
           (type u8vec buf cmode cmapl cmapd)
           (type index fp target window ndirect nbl-l nbl-i nbl-d)
           (type (simple-array fixnum (5)) rb)
           (type (integer 0 3) npostfix)
           (type simple-vector htreel htreei htreed)
           (type u32vec lbt lct ibt ict dbt dct)
           (type fixnum lcount icount dcount))
  (let ((in (reader-in r))
        (limit (reader-limit r))
        (val (reader-val r))
        (bits (reader-bits r))
        (pos (reader-pos r))
        (cmd-ins +cmd-ins-tab+)
        (cmd-cpy +cmd-cpy-tab+)
        (blen +blen-tab+)
        (dsc-index +dsc-index+)
        (dsc-value +dsc-value+)
        (lt 0) (lp 1) (it 0) (ip 1) (dt 0) (dp 1)   ; block type and previous, per category
        (itab (svref htreei 0))
        ;; per literal block type: context id -> literal decode table
        (lslices (let ((v (make-array nbl-l)))
                   (dotimes (b nbl-l v)
                     (let ((sl (make-array 64)))
                       (dotimes (c 64)
                         (setf (svref sl c) (svref htreel (aref cmapl (+ (* 64 b) c)))))
                       (setf (svref v b) sl)))))
        (lslice nil)
        (lctx (svref +ctx-mode-luts+ (aref cmode 0)))
        (p1 (if (> fp 0) (aref buf (- fp 1)) 0))
        (p2 (if (> fp 1) (aref buf (- fp 2)) 0))
        (postfix-mask (1- (ash 1 npostfix)))
        #+decompress-avx512
        (st (make-array +st-size+ :element-type '(unsigned-byte 64) :initial-element 0)))
    #+decompress-avx512
    (declare (dynamic-extent st))
    (declare (type u8vec in lctx)
             (type index limit lt lp it ip dt dp)
             (type sb-ext:word pos bits)       ; full words: SBCL keeps them untagged
             (type (unsigned-byte 64) val)
             (type u32vec cmd-ins cmd-cpy blen itab)
             (type (simple-array fixnum (16)) dsc-index dsc-value)
             (type (unsigned-byte 8) p1 p2)
             (type fixnum postfix-mask)
             (type simple-vector lslices))
    (setf lslice (svref lslices 0))
    #+decompress-avx512
    (macrolet ((sets (&rest pairs)
                 `(setf ,@(loop for (k v) on pairs by #'cddr
                                collect `(aref st ,k) collect `(ldb (byte 64 0) ,v)))))
      (sets +st-target+ target +st-inlim+ (- limit 8) +st-inend+ (+ limit 8)
            +st-tbase+ (reader-tbase r)
            +st-window+ (if +big-window+ most-positive-fixnum window)
            +st-ndirect+ ndirect +st-npostfix+ npostfix +st-pmask+ postfix-mask)
      (dotimes (k 16)
        (sets (+ +st-dsci+ k) (aref dsc-index k) (+ +st-dscv+ k) (aref dsc-value k))))
    ;; one stage macro per step of a command; they expand in place
    (macrolet ((switch (ntypes type prev count btab ctab)
                 ;; block switch: new block type and count for one category
                 `(let* ((%s (br-sym ,btab))
                         (%new (cond ((= %s 0) ,prev)
                                     ((= %s 1) (let ((%x (1+ ,type))) (if (>= %x ,ntypes) 0 %x)))
                                     (t (- %s 2)))))
                    (declare (type index %new))
                    (when (>= %new ,ntypes) (berr "block type ~d out of range" %new))
                    (setf ,prev ,type ,type %new)
                    (let ((%c (aref blen (br-sym ,ctab))))
                      (setf ,count (+ (ash %c -5) (br-bits (logand %c 31)))))))
               #+decompress-avx512
               (fast-commands ()
                 ;; run the VOP from here: state into ST, back, and P1/P2 again
                 `(progn
                    (setf (aref st +st-icount+) icount (aref st +st-lcount+) lcount
                          (aref st +st-dcount+) dcount (aref st +st-rbidx+) (aref rb 4))
                    (dotimes (%k 4) (setf (aref st (+ +st-rb+ %k)) (aref rb %k)))
                    (multiple-value-bind (%v %b %p %f %s)
                        (brotli-fast val bits pos fp in buf (reader-tail r) st itab lslice lctx
                                     (svref htreed (aref cmapd (* 4 dt)))
                                     (svref htreed (aref cmapd (+ (* 4 dt) 1)))
                                     (svref htreed (aref cmapd (+ (* 4 dt) 2)))
                                     (svref htreed (aref cmapd (+ (* 4 dt) 3))))
                      (setf val %v bits %b pos %p fp %f
                            icount (aref st +st-icount+) lcount (aref st +st-lcount+)
                            dcount (aref st +st-dcount+) (aref rb 4) (aref st +st-rbidx+)
                            p1 (if (> fp 0) (aref buf (- fp 1)) 0)
                            p2 (if (> fp 1) (aref buf (- fp 2)) 0))
                      (dotimes (%k 4) (setf (aref rb %k) (aref st (+ +st-rb+ %k))))
                      %s)))
               (max-distance ()
                 ;; farthest real back-reference; past it, a dictionary word
                 `(if +big-window+ fp (min window fp)))
               (literal-run ()
                 ;; ILEN literals, block by block
                 `(loop while (> ilen 0) do
                    (when (zerop lcount)
                      (switch nbl-l lt lp lcount lbt lct)
                      (setf lslice (svref lslices lt)
                            lctx (svref +ctx-mode-luts+ (aref cmode lt))))
                    (let ((k (min ilen lcount)) (ctx lctx) (slice lslice))
                      (declare (type index k) (type u8vec ctx) (type simple-vector slice))
                      ;; byte accesses through SAPs: a SAP offset is a raw
                      ;; integer, so FP/P1/P2 stay untagged
                      (sb-sys:with-pinned-objects (ctx)
                        (let ((cs (sb-sys:vector-sap ctx))
                              (bs (sb-sys:vector-sap buf)))
                          (loop repeat k do
                            (let* ((cid (logior (sb-sys:sap-ref-8 cs p1)
                                                (sb-sys:sap-ref-8 cs (+ 256 p2))))
                                   (lit (br-sym (the u32vec (svref slice cid)) (br-refill))))
                              (setf (sb-sys:sap-ref-8 bs fp) lit p2 p1 p1 lit)
                              (incf fp)))))
                      (decf ilen k)
                      (decf lcount k))))
               (explicit-distance ()
                 ;; distance code (context: copy length) -> DISTANCE; real
                 ;; back-references go onto the ring of last distances
                 `(progn
                    (when (zerop dcount)
                      (switch nbl-d dt dp dcount dbt dct))
                    (decf dcount)
                    (let ((dcode (br-sym (the u32vec
                                              (svref htreed (aref cmapd (+ (* 4 dt)
                                                                           (if (> clen 4) 3 (- clen 2)))))))))
                      (declare (type index dcode))
                      (cond
                        ((< dcode 16)
                         (setf distance (+ (aref rb (logand (+ idx (aref dsc-index dcode)) 3))
                                           (aref dsc-value dcode))))
                        ((< dcode (+ 16 ndirect)) (setf distance (- dcode 15)))
                        (t (let* ((d (- dcode ndirect 16))
                                  (ndistbits (+ 1 (ash d (- (1+ npostfix)))))
                                  (dextra (br-bits ndistbits))
                                  (hcode (ash d (- npostfix)))
                                  (offset (- (ash (+ 2 (logand hcode 1)) ndistbits) 4)))
                             (declare (type (integer 0 32) ndistbits))
                             (setf distance (+ (ash (+ offset dextra) npostfix)
                                               (logand d postfix-mask) ndirect 1)))))
                      (when (< distance 1) (berr "invalid distance"))
                      (when (and (/= dcode 0) (<= distance (max-distance)))
                        (setf (aref rb (logand idx 3)) distance
                              (aref rb 4) (1+ idx))))))
               (copy-or-dictionary ()
                 ;; CLEN bytes from DISTANCE back, or a static dictionary word
                 `(let ((max-dist (max-distance)))
                    (if (<= distance max-dist)
                        (progn
                          (when (> clen (- target fp)) (berr "copy length overruns meta-block"))
                          (copy-back-reference buf fp distance clen p1 p2))
                        (progn
                          (setf fp (dictionary-word buf fp clen distance max-dist target))
                          (setf p1 (if (> fp 0) (aref buf (- fp 1)) 0)
                                p2 (if (> fp 1) (aref buf (- fp 2)) 0)))))))
      (loop while (< fp target) do
        ;; STATUS 1: a whole command in Lisp; 2-4: the rest of the command
        ;; the VOP stopped in (from the literal run, the distance or the copy)
        (let ((status 1) (cmd 0) (ilen 0) (clen 0) (distance 0))
          (declare (type (unsigned-byte 64) status) (type index cmd ilen clen)
                   (type fixnum distance))
          #+decompress-avx512
          (progn
            (setf status (fast-commands))
            (case status
              ((0) (return))
              ((1 2 3 4))
              (10 (berr "insert length overruns meta-block"))
              (11 (berr "copy length overruns meta-block"))
              (12 (berr "invalid distance"))
              (t (berr "end of input")))
            (when (> status 1)
              (setf cmd (aref st +st-cmd+) ilen (aref st +st-ilen+) clen (aref st +st-clen+))
              (when (= status 4) (setf distance (aref st +st-dist+)))))
          (when (= status 1)
            (when (zerop icount)
              (switch nbl-i it ip icount ibt ict)
              (setf itab (svref htreei it)))
            (decf icount)
            ;; insert-and-copy command: insert length, copy length, implicit distance?
            (setf cmd (br-sym itab))
            (let ((ie (aref cmd-ins cmd))
                  (ce (aref cmd-cpy cmd)))
              (setf ilen (+ (ash ie -5) (br-bits (logand ie 31)))
                    clen (+ (ash ce -5) (br-bits (logand ce 31)))))
            (when (> ilen (- target fp)) (berr "insert length overruns meta-block")))
          (literal-run)
          (when (>= fp target) (return))
          (let ((idx (aref rb 4)))
            (declare (type fixnum idx))
            (cond ((= status 4))          ; a dictionary word: DISTANCE is known
                  ((< cmd 128)
                   (setf distance (aref rb (logand (+ idx 3) 3))))   ; last distance, no push
                  (t (explicit-distance)))
            (copy-or-dictionary)))))
    (setf (reader-val r) val (reader-bits r) bits (reader-pos r) pos)
    (when (/= fp target) (berr "meta-block produced wrong length"))
    fp)))

(defun compressed-meta-block (r buf fp mlen window rb)
  "Decode one compressed meta-block of MLEN bytes into BUF at FP (BUF has room
for MLEN + slack).  Returns the new FP."
  (declare (type reader r) (type u8vec buf) (type index fp mlen window)
           (type (simple-array fixnum (5)) rb))
  (let* ((target (+ fp mlen))
         (nbl-l (read-nbltypes r)))
    (multiple-value-bind (lbt lct lcount) (read-block-switch-codes r nbl-l)
      (let ((nbl-i (read-nbltypes r)))
        (multiple-value-bind (ibt ict icount) (read-block-switch-codes r nbl-i)
          (let ((nbl-d (read-nbltypes r)))
            (multiple-value-bind (dbt dct dcount) (read-block-switch-codes r nbl-d)
              (let* ((npostfix (read-bits r 2))
                     (ndirect (ash (read-bits r 4) npostfix))
                     (cmode (make-array nbl-l :element-type '(unsigned-byte 8))))
                (dotimes (i nbl-l) (setf (aref cmode i) (read-bits r 2)))
                (let* ((ntreesl (read-nbltypes r))
                       (cmapl (if (>= ntreesl 2)
                                  (read-context-map r (* 64 nbl-l) ntreesl)
                                  (make-array (* 64 nbl-l) :element-type '(unsigned-byte 8)
                                                           :initial-element 0)))
                       (ntreesd (read-nbltypes r))
                       (cmapd (if (>= ntreesd 2)
                                  (read-context-map r (* 4 nbl-d) ntreesd)
                                  (make-array (* 4 nbl-d) :element-type '(unsigned-byte 8)
                                                          :initial-element 0)))
                       (htreel (make-array ntreesl))
                       (htreei (make-array nbl-i))
                       (htreed (make-array ntreesd))
                       (dist-alphabet (+ 16 ndirect (ash 48 npostfix))))
                  (dotimes (i ntreesl) (setf (svref htreel i) (read-prefix-code r 256)))
                  (dotimes (i nbl-i) (setf (svref htreei i) (read-prefix-code r 704)))
                  (dotimes (i ntreesd) (setf (svref htreed i) (read-prefix-code r dist-alphabet)))
                  (sb-sys:with-pinned-objects (buf)
                    (macrolet ((run (name)
                                 `(,name r buf fp target window rb npostfix ndirect cmode cmapl cmapd
                                         htreel htreei htreed
                                         nbl-l lbt lct lcount nbl-i ibt ict icount nbl-d dbt dct dcount)))
                      (if (>= window target)
                          (run command-loop/big-window)
                          (run command-loop)))))))))))))

;;; ---- the stream (sec. 9.1) ------------------------------------------------

(declaim (inline read-window-bits))
(defun read-window-bits (r)
  (if (zerop (read-bit r))
      16
      (let ((n (read-bits r 3)))
        (if (plusp n)
            (+ 17 n)
            (let ((m (read-bits r 3)))
              (case m (0 17) (1 (berr "invalid window size")) (t (+ 8 m))))))))

(defun grow (buf fp n)
  "BUF with room for N more bytes (plus slack) after FP."
  (declare (type u8vec buf) (type index fp n))
  (let ((need (+ fp n +out-slack+)))
    (if (<= need (length buf))
        buf
        (let ((new (make-array (max need (* 2 (length buf))) :element-type '(unsigned-byte 8))))
          (replace new buf :end2 fp)
          new))))

(defun decompress (input &key size-hint (start 0) end)
  "Decode the brotli stream INPUT (octets, RFC 7932); return the decoded octets.
START and END bound the stream within INPUT, which is read in place.
SIZE-HINT, the decoded size if known (WOFF2 records it), lets the output be
allocated once.  Signals BROTLI-ERROR on malformed input."
  (let* ((in (if (typep input 'u8vec) input (coerce input 'u8vec)))
         (len (bounded-end in start end))
         (r (make-reader in :start start :end len))
         (tail (reader-tail r)))
    (sb-sys:with-pinned-objects (in tail)
      (let* ((window (- (ash 1 (read-window-bits r)) 16))
             (buf (make-array (+ (or size-hint (max 4096 (* 4 (- len start)))) +out-slack+)
                              :element-type '(unsigned-byte 8)))
             (fp 0)
             ;; ring of the last four distances + write index, initially 4,11,15,16
             (rb (make-array 5 :element-type 'fixnum :initial-contents '(16 15 11 4 4))))
        (declare (type u8vec buf) (type index fp))
        (loop
          (let ((islast (read-bit r)))
            (when (and (= islast 1) (= (read-bit r) 1)) (return))  ; ISLASTEMPTY
            (let ((mnibbles (read-bits r 2)))
              (if (= mnibbles 3)
                  ;; metadata: skipped
                  (progn
                    (when (plusp (read-bit r)) (berr "reserved bit set"))
                    (let ((nbytes (read-bits r 2)))   ; size of the length field
                      (if (zerop nbytes)
                          (align r)
                          (let ((n (1+ (read-bits r (* 8 nbytes)))))
                            (align r)
                            (skip-bytes r n)))))
                  (let ((mlen (1+ (read-bits r (* 4 (+ mnibbles 4))))))
                    (setf buf (grow buf fp mlen))
                    (if (and (= islast 0) (= (read-bit r) 1))   ; ISUNCOMPRESSED
                        (progn
                          (align r)
                          (let ((p (byte-pos r)))
                            (when (> (+ p mlen) len) (berr "end of input"))
                            (replace buf in :start1 fp :start2 p :end2 (+ p mlen))
                            (seek r (+ p mlen))
                            (incf fp mlen)))
                        (setf fp (compressed-meta-block r buf fp mlen window rb))))))
            (when (= islast 1) (return))))
        ;; the rest of the last byte is padding and must be zero
        (align r)
        (when (> (- (* 8 (reader-pos r)) (reader-bits r)) (* 8 len)) (berr "end of input"))
        ;; give back the unused tail in place instead of copying the output
        (sb-kernel:%shrink-vector buf fp)))))
