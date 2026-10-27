;;;; src/huffman.lisp — prefix codes (RFC 7932 sec. 3) and their decode tables.
;;;;
;;;; A decode table is a u32 vector: a 2^8-entry root indexed by the next 8
;;;; input bits, entry = sym<<5 | nbits.  Codes longer than 8 bits go through
;;;; a root entry with nbits = 16 + k that links to a 2^k-entry subtable at
;;;; offset sym, indexed by the following k bits, entry = sym<<5 | (len - 8).
;;;; Code-length codes (all <= 5 bits) use a one-level 32-entry table.
;;;;
;;;; Tables are built from the used symbols only, never by scanning the whole
;;;; alphabet: readers collect (symbol, length) pairs in symbol order together
;;;; with the count per length, as libbrotli does.
(in-package #:decompress)

(deftype u16vec () '(simple-array (unsigned-byte 16) (*)))
(deftype counts () '(simple-array (unsigned-byte 16) (16)))

(declaim (inline rev-bits))
(defun rev-bits (code len rev8)
  "The low LEN (<= 15) bits of CODE reversed; REV8 is +REV8+."
  (declare (type (unsigned-byte 16) code) (type (integer 1 15) len) (type u8vec rev8))
  (ash (logior (ash (aref rev8 (logand code 255)) 8) (aref rev8 (ash code -8))) (- len 16)))

(defun build-table (syms lens m count &optional (root-bits 8) map)
  "Decode table for the canonical code whose M used symbols are SYMS[i] with
lengths LENS[i] (1..15) in increasing symbol order, COUNT[l] of them of
length l.  The root has 2^ROOT-BITS (5..11) entries; codes longer than that
get second-level tables.  Decode with BR-SYM using the same ROOT-BITS.  With
MAP, entries hold MAP[symbol] instead of the symbol (a decoder can store what
it needs per symbol, e.g. a length base and extra-bit count)."
  ;; safety 0: indices are in range by construction (lengths <= 15, root slots
  ;; < 2^ROOT-BITS, subtables sized from their longest code); overlapping
  ;; (corrupt) codes are caught by the INVALID PREFIX CODE check
  (declare (type u16vec syms) (type u8vec lens) (type (integer 0 1024) m)
           (type counts count) (type (integer 5 11) root-bits)
           (type (or null u32vec) map)
           (optimize speed (safety 0)))
  (when (<= m 1)                                        ; zero-bit code
    (return-from build-table
      (make-array (ash 1 root-bits) :element-type '(unsigned-byte 32)
                                    :initial-element (if (= m 1)
                                                         (ash (if map (aref map (aref syms 0)) (aref syms 0)) 5)
                                                         0))))
  (let ((start (make-array 17 :element-type '(unsigned-byte 16) :initial-element 0))
        (sorted (make-array m :element-type '(unsigned-byte 16)))
        (revs (make-array m :element-type '(unsigned-byte 16)))
        (submax (make-array (ash 1 root-bits) :element-type '(unsigned-byte 8) :initial-element 0))
        (los (make-array (ash 1 root-bits) :element-type '(unsigned-byte 16)))  ; root slots with a subtable
        (nlos 0)
        (rmask (1- (ash 1 root-bits)))
        (total (ash 1 root-bits))
        (rev8 +rev8+))
    (declare (dynamic-extent start sorted revs submax los)
             (type (integer 0 2048) nlos rmask) (type (integer 0 #.(* 2048 129)) total))
    (loop for l from 1 to 15 do (setf (aref start (1+ l)) (+ (aref start l) (aref count l))))
    ;; counting sort by length; stable, so each length stays in symbol order
    (dotimes (i m)
      (let ((l (aref lens i)))
        (setf (aref sorted (aref start l)) (aref syms i))
        (incf (aref start l))))
    ;; canonical codes in (length, symbol) order, bit-reversed for LSB-first
    (let ((code 0) (i 0))
      (declare (type (unsigned-byte 17) code) (type (integer 0 1024) i))
      (loop for l of-type (integer 1 16) from 1 to 15 do
        (dotimes (k (aref count l))
          (let ((rev (rev-bits (logand code #xffff) l rev8)))
            (setf (aref revs i) rev)
            (when (> l root-bits)
              (let ((lo (logand rev rmask)))
                (when (zerop (aref submax lo)) (setf (aref los nlos) lo) (incf nlos))
                (setf (aref submax lo) (max (aref submax lo) l))))
            (incf i)
            (incf code)))
        (setf code (ash code 1))))
    (dotimes (j nlos)
      (incf total (ash 1 (logand 7 (- (aref submax (aref los j)) root-bits)))))
    (let ((tab (make-array total :element-type '(unsigned-byte 32) :initial-element 0))
          (off (ash 1 root-bits)) (i 0))
      (declare (type (integer 0 #.(* 2048 129)) off) (type (integer 0 1024) i))
      (dotimes (j nlos)
        (let* ((lo (aref los j)) (mx (aref submax lo)))
          (setf (aref tab lo) (logior (ash off 5) (+ 16 (logand 7 (- mx root-bits)))))
          (incf off (ash 1 (logand 7 (- mx root-bits))))))
      (loop for l of-type (integer 1 16) from 1 to 15 do
        (dotimes (k (aref count l))
          (let ((s (if map (aref map (aref sorted i)) (aref sorted i))) (rev (aref revs i)))
            (if (<= l root-bits)
                (loop for j of-type fixnum from rev below (ash 1 root-bits) by (ash 1 l)
                      do (when (>= (logand (aref tab j) 31) 16) (berr "invalid prefix code"))
                         (setf (aref tab j) (logior (ash s 5) l)))
                (let* ((e (aref tab (logand rev rmask)))
                       (sub (ash e -5)) (sb (logand 7 (- (logand e 31) 16))))
                  (loop for j of-type fixnum from (ash rev (- root-bits)) below (ash 1 sb)
                        by (ash 1 (logand 7 (- l root-bits)))
                        do (setf (aref tab (+ sub j)) (logior (ash s 5) (- l root-bits))))))
            (incf i))))
      tab)))

(defmacro with-code-arrays ((syms lens count n) &body body)
  "Stack-allocated SYMS, LENS (N entries) and COUNT for collecting a code."
  `(let ((,syms (make-array ,n :element-type '(unsigned-byte 16)))
         (,lens (make-array ,n :element-type '(unsigned-byte 8)))
         (,count (make-array 16 :element-type '(unsigned-byte 16) :initial-element 0)))
     (declare (dynamic-extent ,syms ,lens ,count))
     ,@body))

(declaim (inline alphabet-bits))
(defun alphabet-bits (size)
  (max 1 (integer-length (1- size))))

(defun read-simple-code (r nsym alphabet-size &optional (root-bits 8))
  "Simple prefix code (sec. 3.4): NSYM (1-4) symbols with fixed lengths."
  (declare (type reader r) (type (integer 1 4) nsym) (type (integer 1 1024) alphabet-size))
  (let ((s (make-array 4 :element-type '(unsigned-byte 16) :initial-element 0))
        (l (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0))
        (ab (alphabet-bits alphabet-size)))
    (declare (dynamic-extent s l))
    (dotimes (i nsym)
      (let ((sym (read-bits r ab)))
        (when (>= sym alphabet-size) (berr "prefix code symbol ~d out of range" sym))
        (setf (aref s i) sym)))
    (loop for i from 1 below nsym
          do (loop for j from 0 below i
                   do (when (= (aref s i) (aref s j)) (berr "duplicate prefix code symbol"))))
    (ecase nsym
      (1)
      (2 (fill l 1 :end 2))
      (3 (setf (aref l 0) 1 (aref l 1) 2 (aref l 2) 2))
      (4 (if (= 1 (read-bit r))
             (setf (aref l 0) 1 (aref l 1) 2 (aref l 2) 3 (aref l 3) 3)
             (fill l 2))))
    ;; canonical order wants each length's symbols ascending: sort the (at
    ;; most 4) pairs by symbol, then count
    (loop for i from 1 below nsym
          do (loop for j downfrom i above 0
                   while (< (aref s j) (aref s (1- j)))
                   do (rotatef (aref s j) (aref s (1- j)))
                      (rotatef (aref l j) (aref l (1- j)))))
    (with-code-arrays (syms lens count 4)
      (dotimes (i nsym)
        (setf (aref syms i) (aref s i) (aref lens i) (aref l i))
        (incf (aref count (aref l i))))
      (build-table syms lens nsym count root-bits))))

(defun read-complex-code (r hskip alphabet-size &optional (root-bits 8))
  "Complex prefix code (sec. 3.5): code lengths, themselves prefix coded."
  (declare (type reader r) (type (integer 0 3) hskip) (type (integer 1 1024) alphabet-size)
           (optimize speed (safety 0)))   ; every store is guarded by M < ALPHABET-SIZE
  ;; the code-length code: lengths 0-5 in a fixed variable-length code
  (let ((cl-table
          (with-code-arrays (syms lens count 18)
            (let ((cl-len (make-array 18 :element-type '(unsigned-byte 8) :initial-element 0))
                  (order +clcl-order+) (space 0) (m 0))
              (declare (dynamic-extent cl-len) (type fixnum space) (type (integer 0 18) m))
              (loop for i from hskip below 18 do
                ;; LSB-first: 0->00 3->10 4->01 2->011 1->0111 5->1111
                (let ((len (if (zerop (read-bit r))
                               (if (zerop (read-bit r)) 0 3)
                               (if (zerop (read-bit r)) 4
                                   (if (zerop (read-bit r)) 2
                                       (if (zerop (read-bit r)) 1 5))))))
                  (setf (aref cl-len (aref order i)) len)
                  (when (plusp len)
                    (incf space (ash 32 (- len)))
                    (when (>= space 32) (return)))))
              (dotimes (sym 18)
                (let ((len (aref cl-len sym)))
                  (when (plusp len)
                    (setf (aref syms m) sym (aref lens m) len)
                    (incf m)
                    (incf (aref count len)))))
              ;; complete, unless it is a single (zero-bit) code
              (unless (or (= m 1) (= space 32)) (berr "invalid code length code"))
              (build-table syms lens m count 5)))))
    ;; the symbol code lengths, with repeat codes 16 (previous length) and 17
    ;; (zeros); only the used symbols are recorded
    (with-code-arrays (syms lens count alphabet-size)
      (let ((m 0) (symbol 0) (prev-len 8) (repeat 0) (repeat-len 0) (space 32768))
        (declare (type (integer 0 1024) m) (type fixnum symbol prev-len repeat repeat-len space))
        (loop while (and (< symbol alphabet-size) (> space 0)) do
          (let ((c (with-reader (r) (br-sym-small cl-table 5))))
            (if (< c 16)
                (progn
                  (setf repeat 0)
                  (when (plusp c)
                    (setf (aref syms m) symbol (aref lens m) c)
                    (incf m)
                    (incf (aref count c))
                    (setf prev-len c)
                    (decf space (ash 32768 (- c))))
                  (incf symbol))
                (let ((extra (- c 14))
                      (new-len (if (= c 16) prev-len 0)))
                  (unless (= repeat-len new-len) (setf repeat 0 repeat-len new-len))
                  (let ((old repeat))
                    (when (plusp repeat) (setf repeat (ash (- repeat 2) extra)))
                    (incf repeat (+ (read-bits r extra) 3))
                    (let ((delta (- repeat old)))
                      (when (> (+ symbol delta) alphabet-size)
                        (berr "code length repeat past the alphabet"))
                      (if (plusp new-len)
                          (progn
                            (decf space (ash delta (- 15 new-len)))
                            (dotimes (k delta)
                              (when (< symbol alphabet-size)
                                (setf (aref syms m) symbol (aref lens m) new-len)
                                (incf m)
                                (incf (aref count new-len))
                                (incf symbol))))
                          (setf symbol (min alphabet-size (+ symbol delta))))))))))
        ;; the lengths must describe a complete code, no more and no less
        (unless (zerop space) (berr "incomplete or oversubscribed prefix code"))
        (build-table syms lens m count root-bits)))))

(defun read-prefix-code (r alphabet-size &optional (root-bits 8))
  "Read a prefix code over ALPHABET-SIZE symbols; return its decode table, with
a 2^ROOT-BITS-entry root."
  (let ((kind (read-bits r 2)))
    (if (= kind 1)
        (read-simple-code r (1+ (read-bits r 2)) alphabet-size root-bits)
        (read-complex-code r kind alphabet-size root-bits))))
