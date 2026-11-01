;;;; src/tables.lisp — the fixed tables of RFC 7932 and the static dictionary.
;;;;
;;;; Raw data (context lookup tables, word transforms, length/command/distance
;;;; code tables) as given by the RFC and libbrotli, then the derived forms the
;;;; decoder indexes.  All are load-time globals with declared types: a global
;;;; is one load, where a special variable also checks for a thread binding.
(in-package #:decompress)

(defmacro defglobal-table (name type value &optional doc)
  `(progn (sb-ext:define-load-time-global ,name ,value ,@(when doc (list doc)))
          (declaim (type ,type ,name))))

;;; ---- context modeling (RFC 7932 sec. 7.1) -------------------------------

(defglobal-table +lut0+ u8vec
  (coerce #(0 0 0 0 0 0 0 0 0 4 4 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8 12 16 12 12 20 12 16
        24 28 12 12 32 12 36 12 44 44 44 44 44 44 44 44 44 44 32 32 24 40 28 12 12 48 52 52 52
        48 52 52 52 48 52 52 52 52 52 48 52 52 52 52 52 48 52 52 52 52 52 24 12 28 12 12 12 56
        60 60 60 56 60 60 60 56 60 60 60 60 60 56 60 60 60 60 60 56 60 60 60 60 60 24 12 28 12 0
        0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
        0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3
        2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3 2 3)
          'u8vec))

(defglobal-table +lut1+ u8vec
  (coerce #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1
        1 1 1 1 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
        2 2 2 1 1 1 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 1 1 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
        0 0 0 0 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
          'u8vec))

(defglobal-table +lut2+ u8vec
  (coerce #(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
        2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
        3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4
        4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
        4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
        5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7)
          'u8vec))

;;; Per context mode M (0 LSB6, 1 MSB6, 2 UTF8, 3 Signed) a 512-byte table T
;;; with CID = T[p1] | T[256 + p2] for the previous two output bytes P1, P2.
(defglobal-table +ctx-mode-luts+ simple-vector
  (let ((v (make-array 4)))
    (dotimes (m 4 v)
      (let ((a (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
        (dotimes (p 256)
          (case m
            (0 (setf (aref a p) (logand p #x3f)))
            (1 (setf (aref a p) (ash p -2)))
            (2 (setf (aref a p) (aref +lut0+ p) (aref a (+ 256 p)) (aref +lut1+ p)))
            (3 (setf (aref a p) (ash (aref +lut2+ p) 3) (aref a (+ 256 p)) (aref +lut2+ p)))))
        (setf (svref v m) a)))))

;;; ---- insert-and-copy commands and block counts (sec. 5, 6) ---------------

(defglobal-table +ins-bits+ simple-vector   ; insert length extra bits
  #(0 0 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 7 8 9 10 12 14 24))

(defglobal-table +ins-base+ simple-vector   ; insert length base
  #(0 1 2 3 4 5 6 8 10 14 18 26 34 50 66 98 130 194 322 578 1090 2114 6210 22594))

(defglobal-table +cpy-bits+ simple-vector   ; copy length extra bits
  #(0 0 0 0 0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 7 8 9 10 24))

(defglobal-table +cpy-base+ simple-vector   ; copy length base
  #(2 3 4 5 6 7 8 9 10 12 14 18 22 30 38 54 70 102 134 198 326 582 1094 2118))

(defglobal-table +blen-bits+ simple-vector   ; block count extra bits
  #(2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 7 8 9 10 11 12 13 24))

(defglobal-table +blen-base+ simple-vector   ; block count base
  #(1 5 9 13 17 25 33 41 49 65 81 97 113 145 177 209 241 305 369 497 753 1265 2289 4337 8433
    16625))

(defglobal-table +cmd-ins+ simple-vector #(0 0 0 0 8 8 0 16 8 16 16))   ; command cell -> first insert code

(defglobal-table +cmd-cpy+ simple-vector #(0 8 0 8 0 8 16 0 16 8 16))   ; command cell -> first copy code

;;; Command symbol -> base<<5 | extra bits, for the insert and the copy length.
(defun cmd-table (cells base bits shift)
  (let ((a (make-array 704 :element-type '(unsigned-byte 32))))
    (dotimes (cmd 704 a)
      (let ((code (+ (svref cells (ash cmd -6)) (logand (ash cmd shift) 7))))
        (setf (aref a cmd) (logior (ash (svref base code) 5) (svref bits code)))))))
(defglobal-table +cmd-ins-tab+ u32vec (cmd-table +cmd-ins+ +ins-base+ +ins-bits+ -3))
(defglobal-table +cmd-cpy-tab+ u32vec (cmd-table +cmd-cpy+ +cpy-base+ +cpy-bits+ 0))
;;; Block count symbol -> base<<5 | extra bits.
(defglobal-table +blen-tab+ u32vec
  (coerce (map 'vector (lambda (b n) (logior (ash b 5) n)) +blen-base+ +blen-bits+) 'u32vec))

;;; ---- distances (sec. 4) ---------------------------------------------------
;;; Short codes 0..15: ring-buffer slot (relative to the write index) and delta.
(defglobal-table +dsc-index+ (simple-array fixnum (16))
  (coerce #(3 2 1 0 3 3 3 3 3 3 2 2 2 2 2 2) '(simple-array fixnum (16))))
(defglobal-table +dsc-value+ (simple-array fixnum (16))
  (coerce #(0 0 0 0 -1 1 -2 2 -3 3 -1 1 -2 2 -3 3) '(simple-array fixnum (16))))

;;; ---- prefix codes (sec. 3.5) ----------------------------------------------
;;; Order in which the code lengths of the code-length code are stored.
(defglobal-table +clcl-order+ u8vec
  (coerce #(1 2 3 4 0 5 17 6 16 7 8 9 10 11 12 13 14 15) 'u8vec))
;;; Byte bit reversal, for canonical codes read LSB-first.
(defglobal-table +rev8+ u8vec
  (let ((a (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (i 256 a)
      (setf (aref a i) (let ((r 0)) (dotimes (k 8 r) (setf r (logior (ash r 1) (ldb (byte 1 k) i)))))))))

;;; ---- static dictionary and word transforms (sec. 8) -----------------------
;;; Words of length L: 2^NDBITS[L] of them, starting at DOFFSET[L].
(defglobal-table +ndbits+ simple-vector #(0 0 0 0 10 10 11 11 10 10 10 10 10 9 9 8 7 7 8 7 7 6 6 5 5))
(defglobal-table +doffset+ simple-vector
  (let ((d (make-array 26 :initial-element 0)))
    (loop for l from 0 to 24
          do (setf (aref d (1+ l))
                   (+ (aref d l) (if (>= l 4) (* l (ash 1 (svref +ndbits+ l))) 0))))
    d))

(sb-ext:define-load-time-global *dict* nil)
(declaim (type (or null u8vec) *dict*))
(defun load-dictionary ()
  (setf *dict*
        (with-open-file (s (asdf:system-relative-pathname "decompress" "data/dictionary.bin")
                           :element-type '(unsigned-byte 8))
          (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
            (read-sequence v s)
            v))))
(declaim (inline dictionary))
(defun dictionary ()
  "The 122,784-byte static dictionary, read from data/ on first use."
  (or *dict* (load-dictionary)))

;;; The 121 transforms as #(prefix kind k suffix): KIND 0 identity, 1 upper-case
;;; first, 2 upper-case all, 3 omit the first K bytes, 4 omit the last K.
(defglobal-table +transforms+ simple-vector
  (map 'vector
       (lambda (tr)
         (destructuring-bind (prefix kind k suffix) tr
           (vector (coerce prefix 'u8vec) kind k (coerce suffix 'u8vec))))
       '((#() 0 0 #())
         (#() 0 0 #(32))
         (#(32) 0 0 #(32))
         (#() 3 1 #())
         (#() 1 0 #(32))
         (#() 0 0 #(32 116 104 101 32))
         (#(32) 0 0 #())
         (#(115 32) 0 0 #(32))
         (#() 0 0 #(32 111 102 32))
         (#() 1 0 #())
         (#() 0 0 #(32 97 110 100 32))
         (#() 3 2 #())
         (#() 4 1 #())
         (#(44 32) 0 0 #(32))
         (#() 0 0 #(44 32))
         (#(32) 1 0 #(32))
         (#() 0 0 #(32 105 110 32))
         (#() 0 0 #(32 116 111 32))
         (#(101 32) 0 0 #(32))
         (#() 0 0 #(34))
         (#() 0 0 #(46))
         (#() 0 0 #(34 62))
         (#() 0 0 #(10))
         (#() 4 3 #())
         (#() 0 0 #(93))
         (#() 0 0 #(32 102 111 114 32))
         (#() 3 3 #())
         (#() 4 2 #())
         (#() 0 0 #(32 97 32))
         (#() 0 0 #(32 116 104 97 116 32))
         (#(32) 1 0 #())
         (#() 0 0 #(46 32))
         (#(46) 0 0 #())
         (#(32) 0 0 #(44 32))
         (#() 3 4 #())
         (#() 0 0 #(32 119 105 116 104 32))
         (#() 0 0 #(39))
         (#() 0 0 #(32 102 114 111 109 32))
         (#() 0 0 #(32 98 121 32))
         (#() 3 5 #())
         (#() 3 6 #())
         (#(32 116 104 101 32) 0 0 #())
         (#() 4 4 #())
         (#() 0 0 #(46 32 84 104 101 32))
         (#() 2 0 #())
         (#() 0 0 #(32 111 110 32))
         (#() 0 0 #(32 97 115 32))
         (#() 0 0 #(32 105 115 32))
         (#() 4 7 #())
         (#() 4 1 #(105 110 103 32))
         (#() 0 0 #(10 9))
         (#() 0 0 #(58))
         (#(32) 0 0 #(46 32))
         (#() 0 0 #(101 100 32))
         (#() 3 9 #())
         (#() 3 7 #())
         (#() 4 6 #())
         (#() 0 0 #(40))
         (#() 1 0 #(44 32))
         (#() 4 8 #())
         (#() 0 0 #(32 97 116 32))
         (#() 0 0 #(108 121 32))
         (#(32 116 104 101 32) 0 0 #(32 111 102 32))
         (#() 4 5 #())
         (#() 4 9 #())
         (#(32) 1 0 #(44 32))
         (#() 1 0 #(34))
         (#(46) 0 0 #(40))
         (#() 2 0 #(32))
         (#() 1 0 #(34 62))
         (#() 0 0 #(61 34))
         (#(32) 0 0 #(46))
         (#(46 99 111 109 47) 0 0 #())
         (#(32 116 104 101 32) 0 0 #(32 111 102 32 116 104 101 32))
         (#() 1 0 #(39))
         (#() 0 0 #(46 32 84 104 105 115 32))
         (#() 0 0 #(44))
         (#(46) 0 0 #(32))
         (#() 1 0 #(40))
         (#() 1 0 #(46))
         (#() 0 0 #(32 110 111 116 32))
         (#(32) 0 0 #(61 34))
         (#() 0 0 #(101 114 32))
         (#(32) 2 0 #(32))
         (#() 0 0 #(97 108 32))
         (#(32) 2 0 #())
         (#() 0 0 #(61 39))
         (#() 2 0 #(34))
         (#() 1 0 #(46 32))
         (#(32) 0 0 #(40))
         (#() 0 0 #(102 117 108 32))
         (#(32) 1 0 #(46 32))
         (#() 0 0 #(105 118 101 32))
         (#() 0 0 #(108 101 115 115 32))
         (#() 2 0 #(39))
         (#() 0 0 #(101 115 116 32))
         (#(32) 1 0 #(46))
         (#() 2 0 #(34 62))
         (#(32) 0 0 #(61 39))
         (#() 1 0 #(44))
         (#() 0 0 #(105 122 101 32))
         (#() 2 0 #(46))
         (#(194 160) 0 0 #())
         (#(32) 0 0 #(44))
         (#() 1 0 #(61 34))
         (#() 2 0 #(61 34))
         (#() 0 0 #(111 117 115 32))
         (#() 2 0 #(44 32))
         (#() 1 0 #(61 39))
         (#(32) 1 0 #(44))
         (#(32) 2 0 #(61 34))
         (#(32) 2 0 #(44 32))
         (#() 2 0 #(44))
         (#() 2 0 #(40))
         (#() 2 0 #(46 32))
         (#(32) 2 0 #(46))
         (#() 2 0 #(61 39))
         (#(32) 2 0 #(46 32))
         (#(32) 1 0 #(61 34))
         (#(32) 2 0 #(61 39))
         (#(32) 1 0 #(61 39)))))
