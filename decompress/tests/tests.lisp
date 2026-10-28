;;;; tests/tests.lisp — decompress against libbrotli as the oracle.
;;;;
;;;;   (asdf:test-system "decompress")       or   sbcl --script tests/run.lisp
;;;;
;;;; 1. Reference vectors: tests/vectors/NAME.*.br decode to NAME.raw.
;;;; 2. Round trips: inputs compressed by libbrotlienc at every quality (0-11),
;;;;    window (10-24) and mode (generic, text, font) decode to the input, with
;;;;    and without :SIZE-HINT.
;;;; 3. Corrupt input: those streams with flipped bits or cut short.  Where
;;;;    libbrotlidec decodes a stream we must produce the same bytes; where it
;;;;    rejects one we must signal BROTLI-ERROR (never crash, never return).
;;;; 4. zlib/deflate round trips: inputs compressed by libz at every level
;;;;    (0-9), window (9-15), memory level (1-9) and strategy (default,
;;;;    filtered, Huffman only, RLE, fixed codes), as zlib streams and as raw
;;;;    deflate, decode to the input, with and without :SIZE-HINT.
;;;; 5. Corrupt zlib streams, judged by libz's uncompress as in 3 (mostly
;;;;    caught by the Adler-32 check).
;;;; 6. Corrupt raw deflate (no checksum, so the decoder's own checks decide),
;;;;    judged by libz's inflate.
;;;; A fixed-seed generator makes every run identical.  Needs libbrotlienc,
;;;; libbrotlidec (the brotli package) and libz; LIBBROTLIENC, LIBBROTLIDEC
;;;; and LIBZ override their paths.

(defpackage #:decompress/tests
  (:use #:cl)
  (:export #:run))
(in-package #:decompress/tests)

(sb-alien:load-shared-object (or (sb-ext:posix-getenv "LIBBROTLIENC") "libbrotlienc.so"))
(sb-alien:load-shared-object (or (sb-ext:posix-getenv "LIBBROTLIDEC") "libbrotlidec.so"))
(sb-alien:load-shared-object (or (sb-ext:posix-getenv "LIBZ") "libz.so"))

(deftype u8vec () '(simple-array (unsigned-byte 8) (*)))

;;; BROTLI_BOOL BrotliEncoderCompress(int quality, int lgwin, BrotliEncoderMode
;;;     mode, size_t input_size, const uint8_t *input, size_t *encoded_size,
;;;     uint8_t *encoded);
(sb-alien:define-alien-routine ("BrotliEncoderCompress" %encoder-compress) sb-alien:int
  (quality sb-alien:int) (lgwin sb-alien:int) (mode sb-alien:int)
  (input-size sb-alien:unsigned-long) (input sb-sys:system-area-pointer)
  (encoded-size (* sb-alien:unsigned-long)) (encoded sb-sys:system-area-pointer))

;;; BrotliDecoderResult BrotliDecoderDecompress(size_t encoded_size, const
;;;     uint8_t *encoded, size_t *decoded_size, uint8_t *decoded);
(sb-alien:define-alien-routine ("BrotliDecoderDecompress" %decoder-decompress) sb-alien:int
  (encoded-size sb-alien:unsigned-long) (encoded sb-sys:system-area-pointer)
  (decoded-size (* sb-alien:unsigned-long)) (decoded sb-sys:system-area-pointer))

(defun c-compress (data quality lgwin mode)
  (let* ((cap (+ 1024 (* 2 (length data))))
         (out (make-array cap :element-type '(unsigned-byte 8))))
    (sb-alien:with-alien ((size sb-alien:unsigned-long cap))
      (sb-sys:with-pinned-objects (data out)
        (unless (= 1 (%encoder-compress quality lgwin mode (length data) (sb-sys:vector-sap data)
                                        (sb-alien:addr size) (sb-sys:vector-sap out)))
          (error "libbrotlienc failed")))
      (subseq out 0 size))))

(defconstant +c-out-cap+ (* 32 1024 1024))
(defvar *c-out* nil)

(defun c-decompress (stream)
  "libbrotlidec's verdict: decoded octets, :ERROR, or :TOO-BIG (inconclusive)."
  (let ((out (or *c-out* (setf *c-out* (make-array +c-out-cap+ :element-type '(unsigned-byte 8))))))
    (sb-alien:with-alien ((size sb-alien:unsigned-long +c-out-cap+))
      (sb-sys:with-pinned-objects (stream out)
        (let ((result (%decoder-decompress (length stream) (sb-sys:vector-sap stream)
                                           (sb-alien:addr size) (sb-sys:vector-sap out))))
          (cond ((/= result 1) (if (>= size +c-out-cap+) :too-big :error))
                ((>= size +c-out-cap+) :too-big)
                (t (subseq out 0 size))))))))

;;; ---- deterministic inputs ------------------------------------------------

(defvar *seed* 88172645463325252)
(defun rnd (n)
  "xorshift64: a number below N."
  (let ((x *seed*))
    (setf x (logxor x (ldb (byte 64 0) (ash x 13)))
          x (logxor x (ash x -7))
          x (logxor x (ldb (byte 64 0) (ash x 17)))
          *seed* x)
    (mod x n)))

(defun octets (list) (coerce list 'u8vec))

(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defvar *text*
  ;; the reference originals, concatenated: fixed text and markup, so the
  ;; inputs do not change when the library's own source does
  (apply #'concatenate 'u8vec
         (mapcar #'slurp (sort (directory (merge-pathnames "tests/vectors/*.raw"
                                                          (asdf:system-source-directory "decompress")))
                               #'string< :key #'namestring))))

(defun make-input (kind n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (ecase kind
      (:zeros)
      (:random (dotimes (i n) (setf (aref v i) (rnd 256))))
      (:text (let ((start (rnd (max 1 (- (length *text*) n)))))
               (dotimes (i n) (setf (aref v i) (aref *text* (mod (+ start i) (length *text*)))))))
      (:dictionary                       ; binary-ish: slices of the static dictionary
       (let ((d (decompress::dictionary)))
         (dotimes (i n) (setf (aref v i) (aref d (mod (+ (* 4096 (floor i 4096)) (rnd 7) i) (length d)))))))
      (:runs (let ((i 0))
               (loop while (< i n)
                     do (let ((b (rnd 256)) (k (1+ (rnd 300))))
                          (dotimes (j (min k (- n i))) (setf (aref v (+ i j)) b))
                          (incf i k)))))
      (:pattern (let ((p (1+ (rnd 40))))   ; short-period repeats: overlapping copies
                  (dotimes (i n) (setf (aref v i) (if (< i p) (rnd 256) (aref v (- i p))))))))
    v))

;;; ---- checks ---------------------------------------------------------------

(defvar *pass* 0)
(defvar *fail* 0)
(defun check (ok fmt &rest args)
  (if ok
      (incf *pass*)
      (progn (incf *fail*)
             (when (< *fail* 20) (format t "  FAIL ~?~%" fmt args)))))

(defun ours (stream &rest keys)
  "Our verdict: decoded octets, or :ERROR on BROTLI-ERROR; other errors propagate."
  (handler-case (apply #'decompress:decompress stream keys)
    (decompress:brotli-error () :error)))

(defun test-vectors ()
  (let ((dir (asdf:system-relative-pathname "decompress" "tests/vectors/")))
    (dolist (br (directory (merge-pathnames "*.br" dir)))
      (let* ((name (pathname-name br))
             (raw (slurp (merge-pathnames (format nil "~a.raw" (subseq name 0 (position #\. name))) dir))))
        (check (equalp (ours (slurp br)) raw) "vector ~a" name)))))

(defvar *streams* '() "Compressed streams from the round trips, for corruption.")

(defun test-round-trips (count)
  (dotimes (k count)
    (let* ((kind (nth (rnd 6) '(:zeros :random :text :dictionary :runs :pattern)))
           (n (case (rnd 4) (0 (rnd 64)) (1 (rnd 4096)) (2 (rnd 65536)) (t (rnd 262144))))
           (quality (rnd 12)) (lgwin (+ 10 (rnd 15))) (mode (rnd 3))
           (data (make-input kind n))
           (stream (c-compress data quality lgwin mode)))
      (when (< (length stream) 20000) (push stream *streams*))
      (check (equalp (ours stream) data)
             "round trip ~a ~d B q~d w~d mode ~d" kind n quality lgwin mode)
      (check (equalp (ours stream :size-hint n) data)
             "round trip with size hint ~a ~d B q~d w~d mode ~d" kind n quality lgwin mode))))

(defun corrupt (stream)
  (let ((s (copy-seq stream)))
    (if (and (> (length s) 1) (zerop (rnd 4)))
        (subseq s 0 (rnd (length s)))                       ; cut short
        (progn (dotimes (i (1+ (rnd 3)))                    ; flip 1-3 bits
                 (when (plusp (length s))
                   (let ((p (rnd (length s))))
                     (setf (aref s p) (logxor (aref s p) (ash 1 (rnd 8)))))))
               s))))

(defun test-corrupt (count)
  (let ((streams (coerce *streams* 'vector)) (rejected 0) (accepted 0) (skipped 0))
    (dotimes (k count)
      (let* ((bad (corrupt (aref streams (rnd (length streams)))))
             (c (c-decompress bad)))
        (if (eq c :too-big)
            (incf skipped)
            (let ((ours (ours bad)))
              (if (eq c :error) (incf rejected) (incf accepted))
              (check (equalp ours c) "corrupt stream: libbrotli ~a, ours ~a"
                     (if (eq c :error) "rejects" "decodes")
                     (if (eq ours :error) "rejects" (if (equalp ours c) "matches" "differs")))))))
    (format t "  (libbrotli rejected ~d, decoded ~d, inconclusive ~d)~%" rejected accepted skipped)))

;;; ---- zlib / deflate --------------------------------------------------------

;;; z_stream (LP64): next_in 0, avail_in 8, total_in 16, next_out 24,
;;; avail_out 32, total_out 40, msg 48, state 56, zalloc 64, zfree 72,
;;; opaque 80, data_type 88, adler 96, reserved 104; 112 bytes.
(defconstant +z-stream-size+ 112)
(sb-alien:define-alien-routine ("zlibVersion" zlib-version) sb-alien:c-string)
(sb-alien:define-alien-routine ("deflateInit2_" %deflate-init2) sb-alien:int
  (strm sb-sys:system-area-pointer) (level sb-alien:int) (method sb-alien:int)
  (window-bits sb-alien:int) (mem-level sb-alien:int) (strategy sb-alien:int)
  (version sb-alien:c-string) (stream-size sb-alien:int))
(sb-alien:define-alien-routine ("deflate" %deflate) sb-alien:int
  (strm sb-sys:system-area-pointer) (flush sb-alien:int))
(sb-alien:define-alien-routine ("deflateEnd" %deflate-end) sb-alien:int
  (strm sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("uncompress" %uncompress) sb-alien:int
  (dest sb-sys:system-area-pointer) (dest-len (* sb-alien:unsigned-long))
  (source sb-sys:system-area-pointer) (source-len sb-alien:unsigned-long))

(defun z-compress (data level window-bits mem-level strategy)
  "DATA deflated by libz; negative WINDOW-BITS gives raw deflate."
  (let* ((cap (+ 1024 (* 2 (length data))))
         (out (make-array cap :element-type '(unsigned-byte 8)))
         (strm (sb-alien:make-alien (sb-alien:unsigned 8) +z-stream-size+))
         (sap (sb-alien:alien-sap strm)))
    (unwind-protect
         (progn
           (dotimes (i +z-stream-size+) (setf (sb-sys:sap-ref-8 sap i) 0))
           (unless (zerop (%deflate-init2 sap level 8 window-bits mem-level strategy
                                          (zlib-version) +z-stream-size+))
             (error "deflateInit2 failed"))
           (sb-sys:with-pinned-objects (data out)
             (setf (sb-sys:sap-ref-sap sap 0) (sb-sys:vector-sap data)
                   (sb-sys:sap-ref-32 sap 8) (length data)
                   (sb-sys:sap-ref-sap sap 24) (sb-sys:vector-sap out)
                   (sb-sys:sap-ref-32 sap 32) cap)
             (unless (= 1 (%deflate sap 4)) (error "deflate did not finish")))
           (prog1 (subseq out 0 (sb-sys:sap-ref-64 sap 40))
             (%deflate-end sap)))
      (sb-alien:free-alien strm))))

(defun z-uncompress (stream)
  "libz's verdict on a zlib stream: decoded octets, :ERROR, or :TOO-BIG."
  (let ((out (or *c-out* (setf *c-out* (make-array +c-out-cap+ :element-type '(unsigned-byte 8))))))
    (sb-alien:with-alien ((size sb-alien:unsigned-long +c-out-cap+))
      (sb-sys:with-pinned-objects (stream out)
        (let ((result (%uncompress (sb-sys:vector-sap out) (sb-alien:addr size)
                                   (sb-sys:vector-sap stream) (length stream))))
          (cond ((>= size +c-out-cap+) :too-big)
                ((/= result 0) :error)
                (t (subseq out 0 size))))))))

(sb-alien:define-alien-routine ("inflateInit2_" %inflate-init2) sb-alien:int
  (strm sb-sys:system-area-pointer) (window-bits sb-alien:int)
  (version sb-alien:c-string) (stream-size sb-alien:int))
(sb-alien:define-alien-routine ("inflate" %inflate) sb-alien:int
  (strm sb-sys:system-area-pointer) (flush sb-alien:int))
(sb-alien:define-alien-routine ("inflateEnd" %inflate-end) sb-alien:int
  (strm sb-sys:system-area-pointer))

(defun z-inflate-raw (stream)
  "libz's verdict on raw deflate data: decoded octets, :ERROR, or :TOO-BIG."
  (let ((out (or *c-out* (setf *c-out* (make-array +c-out-cap+ :element-type '(unsigned-byte 8)))))
        (strm (sb-alien:make-alien (sb-alien:unsigned 8) +z-stream-size+)))
    (let ((sap (sb-alien:alien-sap strm)))
      (unwind-protect
           (progn
             (dotimes (i +z-stream-size+) (setf (sb-sys:sap-ref-8 sap i) 0))
             (unless (zerop (%inflate-init2 sap -15 (zlib-version) +z-stream-size+))
               (error "inflateInit2 failed"))
             (sb-sys:with-pinned-objects (stream out)
               (setf (sb-sys:sap-ref-sap sap 0) (sb-sys:vector-sap stream)
                     (sb-sys:sap-ref-32 sap 8) (length stream)
                     (sb-sys:sap-ref-sap sap 24) (sb-sys:vector-sap out)
                     (sb-sys:sap-ref-32 sap 32) +c-out-cap+)
               (let ((result (%inflate sap 4))
                     (n (sb-sys:sap-ref-64 sap 40)))
                 (%inflate-end sap)
                 (cond ((>= n +c-out-cap+) :too-big)
                       ((/= result 1) :error)          ; Z_STREAM_END = success
                       (t (subseq out 0 n))))))
        (sb-alien:free-alien strm)))))

(defun ours-z (fn stream &rest keys)
  (handler-case (apply fn stream keys)
    (decompress:deflate-error () :error)))

(defvar *z-streams* '() "zlib streams from the round trips, for corruption.")
(defvar *raw-streams* '() "Raw deflate streams from the round trips, for corruption.")

(defun test-zlib-round-trips (count)
  (dotimes (k count)
    (let* ((kind (nth (rnd 6) '(:zeros :random :text :dictionary :runs :pattern)))
           (n (case (rnd 4) (0 (rnd 64)) (1 (rnd 4096)) (2 (rnd 65536)) (t (rnd 262144))))
           (level (rnd 10)) (wbits (+ 9 (rnd 7))) (mem (1+ (rnd 9))) (strategy (rnd 5))
           (raw (zerop (rnd 3)))
           (data (make-input kind n))
           (stream (z-compress data level (if raw (- wbits) wbits) mem strategy))
           (fn (if raw #'decompress:inflate #'decompress:zlib-decompress)))
      (when (< (length stream) 20000) (if raw (push stream *raw-streams*) (push stream *z-streams*)))
      (check (equalp (ours-z fn stream) data)
             "~:[zlib~;raw deflate~] ~a ~d B level ~d window ~d mem ~d strategy ~d"
             raw kind n level wbits mem strategy)
      (check (equalp (ours-z fn stream :size-hint n) data)
             "~:[zlib~;raw deflate~] with size hint ~a ~d B level ~d window ~d mem ~d strategy ~d"
             raw kind n level wbits mem strategy))))

(defun test-zlib-corrupt (count)
  (let ((streams (coerce *z-streams* 'vector)) (rejected 0) (accepted 0) (skipped 0))
    (dotimes (k count)
      (let* ((bad (corrupt (aref streams (rnd (length streams)))))
             (z (z-uncompress bad)))
        (if (eq z :too-big)
            (incf skipped)
            (let ((ours (ours-z #'decompress:zlib-decompress bad)))
              (if (eq z :error) (incf rejected) (incf accepted))
              (check (equalp ours z) "corrupt zlib stream: libz ~a, ours ~a"
                     (if (eq z :error) "rejects" "decodes")
                     (if (eq ours :error) "rejects" (if (equalp ours z) "matches" "differs")))))))
    (format t "  (libz rejected ~d, decoded ~d, inconclusive ~d)~%" rejected accepted skipped)))

(defun test-raw-corrupt (count)
  (let ((streams (coerce *raw-streams* 'vector)) (rejected 0) (accepted 0) (skipped 0))
    (dotimes (k count)
      (let* ((bad (corrupt (aref streams (rnd (length streams)))))
             (z (z-inflate-raw bad)))
        (if (eq z :too-big)
            (incf skipped)
            (let ((ours (ours-z #'decompress:inflate bad)))
              (if (eq z :error) (incf rejected) (incf accepted))
              (check (equalp ours z) "corrupt raw deflate: libz ~a, ours ~a"
                     (if (eq z :error) "rejects" "decodes")
                     (if (eq ours :error) "rejects" (if (equalp ours z) "matches" "differs")))))))
    (format t "  (libz rejected ~d, decoded ~d, inconclusive ~d)~%" rejected accepted skipped)))

(defun run (&key (round-trips 1500) (corrupt 4000) (zlib-round-trips 1500) (zlib-corrupt 4000) (raw-corrupt 4000))
  (setf *pass* 0 *fail* 0 *streams* '() *z-streams* '() *raw-streams* '()
        *seed* 88172645463325252)
  (format t "~&reference vectors~%") (test-vectors)
  (format t "round trips via libbrotlienc (~d)~%" round-trips) (test-round-trips round-trips)
  (format t "corrupt streams vs libbrotlidec (~d)~%" corrupt) (test-corrupt corrupt)
  (format t "zlib and raw deflate round trips via libz (~d)~%" zlib-round-trips)
  (test-zlib-round-trips zlib-round-trips)
  (format t "corrupt zlib streams vs libz (~d)~%" zlib-corrupt) (test-zlib-corrupt zlib-corrupt)
  (format t "corrupt raw deflate vs libz (~d)~%" raw-corrupt) (test-raw-corrupt raw-corrupt)
  (format t "~&~d passed, ~d failed~%" *pass* *fail*)
  (zerop *fail*))
