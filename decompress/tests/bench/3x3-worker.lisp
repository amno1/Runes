;;;; tests/bench/3x3-worker.lisp — one process of 3x3.lisp: decodes the cases
;;;; with one implementation and prints "case<TAB>MB/s" lines.
;;;;
;;;; Environment:
;;;;   IMPL=lisp   decompress from DIR (an ASDF directory); UNPIN=1 pushes
;;;;               :DECOMPRESS-UNPIN-NIL on *FEATURES* before it is loaded, for
;;;;               sources that declare (SB-EXT:UNPIN-NIL) under that feature
;;;;   IMPL=c      libdeflate (zlib streams) and libbrotlidec (Brotli streams),
;;;;               through SB-ALIEN
;;;;   ROUNDS, TARGET   rounds per case (best kept) and output bytes per round
;;;;   ONLY=text   only the cases whose name contains the text

(require :asdf)

(defvar *impl* (or (sb-ext:posix-getenv "IMPL") "lisp"))
(defvar *here* (directory-namestring *load-truename*))
(defvar *rounds* (parse-integer (or (sb-ext:posix-getenv "ROUNDS") "3")))
(defvar *target* (parse-integer (or (sb-ext:posix-getenv "TARGET") "60000000")))

(defparameter *cases*
  '(("zlib  RedHatText.woff"   "RedHatText-Regular.woff")
    ("zlib  SourceCodePro.woff" "SourceCodePro-Regular.woff")
    ("zlib  meslo.z9"          "meslo.z9.z")
    ("zlib  src8m.z6"          "src8m.z6.z")
    ("brotli Meslo.br"         "MesloLGLNerdFont-Bold.br")
    ("brotli Tangut.br"        "NotoSerifTangut-Regular.br")
    ("brotli src8m.q9"         "src8m.q9.br")))

(if (string= *impl* "lisp")
    (progn
      (when (equal (sb-ext:posix-getenv "UNPIN") "1")
        (pushnew :decompress-unpin-nil *features*))
      (push (uiop:ensure-directory-pathname (sb-ext:posix-getenv "DIR"))
            asdf:*central-registry*)
      (let ((real-error *error-output*))
        (handler-bind ((warning #'muffle-warning)
                       (sb-ext:compiler-note #'muffle-warning)
                       (error (lambda (c)
                                (format real-error "~&load failed: ~a~%" c)
                                (sb-ext:exit :code 1 :abort t))))
          (let ((*error-output* (make-broadcast-stream))
                (*standard-output* (make-broadcast-stream)))
            (asdf:load-system "decompress")))))
    (progn
      (sb-alien:load-shared-object (or (sb-ext:posix-getenv "LIBBROTLIDEC") "libbrotlidec.so"))
      (sb-alien:load-shared-object (or (sb-ext:posix-getenv "LIBDEFLATE") "libdeflate.so"))))

(defvar *deflate*
  (when (string= *impl* "c")
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "libdeflate_alloc_decompressor" (function sb-sys:system-area-pointer)))))

(defun c-decompress (in n zlib)
  (declare (type (simple-array (unsigned-byte 8) (*)) in) (type fixnum n))
  (let ((out (make-array n :element-type '(unsigned-byte 8))))
    (sb-alien:with-alien ((size sb-alien:unsigned-long n))
      (sb-sys:with-pinned-objects (in out)
        (unless (if zlib
                    (zerop (sb-alien:alien-funcall
                            (sb-alien:extern-alien "libdeflate_zlib_decompress"
                                                   (function sb-alien:int sb-sys:system-area-pointer
                                                             sb-sys:system-area-pointer sb-alien:unsigned-long
                                                             sb-sys:system-area-pointer sb-alien:unsigned-long
                                                             sb-sys:system-area-pointer))
                            *deflate* (sb-sys:vector-sap in) (length in)
                            (sb-sys:vector-sap out) (length out) (sb-sys:int-sap 0)))
                    (= 1 (sb-alien:alien-funcall
                          (sb-alien:extern-alien "BrotliDecoderDecompress"
                                                 (function sb-alien:int sb-alien:unsigned-long
                                                           sb-sys:system-area-pointer
                                                           (* sb-alien:unsigned-long)
                                                           sb-sys:system-area-pointer))
                          (length in) (sb-sys:vector-sap in) (sb-alien:addr size)
                          (sb-sys:vector-sap out))))
          (error "failed to decode"))
        (unless (= size n) (error "produced ~d bytes, expected ~d" size n))))
    out))

(defun slurp (p)
  (with-open-file (s p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun now ()
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ sec (/ usec 1d6))))

(defun be (v i n)
  (let ((r 0)) (dotimes (k n r) (setf r (logior (ash r 8) (aref v (+ i k)))))))

(defun woff-tables (v)
  (loop for i below (be v 12 2)
        for e = (+ 44 (* 20 i))
        for offset = (be v (+ e 4) 4)
        for comp = (be v (+ e 8) 4)
        for orig = (be v (+ e 12) 4)
        when (< comp orig)
          collect (cons (subseq v offset (+ offset comp)) orig)))

(defun decode (in n zlib)
  (if (string= *impl* "c")
      (c-decompress in n zlib)
      (if zlib
          (funcall (intern "ZLIB-DECOMPRESS" "DECOMPRESS") in :size-hint n)
          (funcall (intern "DECOMPRESS" "DECOMPRESS") in :size-hint n))))

(defun checksum (out h)
  (declare (type (simple-array (unsigned-byte 8) (*)) out) (type (unsigned-byte 32) h)
           (optimize speed))
  (dotimes (i (length out) h)
    (setf h (logand (+ (* h 31) (aref out i)) #xffffffff))))

(defun job (path)
  "(values thunk bytes), the thunk decoding the file once and returning a
checksum of its output."
  (let ((v (slurp path)))
    (flet ((size ()
             (with-open-file (s (make-pathname :type "size" :defaults path))
               (parse-integer (read-line s)))))
      (cond
        ((string= (pathname-type path) "woff")
         (let ((tables (woff-tables v)))
           (values (lambda ()
                     (let ((h 0))
                       (loop for (z . n) in tables do (setf h (checksum (decode z n t) h)))
                       h))
                   (reduce #'+ tables :key #'cdr))))
        ((string= (pathname-type path) "z")
         (let ((n (size))) (values (lambda () (decode v n t)) n)))
        (t
         (let ((n (size))) (values (lambda () (decode v n nil)) n)))))))

(let ((dir (merge-pathnames "data/" *here*))
      (only (or (sb-ext:posix-getenv "ONLY") "")))
  (loop for (name file) in *cases*
        when (search only name)
          do (multiple-value-bind (fn n) (job (merge-pathnames file dir))
               (let* ((reps (max 5 (floor *target* n))) (best 0d0)
                      (out (funcall fn))                     ; warm-up and check
                      (sum (if (integerp out) out (checksum out 0))))
                 (dotimes (k *rounds*)
                   (let ((t0 (now)))
                     (dotimes (i reps) (funcall fn))
                     (setf best (max best (/ (* reps n) 1d6 (- (now) t0))))))
                 (format t "~a~c~,2f~c~x~%" name #\Tab best #\Tab sum)
                 (finish-output)))))
