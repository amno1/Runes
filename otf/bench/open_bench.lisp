;;;; open_bench.lisp — the time and allocation of runes/otf's open-font-loader
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; Usage: sbcl --load open_bench.lisp NAME=PATH ...  (after the runes system's
;;; central registry entries are set up, as hint_bench.lisp does).  Prints
;;; "NAME MICROSECONDS BYTES" rows: the best of five batches of 2000 opens, with
;;; the file read already and the bytes allocated per open.  Opening is not
;;; hinting: these rows are not part of the hinting ratio.

(require :asdf)
(ignore-errors (load "~/quicklisp/setup.lisp"))
(let ((root (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*))))
  (push root asdf:*central-registry*)
  (push (merge-pathnames "decompress/" root) asdf:*central-registry*))
(asdf:load-system :zpb-ttf)

(setf (sb-ext:bytes-consed-between-gcs) (* 512 1024 1024))
(format t "# runes/otf open: open-font-loader + close-font-loader, best of 5 batches of 2000; SBCL ~a~%"
        (lisp-implementation-version))

(defun now-us ()
  (multiple-value-bind (s ns) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* s 1d6) (/ ns 1d3))))

(dolist (arg (cdr (member "--" sb-ext:*posix-argv* :test #'string=)))
  (let* ((eq (position #\= arg))
         (name (subseq arg 0 eq))
         (fd (runes/otf:ensure-font-data (subseq arg (1+ eq))))
         (best 1d18) (bytes 0))
    (dotimes (i 200) (zpb-ttf:close-font-loader (zpb-ttf:open-font-loader fd)))
    (dotimes (b 5)
      (sb-ext:gc :full t)
      (let ((consed (sb-ext:get-bytes-consed)) (start (now-us)))
        (dotimes (i 2000) (zpb-ttf:close-font-loader (zpb-ttf:open-font-loader fd)))
        (let ((each (/ (- (now-us) start) 2000)))
          (when (< each best)
            (setf best each
                  bytes (round (- (sb-ext:get-bytes-consed) consed) 2000))))))
    (format t "~a ~,3f ~d~%" name best bytes)))
