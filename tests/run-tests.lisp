;;;; run-tests.lisp — Master test runner for zpb-otf and zpb-ttf
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-user)

(defun run-all-tests ()
  (format t "~%========================================================~%")
  (format t "Running All Test Suites: Contour + Table Readers~%")
  (format t "========================================================~%~%")

  (let ((this-dir (make-pathname :directory (pathname-directory *load-truename*))))
    ;; 1. Contour segment tests
    (format t "--- 1. Running Contour Segment Tests (test.lisp) ---~%")
    (load (merge-pathnames "test.lisp" this-dir))
    (format t "  [PASS] All contour segment assertions succeeded.~%~%")

    ;; 2. Comprehensive Table Tests
    (format t "--- 2. Running Table Reader Tests (tables-test.lisp) ---~%")
    (load (merge-pathnames "tables-test.lisp" this-dir))
    (uiop:symbol-call :zpb-otf-test :run-table-tests)

    ;; 3. Charstring evaluator
    (format t "~%--- 3. Running Charstring Evaluator Tests (charstring-test.lisp) ---~%")
    (asdf:load-system :zpb-otf/charstring)
    (load (merge-pathnames "charstring-test.lisp" this-dir))
    (uiop:symbol-call :zpb-otf/charstring-test :run-charstring-tests)

    ;; 4. CFF DICTs, font matrices and fonts
    (format t "~%--- 4. Running CFF Tests (cff-test.lisp) ---~%")
    (load (merge-pathnames "cff-test.lisp" this-dir))
    (uiop:symbol-call :zpb-otf/cff-test :run-cff-tests)

    (format t "~%========================================================~%")
    (format t "ALL TEST SUITES PASSED CLEANLY!~%")
    (format t "========================================================~%")
    t))

#+sbcl
(when (member (pathname-name *load-truename*) '("run-tests") :test 'string-equal)
  (handler-case
      (progn
        (run-all-tests)
        (sb-ext:exit :code 0))
    (error (e)
      (format *error-output* "~%TEST RUN FAILED: ~A~%" e)
      (sb-ext:exit :code 1))))
