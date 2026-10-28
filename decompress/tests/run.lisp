;;;; tests/run.lisp — run the test suite from the shell:
;;;;   sbcl --script tests/run.lisp            (exit status 0 = all passed)
(require :asdf)
(push (merge-pathnames "../" (directory-namestring *load-truename*)) asdf:*central-registry*)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "decompress/tests"))
(sb-ext:exit :code (if (uiop:symbol-call :decompress/tests :run) 0 1))
