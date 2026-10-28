;;;; package.lisp — OpenType spec code generator for Common Lisp
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :runes/otf)
    (make-package :runes/otf :use '(:cl))))

(defpackage #:runes/otf/codegen
  (:nicknames #:otf-codegen)
  (:use #:cl #:runes/otf)
  (:shadow #:base #:head #:name #:read-name)
  (:export
   #:read-sexp-file
   #:emit-file-forms
   #:generate-table-file
   #:generate-all-tables))
