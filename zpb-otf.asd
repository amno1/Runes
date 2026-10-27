;;;; zpb-otf.asd — OpenType font parsing system for Common Lisp
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

;;; The Brotli and zlib decoders that WOFF2 and WOFF 1.0 need live in
;;; src/decompress as their own system, decompress, which can also be used
;;; on its own.  ASDF does not search subdirectories, so load its definition
;;; from here.
(asdf:load-asd (merge-pathnames "src/decompress/decompress.asd"
                                (or *load-truename* *compile-file-truename*)))

(asdf:defsystem #:zpb-otf/runtime
  :version "0.1.0"
  :author "Arthur Miller and contributors"
  :description "Runtime structures, buffer reading, base condition classes and WOFF/WOFF2 decoding for zpb-otf"
  :license "MIT"
  :depends-on (#:alexandria #:decompress)
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:module "runtime"
                  :serial t
                  :components ((:file "package")
                               (:file "conditions")
                               (:file "font-data")
                               (:file "woff1")
                               (:file "woff2")
                               (:file "table-ref")))))))

(asdf:defsystem #:zpb-otf/codegen
  :version "0.1.0"
  :author "Arthur Miller and contributors"
  :description "OpenType table code generator from s-expression schema"
  :license "MIT"
  :depends-on (#:zpb-otf/runtime)
  :serial t
  :components ((:module "codegen"
                :serial t
                :components ((:file "package")
                             (:file "emitter")
                             (:file "generator")))))

(asdf:defsystem #:zpb-otf
  :version "0.1.0"
  :author "Arthur Miller and contributors"
  :description "Zero-copy, bounds-checked OpenType table parser generated from Microsoft OpenType spec"
  :license "MIT"
  :depends-on (#:zpb-otf/runtime)
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:module "tables"
                  ;; Low-level tables first, shared layout/variation structures before their users.
                  :serial t
                  :components
                  ((:file "font")
                   (:file "head")
                   (:file "maxp")
                   (:file "hhea")
                   (:file "hmtx")
                   (:file "vhea")
                   (:file "vmtx")
                   (:file "name")
                   (:file "os2")
                   (:file "post")
                   (:file "cmap")
                   (:file "glyf")
                   (:file "gasp")
                   (:file "hdmx")
                   (:file "dsig")
                   (:file "meta")
                   (:file "ltag")
                   (:file "vorg")
                   (:file "cpal")
                   (:file "svg")
                   (:file "cff")
                   (:file "cff2")
                   (:file "bitmap")
                   (:file "cblc")
                   (:file "cbdt")
                   (:file "eblc")
                   (:file "ebdt")
                   (:file "sbix")
                   (:file "fvar")
                   (:file "variations")
                   (:file "hvar")
                   (:file "vvar")
                   (:file "mvar")
                   (:file "cvar")
                   (:file "gvar")
                   (:file "stat")
                   (:file "layout")
                   (:file "aat")
                   (:file "ankr")
                   (:file "feat")
                   (:file "trak")
                   (:file "kerx")
                   (:file "kern")
                   (:file "mort")
                   (:file "morx")
                   (:file "gpos")
                   (:file "gsub")
                   (:file "gdef")
                   (:file "base")
                   (:file "math")
                   (:file "avar")
                   (:file "colr")
                   (:file "varc")
                   (:file "ift")))
                 (:file "extensions")))))

(asdf:defsystem #:zpb-otf/charstring
  :version "0.1.0"
  :author "Arthur Miller and contributors"
  :description "Type 1 and Type 2 (CFF/CFF2) charstring evaluator, ported from Fontations"
  :license "MIT"
  :depends-on (#:alexandria #:zpb-otf)
  :components ((:module "src"
                :components ((:module "charstring"
                              :serial t
                              :components ((:file "charstring")
                                           (:file "blend")
                                           (:file "dict")
                                           (:file "cff")))))))
