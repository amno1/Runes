;;;; zpb-ttf.asd — Access TrueType and OpenType font metrics and outlines
;;;; The zpb-ttf API, kept for backward compatibility, on top of runes/otf.
;;;; Its sources are in otf/src/ with runes/otf's.
;;;; Copyright (c) 2006 Zachary Beane, 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

(defpackage #:zpb-ttf-system
  (:use #:cl #:asdf))

(in-package #:zpb-ttf-system)

(defsystem #:zpb-ttf
  :version "2.0.0"
  :author "Zachary Beane <xach@xach.com>, Arthur Miller and contributors"
  :description "Access TrueType and OpenType font metrics and outlines from Common Lisp"
  :license "BSD-2-Clause"
  :depends-on (#:runes/otf #:runes/otf/charstring)
  :serial t
  :components ((:module "src"
                :pathname "otf/src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "conditions")
                             (:file "bounding-box")
                             (:file "font-loader")
                             (:file "glyph")
                             (:file "gvar")
                             (:file "glyf-outlines")
                             (:file "cff-outlines")
                             (:file "variations")
                             (:file "gpos-kerning")
                             (:file "font-loader-interface")))))
