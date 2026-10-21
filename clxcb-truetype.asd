;;;; clxcb-truetype.asd — system definition for clxcb-truetype
;;;;
;;;; The system is clxcb-truetype, after clxcb; its package is
;;;; XCB-TRUETYPE (nickname XFT), as clxcb's systems clxcb/<name> have
;;;; packages XCB-<NAME>.

(require :asdf)

(in-package :asdf-user)

(asdf:defsystem #:clxcb-truetype
  :description "Pure Common Lisp TrueType font rendering for XCB."
  :author "Michael Filonenko, Arthur Miller and contributors (see AUTHORS)"
  :license "LGPL-2.1-or-later (see LICENSE and COPYING)"
  :homepage "https://github.com/amno1/clxcb-truetype"
  :source-control (:git "https://github.com/amno1/clxcb-truetype.git")
  :bug-tracker "https://github.com/amno1/clxcb-truetype/issues"
  :version "0.9.0"
  :depends-on ("clxcb" "clxcb/render"
               #:cl-truetype
               #:zpb-ttf
               #:trivial-garbage
               #:bordeaux-threads
               #:uiop)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "drawable")
               (:file "cache")
               (:file "glyphs")
               (:file "render")))
