;;;; runes.asd — system definition for Runes
;;;;
;;;; Pure Common Lisp TrueType / OpenType font discovery, metrics,
;;;; caching, and antialiased glyph rasterization.

(require :asdf)

(in-package :asdf-user)

(asdf:defsystem #:runes
  :description "Pure Common Lisp TrueType/OpenType font metrics, discovery, and rasterization."
  :author "Michael Filonenko, Arthur Miller and contributors (see AUTHORS)"
  :license "LGPL-2.1-or-later (see LICENSE and COPYING)"
  :homepage "https://github.com/amno1/runes"
  :source-control (:git "https://github.com/amno1/runes.git")
  :bug-tracker "https://github.com/amno1/runes/issues"
  :version "1.0.0"
  :depends-on (#:zpb-ttf
               #:cl-vectors
               #:cl-paths-ttf
               #:cl-aa
               #:cacle
               #:bordeaux-threads
               #:uiop)
  :pathname "src/core/"
  :serial t
  :components ((:file "package")
               (:file "font-cache")
               (:file "metrics")
               (:file "raster")
               (:file "glyph-images")))

(asdf:defsystem #:runes/xcb
  :description "Pure Common Lisp TrueType font rendering for XCB."
  :author "Michael Filonenko, Arthur Miller and contributors (see AUTHORS)"
  :license "LGPL-2.1-or-later (see LICENSE and COPYING)"
  :homepage "https://github.com/amno1/runes"
  :version "1.0.0"
  :depends-on ("clxcb" "clxcb/render"
               #:runes
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
