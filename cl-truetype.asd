;;;; cl-truetype.asd — system definition for cl-truetype
;;;;
;;;; The backend-neutral part of clxcb-truetype: font discovery, metrics
;;;; and rasterization to alpha maps.  It draws nothing; clxcb-truetype
;;;; draws its alpha maps through XCB RENDER, and other backends can blend
;;;; them into their own pixels.

(require :asdf)

(in-package :asdf-user)

(asdf:defsystem #:cl-truetype
  :description "Pure Common Lisp TrueType font metrics and rasterization."
  :author "Michael Filonenko, Arthur Miller and contributors (see AUTHORS)"
  :license "LGPL-2.1-or-later (see LICENSE and COPYING)"
  :homepage "https://github.com/amno1/clxcb-truetype"
  :source-control (:git "https://github.com/amno1/clxcb-truetype.git")
  :bug-tracker "https://github.com/amno1/clxcb-truetype/issues"
  :version "0.9.0"
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
