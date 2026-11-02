;;;; runes.asd — system definitions for Runes
;;;;
;;;; Pure Common Lisp TrueType / OpenType font management: discovery,
;;;; loading, caching, metrics and layout.  Rasterization and drawing are
;;;; in the systems after it: runes/raster, a simple CPU text renderer, and
;;;; the runes/xcb and runes/wayland text bridges built on it.  Antron
;;;; renders text from runes alone.

(require :asdf)

(in-package :asdf-user)

(asdf:defsystem #:runes
  :description "Pure Common Lisp TrueType/OpenType font discovery, loading, caching, metrics and layout."
  :author "Michael Filonenko, Arthur Miller and contributors (see AUTHORS)"
  :license "LGPL-2.1-or-later (see LICENSE and COPYING)"
  :homepage "https://github.com/amno1/runes"
  :source-control (:git "https://github.com/amno1/runes.git")
  :bug-tracker "https://github.com/amno1/runes/issues"
  :version "2.0.0"
  :depends-on (#:zpb-ttf
               #:cacle
               #:bordeaux-threads
               #:uiop)
  :pathname "src/core/"
  :serial t
  :components ((:file "package")
               (:file "font-cache")
               (:file "metrics")
               (:file "font")
               (:file "layout")))

(asdf:defsystem #:runes/raster
  :description "A simple CPU text renderer: Runes fonts rasterized into A8 alpha maps with cl-vectors and cl-aa."
  :author "Michael Filonenko, Arthur Miller and contributors (see AUTHORS)"
  :license "LGPL-2.1-or-later (see LICENSE and COPYING)"
  :homepage "https://github.com/amno1/runes"
  :version "2.0.0"
  :depends-on (#:runes
               #:zpb-ttf
               #:cl-vectors
               #:cl-paths-ttf
               #:cl-aa
               #:cacle
               #:trivial-garbage
               #:bordeaux-threads)
  :pathname "src/raster/"
  :serial t
  :components ((:file "package")
               (:file "raster")
               (:file "glyph-images")))

(asdf:defsystem #:runes/xcb
  :description "Runes text drawn on X11 through XCB and RENDER, until Antron draws text."
  :author "Michael Filonenko, Arthur Miller and contributors (see AUTHORS)"
  :license "LGPL-2.1-or-later (see LICENSE and COPYING)"
  :homepage "https://github.com/amno1/runes"
  :version "2.0.0"
  :depends-on ("clxcb" "clxcb/render"
               #:runes
               #:runes/raster
               #:zpb-ttf
               #:trivial-garbage
               #:bordeaux-threads
               #:uiop)
  :pathname "src/xcb/"
  :serial t
  :components ((:file "package")
               (:file "drawable")
               (:file "cache")
               (:file "glyphs")
               (:file "render")))

(asdf:defsystem #:runes/wayland
  :description "Runes text blended into client-side pixels, as on Wayland, until Antron draws text."
  :author "Arthur Miller and contributors (see AUTHORS)"
  :license "LGPL-2.1-or-later (see LICENSE and COPYING)"
  :homepage "https://github.com/amno1/runes"
  :version "2.0.0"
  :depends-on (#:runes
               #:runes/raster
               #:bordeaux-threads)
  :pathname "src/wayland/"
  :serial t
  :components ((:file "package")
               (:file "text")))

;;; ---- runes/otf: the OpenType reader (formerly zpb-otf), in otf/ --------------
;;;
;;; MIT licensed (otf/LICENSE).  The Brotli and zlib decoders that WOFF2 and
;;; WOFF 1.0 need are the decompress system in decompress/, which can also be
;;; used on its own.  ASDF does not search subdirectories, so its definition
;;; is loaded from here.
(asdf:load-asd (merge-pathnames "decompress/decompress.asd"
                                (or *load-truename* *compile-file-truename*)))

(asdf:defsystem #:runes/otf/runtime
  :version "0.2.0"
  :author "Arthur Miller and contributors"
  :description "Runtime structures, buffer reading, base condition classes and WOFF/WOFF2 decoding for runes/otf"
  :license "MIT"
  :depends-on (#:alexandria #:decompress)
  :serial t
  :components ((:module "src" :pathname "otf/src"
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

(asdf:defsystem #:runes/otf/codegen
  :version "0.1.0"
  :author "Arthur Miller and contributors"
  :description "OpenType table code generator from s-expression schema"
  :license "MIT"
  :depends-on (#:runes/otf/runtime)
  :serial t
  :components ((:module "codegen" :pathname "otf/codegen"
                :serial t
                :components ((:file "package")
                             (:file "emitter")
                             (:file "generator")))))

(asdf:defsystem #:runes/otf
  :version "0.2.0"
  :author "Arthur Miller and contributors"
  :description "Zero-copy, bounds-checked OpenType table parser generated from Microsoft OpenType spec"
  :license "MIT"
  :depends-on (#:runes/otf/runtime)
  :serial t
  :components ((:module "src" :pathname "otf/src"
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

(asdf:defsystem #:runes/otf/charstring
  :version "0.1.0"
  :author "Arthur Miller and contributors"
  :description "Type 1 and Type 2 (CFF/CFF2) charstring evaluator, ported from Fontations"
  :license "MIT"
  :depends-on (#:alexandria #:runes/otf)
  :components ((:module "src" :pathname "otf/src"
                :components ((:module "charstring"
                              :serial t
                              :components ((:file "charstring")
                                           (:file "blend")
                                           (:file "dict")
                                           (:file "cff")))))))
