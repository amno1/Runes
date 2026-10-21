;;;; package.lisp — package definition and exports for truetype
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(defpackage #:truetype
  (:use #:cl)
  (:export
   ;; Font class and accessors
   #:font
   #:font-family
   #:font-subfamily
   #:font-size
   #:font-underline
   #:font-strikethrough
   #:font-overline
   #:font-background
   #:font-foreground
   #:font-overwrite-gcontext
   #:font-antialias
   #:font-equal
   #:check-valid-font-families
   #:flush-font-caches

   ;; Font caching & discovery
   #:*font-dirs*
   #:*font-cache*
   #:*font-loader-cache*
   #:close-font-loaders
   #:cache-font-file
   #:cache-fonts
   #:get-font-families
   #:get-font-subfamilies

   ;; Metrics
   #:target-dpi
   #:fit-font
   #:fit-font-size
   #:text-center
   #:font-ascent
   #:font-descent
   #:font-line-gap
   #:baseline-to-baseline
   #:font-ascent-for-dpi
   #:font-descent-for-dpi
   #:text-bounding-box
   #:text-width
   #:text-height
   #:text-line-bounding-box
   #:text-line-width
   #:text-line-height
   #:font-lines-height
   #:xmin
   #:ymin
   #:xmax
   #:ymax
   #:*allow-fixed-pitch-p*

   ;; Rasterization of whole strings
   #:text-pixarray
   #:text-pixarray-for-dpi
   #:text-pixarray-for-drawable
   #:text-line-pixarray
   #:text-line-pixarray-for-dpi
   #:text-line-pixarray-for-drawable

   ;; Glyphs one at a time
   #:glyph-image
   #:glyph-image-width
   #:glyph-image-height
   #:glyph-image-stride
   #:glyph-image-x
   #:glyph-image-y
   #:glyph-image-advance
   #:glyph-image-data
   #:layout-glyphs
   #:*glyph-images*
   #:*max-glyph-faces*

   ;; For backends
   #:with-font-loader
   #:get-font-pathname
   #:font-units->pixels-x
   #:font-units->pixels-y
   #:font-cache-fetch
   #:font-string-bboxes
   #:font-string-line-bboxes
   #:font-decorations)

  (:documentation "Pure Common Lisp TrueType fonts: discovery, metrics and
   rasterization into alpha maps, for any drawing backend.  It draws
   nothing itself; a backend (clxcb-truetype for XCB, or a client-side
   renderer) puts the alpha maps on screen, and adds TARGET-DPI methods
   for its own drawables."))
