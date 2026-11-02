;;;; package.lisp — package definition and exports for runes
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(defpackage #:runes
  (:nicknames #:truetype #:cl-truetype)
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
   #:line-box
   #:kern-offset
   #:xmin
   #:ymin
   #:xmax
   #:ymax
   #:*allow-fixed-pitch-p*

   ;; Layout
   #:layout-string
   #:font-decorations

   ;; For backends
   #:with-font-loader
   #:get-font-pathname
   #:font-units->pixels-x
   #:font-units->pixels-y
   #:font-cache-fetch
   #:make-font-cache
   #:font-string-bboxes
   #:font-string-line-bboxes)

  (:documentation "Pure Common Lisp TrueType and OpenType font management:
   finding installed fonts, opening and caching them, metrics, and laying
   out a line of glyphs.  It draws nothing and rasterizes nothing: a
   renderer turns the glyphs into pixels (runes/raster for now), and adds
   TARGET-DPI methods for its own drawables."))
