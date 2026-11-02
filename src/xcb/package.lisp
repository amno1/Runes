;;;; package.lisp — package definition and exports for runes/xcb
;;;; Copyright (C) 2012–2014 Michael Filonenko
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(defpackage #:runes/xcb
  (:nicknames #:runes-xcb #:xcb-truetype #:xft)
  (:use #:cl #:runes #:runes/raster)
  (:import-from #:xcb-render
                #:create-picture
                #:free-picture
                #:composite
                #:create-solid-fill)
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

   ;; Drawable wrappers & lifecycle
   #:drawable
   #:drawable-connection
   #:drawable-id
   #:drawable-owned-p
   #:drawable-visual
   #:drawable-picture
   #:drawable-screen
   #:window
   #:pixmap
   #:destroy-drawable
   #:release-drawable
   #:flush-deferred-frees
   #:*draw-with-glyph-sets*

   ;; Connection & screen format helpers
   #:format-for-visual
   #:a8-format
   #:pict-formats-reply-for
   #:screen-dpi
   #:dpi-x
   #:dpi-y

   ;; Metrics
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

   ;; Rasterization
   #:text-pixarray
   #:text-pixarray-for-dpi
   #:text-pixarray-for-drawable
   #:text-line-pixarray
   #:text-line-pixarray-for-dpi
   #:text-line-pixarray-for-drawable

   ;; Picture & GC caches
   #:*fill-cache*
   #:solid-fill-picture
   #:*mask-gc-cache*
   #:mask-gc-for
   #:drawable-picture-for
   #:+pict-op-over+

   ;; Rendering
   #:draw-text
   #:draw-text-line
   #:put-alpha-mask
   #:put-image-chunked)

  (:documentation "Runes text drawn on X11 through XCB and RENDER: a
   bridge until Antron draws text itself.  Fonts and metrics come from
   RUNES and glyph images from RUNES/RASTER, whose API this package
   re-exports.  Its drawables are RUNES/XCB:WINDOW and RUNES/XCB:PIXMAP: a
   package that :USEs this one together with CLX or CLIM, which have a
   WINDOW and a PIXMAP of their own, should shadow them or write them
   qualified."))
