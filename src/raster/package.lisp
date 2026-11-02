;;;; package.lisp — package definition and exports for runes/raster
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(defpackage #:runes/raster
  (:use #:cl #:runes)
  (:export
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
   #:layout-styled-glyphs
   #:*glyph-images*
   #:*max-glyph-faces*)

  (:documentation "A simple CPU text renderer: Runes fonts rasterized into
   A8 alpha maps, whole strings, or glyphs one at a time and cached per face.
   The runes/xcb and runes/wayland bridges draw with it. Two systems use this
   package: runes/raster rasterizes with CL-VECTORS and CL-AA,
   runes/raster-antron with Antron's CPU rasterizer; load one of them."))
