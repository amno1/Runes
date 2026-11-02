;;;; package.lisp — package definition and exports for runes/wayland
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(defpackage #:runes/wayland
  (:use #:cl #:runes #:runes/raster)
  (:export
   ;; Glyph runs
   #:glyph-run
   #:flush-glyph-runs
   #:*max-glyph-runs*

   ;; Drawing through the caller's blend and fill
   #:draw-text
   #:draw-text-line

   ;; Drawing into a plain pixel buffer
   #:blend-alpha-into-buffer
   #:fill-rectangle-in-buffer
   #:draw-text-into-buffer)

  (:documentation "Runes text drawn on the client, as a Wayland client
   draws everything: glyph images from RUNES/RASTER blended into the
   client's own pixels.  A bridge until Antron draws text itself.

   DRAW-TEXT and DRAW-TEXT-LINE place each glyph and decoration and leave
   the pixels to the caller, which passes functions that blend an alpha
   map and fill a rectangle on its own surface, with its own clipping.
   DRAW-TEXT-INTO-BUFFER does both into a plain vector of #x00RRGGBB
   pixels."))
