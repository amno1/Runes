;;;; cache.lisp — solid-fill and drawable picture caches
;;;; Copyright (C) 2012–2014 Michael Filonenko and contributors
;;;;   (clx-truetype, from which this library is ported; see AUTHORS)
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: LGPL-2.1-or-later (see LICENSE)

(in-package #:xcb-truetype)

(defconstant +pict-op-over+ 3
  "RENDER PictOpOver: Source Over Destination composite operation.")

;; Lifetime note: *FILL-CACHE* is a two-level cache. The outer table maps
;; CONNECTION to an inner hash table of (COLOUR -> PICTURE).
;; It uses TRIVIAL-GARBAGE:MAKE-WEAK-HASH-TABLE with :WEAKNESS :KEY,
;; so a connection object that becomes unreachable is automatically collected
;; along with its inner colour cache. This prevents long-running applications
;; from leaking connection references and pictures across multiple connections.
(defvar *fill-cache* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq)
  "Maps an XCB CONNECTION to an inner hash table of (COLOUR -> PICTURE).
   The outer table is weak on the connection key so a connection that
   becomes unreachable does not keep the cache entry — and hence itself — alive.")

(defun solid-fill-picture (connection colour)
  "Return a 1x1 solid-fill RENDER picture for COLOUR on CONNECTION, cached in
   *FILL-CACHE*. COLOUR is assumed to be an RGB integer in #x00RRGGBB format."
  (let* ((conn-table (or (gethash connection *fill-cache*)
                         (setf (gethash connection *fill-cache*)
                               (make-hash-table :test 'eql))))
         (cached (gethash colour conn-table)))
    (or cached
        (let* ((id (xcb:generate-id connection))
               (r  (ldb (byte 8 16) colour))
               (g  (ldb (byte 8 8)  colour))
               (b  (ldb (byte 8 0)  colour))
               (color (xcb-render:make-color (logior (ash r 8) r)
                                             (logior (ash g 8) g)
                                             (logior (ash b 8) b)
                                             #xffff)))
          (create-solid-fill connection id color)
          (setf (gethash colour conn-table) id)))))

(defun drawable-picture-for (drawable &optional format)
  "Return DRAWABLE's RENDER picture, creating it on first access.  FORMAT
   is the PictFormat to create it with; when NIL it is looked up from
   DRAWABLE's visual.  The lookup only happens when the picture is created."
  (or (drawable-picture drawable)
      (let* ((conn (drawable-connection drawable))
             (id   (xcb:generate-id conn)))
        (create-picture conn
                        id
                        (drawable-id drawable)
                        (or format
                            (format-for-visual conn (drawable-visual-id drawable)))
                        0)
        ;; A drawable destroyed or released earlier has no finalizer:
        ;; register one again, so the GC frees this picture too.
        (%arm-finalizer drawable)
        (setf (drawable-picture drawable) id))))

(defvar *mask-gc-cache* (trivial-garbage:make-weak-hash-table :weakness :key :test 'eq)
  "Maps an XCB CONNECTION to a depth-8 Graphics Context (GC) ID used for uploading
   alpha masks via PUT-IMAGE-CHUNKED. The table is weak on CONNECTION.")

(defun mask-gc-for (connection drawable-id &optional root)
  "Return a GC for depth-8 drawables on CONNECTION, made against DRAWABLE-ID
   (a depth-8 pixmap) and cached.  A GC belongs to one screen and depth, so
   the cache is per connection and per screen: ROOT names the screen, by its
   root window.  Without ROOT one GC serves the connection, which is right
   only for a connection used with one screen."
  (let ((per-screen (or (gethash connection *mask-gc-cache*)
                        (setf (gethash connection *mask-gc-cache*)
                              (make-hash-table :test 'eql)))))
    (or (gethash root per-screen)
        (let ((gc (xcb:generate-id connection)))
          (xcb:create-gc connection gc drawable-id 0)
          (setf (gethash root per-screen) gc)))))
