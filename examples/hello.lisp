;;;; examples/hello.lisp — draw a line of antialiased text in a window.
;;;; Copyright (C) 2026 Arthur Miller
;;;; SPDX-License-Identifier: MIT
;;;; An example, under the MIT license so that it can be copied freely
;;;; into programs of your own, whatever their license.
;;;;
;;;; Load it with (load "examples/hello.lisp") after loading runes/xcb,
;;;; then (runes-example:hello).  Press any key or close the window
;;;; to end.

(defpackage #:runes-example
  (:use #:cl)
  (:export #:hello))

(in-package #:runes-example)

(defun hello (&key (text "Hello, Runes") (family "DejaVu Sans") (size 24))
  (xcb:with-x-connection (conn)
    (let* ((screen (first (xcb:conn-roots conn)))
           (win (xcb:generate-id conn)))
      (xcb:create-window conn 0 win (xcb:root screen)
                         100 100 ; x y
                         420 90  ; width height
                         0 1 0   ; no border, InputOutput, use parent visual
                         (logior #x2 #x800)  ; back-pixel, event-mask
                         :background-pixel (xcb:white-pixel screen)
                         :event-mask (logior #x8000 #x1)) ; exposure, key-press
      (xcb:set-wm-name conn win "Runes")
      (xcb:set-wm-delete-protocol conn win)
      (xcb:map-window conn win)
      (xcb:flush conn)
      ;; :OWNED T: the window is handed to the drawable, so
      ;; DESTROY-DRAWABLE destroys it with the picture.
      (let ((drawable (make-instance 'runes/xcb:window :owned t
                                     :connection conn :id win :screen screen))
            (font (make-instance 'runes/xcb:font :family family :size size)))
        (unwind-protect
             (loop for ev = (xcb:wait-for-event conn)
                   do (typecase ev
                        (xcb:expose
                         (runes/xcb:draw-text drawable font text 20 55 :colour #x203060)
                         (xcb:flush conn))
                        ((or xcb:key-press xcb:client-message)
                         (return))))
          (runes/xcb:destroy-drawable drawable)
          (xcb:flush conn))))))
