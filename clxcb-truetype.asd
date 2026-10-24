;;;; clxcb-truetype.asd — backward-compatibility alias for Runes/XCB
;;;;
;;;; Runes/XCB is the new name for clxcb-truetype.

(require :asdf)

(in-package :asdf-user)

(asdf:defsystem #:clxcb-truetype
  :description "Backward-compatibility alias for Runes/XCB."
  :version "1.0.0"
  :depends-on (#:runes/xcb))
