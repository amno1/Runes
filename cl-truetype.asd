;;;; cl-truetype.asd — backward-compatibility alias for Runes
;;;;
;;;; Runes is the new name for cl-truetype.

(require :asdf)

(in-package :asdf-user)

(asdf:defsystem #:cl-truetype
  :description "Backward-compatibility alias for Runes."
  :version "1.0.0"
  :depends-on (#:runes))
