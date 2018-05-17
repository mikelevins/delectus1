;;;; sync.lisp

(in-package :engine)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; implementation of change logs and the change protocol
;;;
;;; A changelog is simply a list of recorded change messages
;;; associated with the identifiable object to which the changes have
;;; been applied. They are recorded in reverse order, by pushing new
;;; ones onto the front of the list.


