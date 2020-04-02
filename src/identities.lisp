;;;; ***********************************************************************
;;;;
;;;; Name:          identities.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with delectus identities
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; Identities are strings derived from time-first uuids the Delectus
;;; uses as identifiers for lists, ops, and user-created columns.  It
;;; does not use standard-format UUID strings because they contain
;;; hyphens and may start with a numeric digit; these properties make
;;; it more complicated to work with them in SQLite, so we render the
;;; UUIDs as lowercase strings without hyphens, and with the letter
;;; 'I' prepended to signify "identity".

(defmethod uuid->identity ((id uuid:uuid))
  (concatenate 'string "I"
               (string-downcase
                (with-output-to-string (out)
                  (uuid::print-bytes out id)))))

(defmethod makeid ()
  (uuid->identity (uuid:make-v1-uuid)))

;;; (time (makeid))
