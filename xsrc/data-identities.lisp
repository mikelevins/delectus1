;;;; ***********************************************************************
;;;;
;;;; Name:          data-identities.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with delectus identities
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************


;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; Identities are strings derived from random (v4) uuids the Delectus
;;; uses as identifiers for lists, ops, and user-created columns.  It
;;; does not use standard-format UUID strings because they contain
;;; hyphens and may start with a numeric digit; these properties make
;;; it more complicated to work with them in SQLite.
;;;
;;; We construct an identity by first creating a v4 (random) UUID, then
;;; printing its bytes to a string in hexadecimal notation. We then
;;; downcase the resulting string and prepend the letter "I".
;;;
;;; For example, "I072e66008a6611ea85ed38c9864ebde0" is a vaid
;;; identity.

;;; ---------------------------------------------------------------------
;;; identities
;;; ---------------------------------------------------------------------

(in-package #:delectus)

(defmethod identity? (thing) nil)

(defmethod identity? ((thing string))
  (and (= 33 (length thing))
       (char= #\I (elt thing 0))
       (let ((result t))
         (block checking
           (loop for i from 1 below (length thing)
              do (unless (find (elt thing i) "0123456789abcdefABCDEF")
                   (setf result nil)
                   (return-from checking nil))))
         result)))

;;; (time (identity? (makeid)))

(defmethod uuid->identity ((id uuid:uuid))
  (concatenate 'string "I"
               (string-downcase
                (with-output-to-string (out)
                  (uuid::print-bytes out id)))))

(defmethod makeid ()
  (uuid->identity (uuid:make-v4-uuid)))

;;; (time (makeid))

(defparameter +delectus-identity-string-length+
  (length (makeid)))

(defmethod identity->uuid ((identity string))
  (assert (identity? identity)() "Not a valid identity")
  (let* ((bytestring (subseq identity 1))
         (uuid-string (join-strings "-"
                                    (list (subseq bytestring 0 8)
                                          (subseq bytestring 8 12)
                                          (subseq bytestring 12 16)
                                          (subseq bytestring 16 20)
                                          (subseq bytestring 20)))))
    (uuid:make-uuid-from-string uuid-string)))

;;; (time (identity->uuid (makeid)))

