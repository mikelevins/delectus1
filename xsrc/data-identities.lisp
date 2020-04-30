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
;;; An *identity* string is the lowercased UUID with hyphens stripped
;;; out. A *userdata column label* is an identity string with the prefix
;;; 'I' added.
;;;
;;; For example, "072e66008a6611ea85ed38c9864ebde0" is an identity,
;;; and "I072e66008a6611ea85ed38c9864ebde0" is the corresponding
;;; userdata column label.

;;; ---------------------------------------------------------------------
;;; identities
;;; ---------------------------------------------------------------------

(in-package #:delectus)

(defmethod identity? (thing) nil)

(defmethod identity? ((thing string))
  (and (= 32 (length thing))
       (every (lambda (ch)(find ch "0123456789abcdefABCDEF"))
              thing)))

;;; (time (identity? (makeid)))

(defmethod uuid->identity ((id uuid:uuid))
  (string-downcase
   (with-output-to-string (out)
     (uuid::print-bytes out id))))

(defmethod makeid ()
  (uuid->identity (uuid:make-v4-uuid)))

;;; (time (makeid))

(defparameter +delectus-identity-string-length+
  (length (makeid)))

(defmethod identity->uuid ((identity string))
  (assert (identity? identity)() "Not a valid identity")
  (let* ((uuid-string (join-strings "-"
                                    (list (subseq identity 0 8)
                                          (subseq identity 8 12)
                                          (subseq identity 12 16)
                                          (subseq identity 16 20)
                                          (subseq identity 20)))))
    (uuid:make-uuid-from-string uuid-string)))

;;; (time (identity->uuid (makeid)))

;;; ---------------------------------------------------------------------
;;; column-labels
;;; ---------------------------------------------------------------------

(defmethod identity->column-label ((identity string))
  (assert (identity? identity)() "Not a valid identity")
  (concatenate 'string "I" identity))

;;; (time (identity->column-label (makeid)))

(defmethod column-label? (thing) nil)

(defmethod column-label? ((thing string))
  (and (= 33 (length thing))
       (char= #\I (elt thing 0))
       (let ((result t))
         (block checking
           (loop for i from 1 below (length thing)
              do (unless (find (elt thing i) "0123456789abcdefABCDEF")
                   (setf result nil)
                   (return-from checking nil))))
         result)))

;;; (time (column-label? (identity->column-label (makeid))))
;;; (time (column-label? "Foo"))
;;; (time (column-label? "I12341234Z"))

(defmethod column-label->identity ((lbl string))
  (assert (column-label? lbl)() "Not a valid column-label")
  (subseq lbl 1))

;;; (time (column-label->identity (identity->column-label (makeid))))

(defmethod column-label->uuid ((lbl string))
  (assert (column-label? lbl)() "Not a valid column-label")
  (identity->uuid (column-label->identity lbl)))

;;; (time (column-label->uuid (identity->column-label (makeid))))
