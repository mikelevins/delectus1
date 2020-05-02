;;;; ***********************************************************************
;;;;
;;;; Name:          data-identities.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with delectus identities
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)
(in-readtable :delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------

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
