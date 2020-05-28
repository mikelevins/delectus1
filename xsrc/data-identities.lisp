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
;;; functions for creating and converting between Delectus identities
;;; and identity-strings.

;;; ---------------------------------------------------------------------
;;; identities
;;; ---------------------------------------------------------------------
;;; an identity is a v4 UUID byte array.

(in-package #:delectus)

(defmethod identity? (thing) nil)

(defmethod identity? ((thing vector))
  (and (vectorp thing)
       (every (lambda (x)(typep x '(unsigned-byte 8)))
              thing)
       t))

;;; (identity? (makeid))
;;; (identity? "foo")

(defmethod uuid->identity ((u uuid:uuid))
  (uuid:uuid-to-byte-array u))

(defmethod identity->uuid ((identity vector))
  (assert (identity identity)() "Not a valid identity: ~S" identity)
  (uuid:byte-array-to-uuid identity))

(defmethod makeid ()
  (coerce (uuid->identity (uuid:make-v4-uuid))
          '(simple-vector 16)))

;;; (time (makeid))
;;; (setf $u (uuid:make-v4-uuid))
;;; (setf $id (uuid->identity $u))
;;; (setf $u2 (identity->uuid $id))

;;; ---------------------------------------------------------------------
;;; identity strings
;;; ---------------------------------------------------------------------
;;; an identity-string is an identity printed to a downcased
;;; hexadecimal string.

(defmethod identity->string ((id vector))
  (assert (identity id)() "Not a valid identity: ~S" id)
  (string-downcase
   (with-output-to-string (out)
     (loop for b across id
        do (format out "~2,'0x" b)))))

;;; (identity->string (makeid))

(defparameter +delectus-identity-string-length+
  (length (identity->string (makeid))))

(defmethod identity-string? (thing) nil)

(defmethod identity-string? ((thing string))
  (and (= 32 (length thing))
       (let ((result t))
         (block checking
           (loop for i from 0 below (length thing)
              do (unless (find (elt thing i) "0123456789abcdefABCDEF")
                   (setf result nil)
                   (return-from checking nil))))
         result)))

(defun make-identity-string ()
  (identity->string (makeid)))

;;; (time (make-identity-string))
;;; (identity-string? (make-identity-string))

(defmethod string->identity ((identity string))
  (assert (identity-string? identity)() "Not a valid identity-string: ~S" identity)
  (let* ((uuid-string (join-strings "-"
                                    (list (subseq identity 0 8)
                                          (subseq identity 8 12)
                                          (subseq identity 12 16)
                                          (subseq identity 16 20)
                                          (subseq identity 20))))
         (uuid (uuid:make-uuid-from-string uuid-string)))
    (coerce (uuid:uuid-to-byte-array uuid)
            '(simple-vector 16))))

;;; (string->identity (identity->string (makeid)))
;;; (string->identity (make-identity-string))
