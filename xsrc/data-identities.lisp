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
;;; an identity-string is a base64-encoded identity.

(defparameter +delectus-identity-string-length+ 22)

(defmethod identity->string ((id vector))
  (assert (identity id)() "Not a valid identity: ~S" id)
  (subseq (string-downcase
           (binascii:encode-base64 id))
          0 +delectus-identity-string-length+))

;;; (identity->string (makeid))

(defun make-identity-string ()
  (identity->string (makeid)))

;;; (time (make-identity-string))

(defmethod string->identity ((identity string))
  (coerce (binascii:decode-base64 identity)
          '(simple-vector 16)))

;;; (string->identity (identity->string (makeid)))
;;; (string->identity (make-identity-string))
