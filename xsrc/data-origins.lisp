;;;; ***********************************************************************
;;;;
;;;; Name:          data-origins.lisp
;;;; Project:       delectus 2
;;;; Purpose:       creating identifiers for ops
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; An origin is a 64-bit signed integer that identifies where an op
;;; came from. We compute one each time Delectus opens a file.
;;;
;;; We compute an origin by doing:
;;;
;;; 1. concatenate:
;;; - the process identity
;;; - ":"
;;; - the absolute pathname of the list file
;;;
;;; 2. hash the resulting string with SHA256
;;;
;;; 3. return the first 16 bytes of the hash; this is the origin value

;;; ---------------------------------------------------------------------
;;; origins
;;; ---------------------------------------------------------------------

(defparameter +origin-vector-length+ 16)

(defmethod make-origin ((process-id vector)(list-file pathname))
  (let* ((pidstr (identity->string process-id))
         (origstr (concatenate 'string
                               pidstr
                               ":"
                               (namestring list-file))))
    (coerce (subseq (ironclad:digest-sequence
                     :sha256
                     (babel:string-to-octets origstr))
                    0 16)
            '(simple-vector 16))))

;;; (make-origin (process-identity) (pathname "/Users/mikel/.emacs"))

(defmethod origin? (thing) nil)

(defmethod origin? ((thing vector))
  (and (= +origin-vector-length+
          (length thing))
       (every (lambda (x)(typep x '(unsigned-byte 8)))
              thing)
       t))

;;; (origin? (make-origin (process-identity) (path "~/.bashrc")))

;;; ---------------------------------------------------------------------
;;; origin strings
;;; ---------------------------------------------------------------------
;;; an origin-string is the first 22 characters of a base64-encoded origin.

(defparameter +delectus-origin-string-length+ 22)

(defmethod origin->string ((origin vector))
  (assert (origin? origin)() "Not a valid origin: ~S" origin)
  (subseq (binascii:encode-base64 origin)
          0 +delectus-origin-string-length+))

;;; (origin->string (make-origin (process-identity) (path "~/.bashrc")))

(defmethod make-origin-string ((process-id vector)(list-file pathname))
  (origin->string (make-origin process-id list-file)))

;;; (time (make-origin-string (process-identity) (path "~/.bashrc")))

(defmethod origin-string? (thing) nil)

(defmethod origin-string? ((thing string))
  (if (not (equal +delectus-origin-string-length+
                  (length thing)))
      nil
      (let ((result t))
        (block checking
          (loop for i from 0 below (length thing)
             do (unless (find (elt thing i) binascii::*base64-encode-table*)
                  (setf result nil)
                  (return-from checking nil))))
        result)))

;;; (origin-string? (make-origin-string (process-identity) (path "~/.bashrc")))

(defmethod string->origin ((origin-string string))
  (assert (origin-string? origin-string)() "Not a valid origin string: ~S" origin-string)
  (coerce (binascii:decode-base64 origin-string)
          '(simple-vector 16)))

;;; (string->origin (origin->string (make-origin (process-identity) (path "~/.bashrc"))))
;;; (string->origin (make-origin-string (process-identity) (path "~/.bashrc")))
