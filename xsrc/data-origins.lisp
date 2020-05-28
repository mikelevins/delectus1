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
;;; We compute an origin by:
;;; 1. concatenate:
;;; - the process identity
;;; - ":"
;;; - the absolute pathname of the list file
;;; 2. hash the resulting string with SHA256
;;; 3. return the first 128 bits of the hash; this is the origin value

(defmethod make-origin-string ((process-id vector)(list-file pathname))
  (assert (uiop/pathname:absolute-pathname-p list-file)()
          "Expected a full pathname; found ~S" list-file)
  (let ((pid (identity->string process-id)))
    (concatenate 'string
                 pid
                 ":"
                 (namestring list-file))))

;;; (make-origin-string (process-identity) (pathname "/Users/mikel/.emacs"))

(defmethod make-origin ((process-id vector)(list-file pathname))
  ;; hash the origin string, then take the first 16 bytes
  (coerce (subseq (ironclad:digest-sequence
                   :sha256
                   (babel:string-to-octets (make-origin-string process-id list-file)))
                  0 16)
          '(simple-vector 16)))

;;; (make-origin (process-identity) (pathname "/Users/mikel/.emacs"))
