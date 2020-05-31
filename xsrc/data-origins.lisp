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
;;; An origin is a 16-byte vector that identifies where an op came
;;; from. We compute one each time Delectus opens a file.
;;;
;;; We compute an origin by doing:
;;;
;;; 1. concatenate to a string:
;;;   - the process identity
;;;   - ":"
;;;   - the absolute pathname of the list file
;;;
;;; 2. hash the concatenated string with SHA256
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

