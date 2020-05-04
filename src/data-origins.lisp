;;;; ***********************************************************************
;;;;
;;;; Name:          origins.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with delectus origins
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; computing origins
;;; ---------------------------------------------------------------------

(defmethod make-origin-string ((process-id vector) (list-file pathname))
  (assert (uiop/pathname:absolute-pathname-p list-file)()
          "Expected a full pathname; found ~S" list-file)
  (let ((path-bytes (babel:string-to-octets (namestring list-file))))
    (concatenate 'string
                 "origin://"
                 (identity->string process-id)
                 ":"
                 (namestring list-file))))

;;; (make-origin-string (process-identity)(pathname "/Users/mikel/.emacs"))

(defmethod make-origin ((process-id vector) (list-file pathname))
  (ironclad:digest-sequence :shake128
                            (babel:string-to-octets (make-origin-string process-id list-file))))

;;; (make-origin (process-identity)(pathname "/Users/mikel/.emacs"))

;;; ---------------------------------------------------------------------
;;; origins registry
;;; ---------------------------------------------------------------------
;;; used to find the pathname used to create an origin in the current
;;; session

(defparameter *origin-pathnames* {})

(defun origin-pathnames () *origin-pathnames*)

(defun set-origin-pathnames (new-origin-pathnames)
  (setf *origin-pathnames*
        new-origin-pathnames))

(defmethod find-origin-pathname ((origin vector))
  (get-key (origin-pathnames)
           origin))

(defmethod register-origin-pathname ((origin vector)(path pathname))
  (set-origin-pathnames (merge-maps (origin-pathnames)
                                    {origin path}))
  (origin-pathnames))

;;; (setf $orig (make-origin (process-identity)(pathname "/Users/mikel/.emacs")))
;;; (equalp (make-origin (process-identity)(pathname "/Users/mikel/.emacs"))(make-origin (process-identity)(pathname "/Users/mikel/.emacs")))
;;; (register-origin-pathname $orig (pathname "/Users/mikel/.emacs"))
;;; (find-origin-pathname $orig)
