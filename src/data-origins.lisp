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
;;; *origins*
;;; ---------------------------------------------------------------------
;;; *origins* is a registry of origins that we've created in the current
;;; session. You can use it to look up a pathname given an origin,
;;; or vice-versa. It works *only* with origins created in the current
;;; session; origins stored in a file in any other Delectus session
;;; cannot be mapped to their pathnames.

(defparameter *origins*
  {:origin->pathname-map {}
   :pathname->origin-map {}})

(defun origins () *origins*)
(defun set-origins (new-origins)
  (setf *origins* new-origins))

(defmethod register-origin ((list-file pathname))
  (assert (uiop/pathname:absolute-pathname-p list-file)()
          "Expected a full pathname; found ~S" list-file)
  (let* ((process-id (process-identity))
         (file-origin (make-origin process-id list-file)))
    (set-origins
     {:origin->pathname-map (merge-maps (get-key (origins) :origin->pathname-map {})
                                        {file-origin list-file})
                            :pathname->origin-map (merge-maps (get-key (origins) :pathname->origin-map {})
                                                              {list-file file-origin})})))

(defmethod pathname->origin ((path pathname))
  (assert (uiop/pathname:absolute-pathname-p path)()
          "Expected a full pathname; found ~S" path)
  (get-key (get-key (origins) :pathname->origin-map {})
           path))

(defmethod origin->pathname ((origin vector))
  (get-key (get-key (origins) :origin->pathname-map {})
           origin))

;;; (register-origin (pathname "/Users/mikel/.emacs"))
;;; (pathname->origin (pathname "/Users/mikel/.emacs"))
;;; (origin->pathname (pathname->origin (pathname "/Users/mikel/.emacs")))

