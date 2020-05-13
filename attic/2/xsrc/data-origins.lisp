;;;; ***********************************************************************
;;;;
;;;; Name:          data-origins.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for computing unique IDs for list files
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; An origin is an identity computed as a hash from the Delectus
;;; node, the process id, and the pathname of the list file. If the
;;; origin of two ops is different, then they were inserted in
;;; different files, and their revision numbers are independent.
;;;
;;; For a given Delectus install and pathname, the origin will always
;;; be the same, however, deleting the nodeid will cause Delectus to
;;; write a new one, and all subsequent origins will use the new
;;; nodeid; the old one will be lost forever. This is not a problem;
;;; Delectus will simply treat ops created under the lost origin as
;;; having been created by a different node.

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; computing origins
;;; ---------------------------------------------------------------------

(defmethod make-origin-string ((nodeid vector) (list-file pathname))
  (assert (uiop/pathname:absolute-pathname-p list-file)()
          "Expected a full pathname; found ~S" list-file)
  (let ((path-bytes (babel:string-to-octets (namestring list-file))))
    (concatenate 'string
                 (identity->string nodeid)
                 ":"
                 (namestring list-file))))

;;; (make-origin-string (delectus-node-identity)(pathname "/Users/mikel/.emacs"))

(defmethod make-origin ((process-id vector) (list-file pathname))
  (ironclad:digest-sequence :shake128
                            (babel:string-to-octets (make-origin-string process-id list-file))))

;;; (make-origin (delectus-node-identity)(pathname "/Users/mikel/.emacs"))
