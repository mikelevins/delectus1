;;;; ***********************************************************************
;;;;
;;;; Name:          store.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Delectus data model
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; store
;;; ---------------------------------------------------------------------
;;; a class that represents data storage

;;; store
;;; ---------------------------------------------------------------------
(defclass store ()
  ((data-path :accessor data-path :initform nil :initarg :data-path)))

;;; initialize-instance :before ((store store) &rest initargs &key &allow-other-keys)
;;; ---------------------------------------------------------------------
;;; vet the data-path:
;;; - if it exists, make sure it's a Delectus 2 file, otherwise error
;;; - if it does not exist, create a Delectus 2 file
(defmethod initialize-instance :after ((store store) &rest initargs &key &allow-other-keys)
  (let ((probed-data-path (probe-file (data-path store))))
    (if probed-data-path
        (setf (data-path store)
              (ensure-delectus-file probed-data-path))
      (setf (data-path store) 
            (create-delectus-file (data-path store))))))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
