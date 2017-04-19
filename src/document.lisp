;;;; ***********************************************************************
;;;;
;;;; Name:          document.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Delectus controller
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; document
;;; ---------------------------------------------------------------------

(defclass document ()
  ((store :accessor store :initform nil :initarg :store)
   (command-queue :accessor command-queue :initform nil)
   (undo-stack :accessor undo-stack :initform nil)
   (redo-stack :accessor redo-stack :initform nil)))

(defmethod visible-column-labels ((document document))
  (let* ((all-labels (store-column-labels (store document)))
         (deleted-labels (store-deleted-labels (store document))))
    (list-difference all-labels deleted-labels :test #'equalp)))

(defmethod visible-rows ((document document))
  (store-nondeleted-rows (store document)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (defparameter $doc (make-instance 'document :store $store))

