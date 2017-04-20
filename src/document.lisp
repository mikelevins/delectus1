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
         (hidden-labels (append +system-column-labels+
                                (store-deleted-labels (store document)))))
    (list-difference all-labels hidden-labels :test #'equalp)))

(defmethod visible-rows ((document document) &optional (column-labels nil))
  (store-nondeleted-rows (store document) column-labels))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (defparameter $doc (make-instance 'document :store $store))

