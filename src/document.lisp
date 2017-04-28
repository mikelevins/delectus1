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
  (delectus.store:store-get-column-labels (store document)))

(defmethod visible-rows ((document document) &key 
                         (column-labels nil)
                         (count-limit nil)
                         (start-index 0)
                         (filter-text "")
                         (sort-column "rowid")
                         (sort-order :ascending))
  (delectus.store:store-get-rows (store document) 
                                 :column-labels column-labels 
                                 :count-limit count-limit
                                 :start-index start-index
                                 :filter-text filter-text
                                 :sort-column sort-column
                                 :sort-order sort-order))



;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (defparameter $doc (make-instance 'document :store $store))


