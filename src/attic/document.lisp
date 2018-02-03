;;;; ***********************************************************************
;;;;
;;;; Name:          document.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Delectus controller
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.document)

;;; ---------------------------------------------------------------------
;;; document
;;; ---------------------------------------------------------------------
;;; *exported class*
;;; The type of objects that represent Delectus documents.

;;; ---------------------------------------------------------------------
;;; store (document)
;;; ---------------------------------------------------------------------
;;; *exported generic function*
;;;
;;; Returns the `store` slot of `document`.

(defclass document ()
  ((store :accessor store :initform nil :initarg :store)
   (command-queue :accessor command-queue :initform nil)
   (undo-stack :accessor undo-stack :initform nil)
   (redo-stack :accessor redo-stack :initform nil)))

;;; ---------------------------------------------------------------------
;;; visible-column-labels (document)
;;; ---------------------------------------------------------------------
;;; *exported generic function*
;;;
;;; Returns the visible column labels of `document` in user-defined
;;; order. The visible labels are all labels other than the hidden
;;; Delectus-defined `rowid` label. The column order is controlled by
;;; user interaction with the view.

(defmethod visible-column-labels ((document document))
  (delectus.store:store-get-column-labels (store document)))

;;; ---------------------------------------------------------------------
;;; visible-rows (document) &key
;;;              (column-labels nil)
;;;              (count-limit nil)
;;;              (start-index 0)
;;;              (filter-text "")
;;;              (sort-column "rowid")
;;;              (sort-order :ascending)
;;; ---------------------------------------------------------------------
;;; *exported generic function*
;;;
;;; Returns all values from the `column-labels` columns of `document`
;;; that match `filter-text`. Results are limited to `count-limit`
;;; rows starting from `start-index`. Results are sorted by column
;;; `sort-column` in `sort-order` direction (either `:ascending` or
;;; `:descending`).

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


;;; TEST CODE
;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (defparameter $doc (make-instance 'document :store $store))


