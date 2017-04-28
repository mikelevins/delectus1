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

;;; document
;;; ---------------------------------------------------------------------
;;; EXPORTED CLASS
;;; The type of objects that represent Delectus documents.

(defclass document ()
  ((store :accessor store :initform nil :initarg :store)
   (command-queue :accessor command-queue :initform nil)
   (undo-stack :accessor undo-stack :initform nil)
   (redo-stack :accessor redo-stack :initform nil)))

;;; visible-column-labels (document)
;;; ---------------------------------------------------------------------
;;; EXPORTED GENERIC FUNCTION
;;;
;;; returns the visible column labels of DOCUMENT in user-defined order.
;;; the visible labels are all labels other than the hidden Delectus-defined
;;; "rowid" label. The column order is controlled by user interaction with
;;; the view.

(defmethod visible-column-labels ((document document))
  (delectus.store:store-get-column-labels (store document)))

;;; visible-rows (document) &key
;;;              (column-labels nil)
;;;              (count-limit nil)
;;;              (start-index 0)
;;;              (filter-text "")
;;;              (sort-column "rowid")
;;;              (sort-order :ascending)
;;; ---------------------------------------------------------------------
;;; EXPORTED GENERIC FUNCTION
;;;
;;; returns all values from the COLUMN-LABELS columns of DOCUMENT
;;; that match FILTER-TEXT. Results are limited to COUNT-LIMIT rows
;;; starting from START-INDEX. Results are sorted by column
;;; SORT-COLUMN in SORT-ORDER direction (either :ascending or :descending)

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


