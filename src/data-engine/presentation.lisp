;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          presentation.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       dynamic views of the Delectus data model
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; the model is the data stored in a Delectus document. the presentation
;;; is the state in which it is to be presented to the user. presentation
;;; manages matters such as: whether to display imtes that have been marked
;;; for deletion; in what order to sort the rows; and so on. UI and API
;;; code that updates and queries the model should always do so through
;;; the presentation.

(defclass presentation ()
  ((model :reader model :initarg :model :initform (make-model))
   (changed? :accessor changed? :initform t)
   ;; sort
   (sort-column :accessor sort-column :initform nil) ; label
   (sort-type :accessor sort-type :initform :alphabetical) ; :alphabetical or :numeric
   (sort-reversed? :accessor sort-reversed? :initform nil)
   ;; filter
   (filter-text :accessor filter-text :initform nil)
   ;; display
   (show-deleted? :accessor show-deleted? :initform nil)))

;;; used for creating new empty delectus documents
(defun make-default-presentation ()
  (make-instance 'presentation :model (make-model :columns '("Items") :rows (list (ensure-row nil)))))

(defmethod update ((pres presentation))
  )

(defmethod columns ((pres presentation))
  (let ((cols (columns (model pres))))
    (if (show-deleted? pres)
        cols
        (seq:filter (lambda (col)(not (deleted? col))) cols))))

(defmethod count-rows ((pres presentation))
  (seq:length (rows (model pres))))

(defmethod value-at ((pres presentation)(column-label string)(row-index integer))
  (value-at (model pres) column-label row-index))


