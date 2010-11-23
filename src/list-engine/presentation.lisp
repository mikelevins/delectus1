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
   (show-deleted? :accessor show-deleted? :initform t)))

;;; used for creating new empty delectus documents
(defun make-default-presentation ()
  (make-instance 'presentation :model (make-model :columns '("Items") :rows (list (ensure-row nil)))))

(defmethod update ((pres presentation))
  )

(defmethod columns ((pres presentation))
  (let ((cols (columns (model pres))))
    (if (show-deleted? pres)
        cols
        (seq:filter (lambda (col)(not (deleted? col))) (elements cols)))))

(defmethod rows ((pres presentation))
  (let* ((rows (elements (rows (model pres))))
         (live-rows (if (show-deleted? pres)
                        rows
                        (seq:filter (lambda (row)(not (deleted? row))) rows)))
         (filtered-rows (if (filter-text pres)
                            (filter-rows live-rows (filter-text pres))
                            live-rows))
         (sorted-rows (if (sort-column pres)
                          (sort-rows pres filtered-rows)
                          live-rows)))
    sorted-rows))

(defmethod count-rows ((pres presentation))
  (count-elements (rows (model pres))))

(defmethod find-column ((pres presentation)(column-label string))
  (find (lambda (col)(equalp (label col) column-label)) (elements (columns pres))))

(defmethod value-at ((pres presentation)(column-label string)(row-index integer))
  (element (element (rows pres) row-index)
           (index (find-column pres column-label))))


