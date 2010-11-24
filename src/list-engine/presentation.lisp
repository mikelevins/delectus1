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
   (sort-column :accessor sort-column :initarg :sort-column :initform nil) ; label
   (sort-type :accessor sort-type :initarg :sort-type :initform :alphabetical) ; :alphabetical or :numeric
   (sort-reversed? :accessor sort-reversed? :initarg :sort-reversed :initform nil)
   ;; filter
   (filter-text :accessor filter-text :initarg :filter-text :initform nil)
   ;; display
   (show-deleted? :accessor show-deleted? :initarg :show-deleted :initform t)
   ;; columns
   (deleted-columns :accessor deleted-columns :initarg :deleted-columns :initform nil)
   ;; rows
   (deleted-rows :accessor deleted-rows :initarg :deleted-rows :initform nil)
   (row-cache :accessor row-cache :initarg :row-cache :initform nil)))

;;; used for creating new empty delectus documents
(defun make-default-presentation ()
  (make-instance 'presentation :model (make-model :columns '("Items") :rows (list nil))))

(defmethod row-deleted? ((pres presentation)(i integer))
  (member i (deleted-rows pres)))

(defmethod mark-row-deleted! ((pres presentation)(i integer))
  (pushnew i (deleted-rows pres)))

(defmethod column-deleted? ((pres presentation)(col string))
  (member col (deleted-columns pres) :test #'equalp))

(defmethod mark-column-deleted! ((pres presentation)(col string))
  (pushnew col (deleted-columns pres) :test #'equalp))

(defmethod filter-match? ((pres presentation)(row vector))
  (when (filter-text pres)
    (some (lambda (e)(search (filter-string pres) e :test #'char-equal)) row)))

(defmethod update ((pres presentation))
  (when (changed? pres)
    (let* ((live-indexes (loop
                            for i from 0 upto (1- (length (rows (model pres))))
                            if (not (row-deleted? pres i)) collect i))
           (filtered-indexes (loop
                                for i in live-indexes
                                if (filter-match? pres (elt (rows (model pres)) i)) collect i))
           (sorted-indexes (sort-indexes pres filtered-indexes)))
      (setf (row-cache pres) (loop for i in sorted-indexes 
                                collect (elt (rows (model pres)) i)))
      (setf (changed? pres) nil))
    pres))

(defmethod columns ((pres presentation))
  (let ((cols (columns (model pres))))
    (if (show-deleted? pres)
        cols
        (seq:filter (lambda (col)(not (column-deleted? pres col))) cols))))

(defmethod rows ((pres presentation))
  (update pres)
  (loop for i across (row-indexes pres)
       collect (aref (rows (model pres)) i)))

(defmethod count-rows ((pres presentation))
  (update pres)
  (length (row-indexes pres)))

(defmethod find-column ((pres presentation)(column-label string))
  (find column-label (columns pres) :test (lambda (a b)(equalp a (label b)))))

(defmethod value-at ((pres presentation)(column-label string)(row-index integer))
  (update pres)
  (let ((col-index (index (find-column pres column-label)))
        (row-index (aref (row-indexes pres) row-index)))
    (aref (aref (rows (model pres)) row-index)
          col-index)))

(defmethod put-value-at! ((pres presentation)(column-label string)(row-index integer) val)
  (setf (aref (aref (rows (model pres)) row-index)
              col-index)
        val)
  (setf (changed? pres) t)
  val)
