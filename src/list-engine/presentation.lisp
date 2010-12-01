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
   (show-deleted? :accessor show-deleted? :initarg :show-deleted :initform nil)
   ;; columns
   (deleted-columns :accessor deleted-columns :initarg :deleted-columns :initform nil)
   (column-cache :accessor column-cache :initarg :row-cache :initform nil)
   ;; rows
   (deleted-rows :accessor deleted-rows :initarg :deleted-rows :initform nil)
   (row-cache :accessor row-cache :initarg :row-cache :initform nil)))

(defmethod print-object ((pres presentation)(s stream))
  (print-unreadable-object (pres s :type t)
    (format s "~{~A ~}" (columns pres))))

(defun make-default-presentation ()
  (make-instance 'presentation
                 :model (make-model 
                         :columns '("Items")
                         :rows '(("")))))

;;; ---------------------------------------------------------------------
;;; row and column state
;;; ---------------------------------------------------------------------

(defmethod column-deleted? ((pres presentation)(label string))
  (member label (deleted-columns pres) :test #'equalp))

(defmethod mark-column-deleted! ((pres presentation)(label string)(deleted? (eql t)))
  (pushnew (find-column pres label)(deleted-columns pres) :test #'eql))

(defmethod mark-column-deleted! ((pres presentation)(label string)(deleted? (eql nil)))
  (setf (deleted-columns pres)
        (remove label (deleted-columns pres) :test #'equalp)))

(defmethod row-deleted? ((pres presentation)(row list))
  (member row (deleted-rows pres) :test #'eql))

(defmethod mark-row-deleted! ((pres presentation)(row list)(deleted? (eql t)))
  (pushnew row (deleted-rows pres) :test #'eql))

(defmethod mark-row-deleted! ((pres presentation)(row list)(deleted? (eql nil)))
  (setf (deleted-rows pres)
        (remove row (deleted-rows pres) :test #'eql)))

;;; ---------------------------------------------------------------------
;;; managing presentation state
;;; ---------------------------------------------------------------------

(defmethod mark-changed! ((pres presentation)(changed? (eql t)))
  (setf (changed? pres) changed?))

(defmethod mark-changed! ((pres presentation)(changed? (eql nil)))
  (setf (changed? pres) changed?))

(defmethod update-column-cache ((pres presentation))
  (if (show-deleted? pres)
      (setf (column-cache pres) (columns (model pres)))
      (setf (column-cache pres) (remove-if (partial #'column-deleted? pres)(columns (model pres))))))

(defmethod filter-rows ((rows list)(txt string))
  (remove-if-not (lambda (row)
                   (some (lambda (it)(search txt it :test #'char-equal))
                         items))
                 rows))

(defmethod sort-rows ((rows list)(index integer)(type (eql :alphabetical)) (reversed? (eql nil)))
  (merge-sort rows :test #'string< :key (partial (flip #'elt) index)))

(defmethod sort-rows ((rows list)(index integer)(type (eql :alphabetical)) (reversed? (eql t)))
  (merge-sort rows :test #'string> :key (partial (flip #'elt) index)))

(defmethod sort-rows ((rows list)(index integer)(type (eql :numeric)) (reversed? (eql nil)))
  (merge-sort rows :test #'< :key (partial (flip #'elt) index)))

(defmethod sort-rows ((rows list)(index integer)(type (eql :numeric)) (reversed? (eql t)))
  (merge-sort rows :test #'> :key (partial (flip #'elt) index)))

(defmethod update-row-cache ((pres presentation))
  (let* ((live-rows (if (show-deleted? pres)
                        (rows (model pres))
                        (remove-if (partial #'row-deleted? pres)(rows (model pres)))))
         (filtered-rows (if (filter-text pres)
                            (filter-rows live-rows (filter-text pres))
                            live-rows))
         (sorted-rows (if (sort-column pres)
                          (sort-rows filtered-rows (column-index pres (sort-column pres))
                                     (sort-type pres)(sort-reversed? pres))
                          filtered-rows)))
    (setf (row-cache pres) sorted-rows)))

(defmethod update ((pres presentation))
  (when (changed? pres)
    (update-column-cache pres)
    (update-row-cache pres)
    (mark-changed! pres nil)))

;;; ---------------------------------------------------------------------
;;; model API
;;; ---------------------------------------------------------------------

(defmethod columns ((pres presentation))
  (update pres)
  (column-cache pres))

(defmethod rows ((pres presentation))
  (update pres)
  (row-cache pres))

(defmethod find-column ((pres presentation)(column-label string))
  (find column-label (columns pres) :test #'equalp))

(defmethod column-index ((pres presentation)(column-label string))
  (position column-label (columns (model pres)) :test #'equalp))

(defmethod count-columns ((pres presentation))
  (length (columns pres)))

(defmethod count-rows ((pres presentation))
  (length (rows pres)))

(defmethod value-at ((pres presentation)(column-label string)(row-index integer))
  (update pres)
  (elt (elt (rows pres) row-index)
       (column-index pres column-label)))

(defmethod put-value-at! ((pres presentation)(column-label string)(row-index integer) (val string))
  (update pres)
  (setf (elt (elt (rows pres) row-index)
             (column-index pres column-label))
        val)
  (mark-changed! pres t)
  val)

(defmethod add-row! ((pres presentation) &optional vals)
  (add-row! (model pres) vals)
  (update pres)
  (mark-changed! pres t))

(defmethod add-column! ((pres presentation)(label string))
  (add-column! (model pres) label)
  (update pres)
  (mark-changed! pres t))
