;;;; ***********************************************************************
;;;;
;;;; Name:          columns.lisp
;;;; Project:       delectus 2
;;;; Purpose:       representing column attributes
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)
(in-readtable :delectus)

;;; column parameters
;;; ---------------------------------------------------------------------

(defparameter *maximum-column-count* 200) ; chosen to be below common database limits
(defparameter *column-order-interval* 10.0) ; default interval between order numbers autoassigned to new columns
(defparameter *minimum-column-order* 10.0)
(defparameter *maximum-column-order* (* *maximum-column-count* *column-order-interval*))

;;; SQLite column-info
;;; ---------------------------------------------------------------------

;;; the labels for the predefined metadata columns that every list has
(defparameter +metadata-column-labels+
  '("optype" "opid" "origin" "revision" "timestamp" "item" "name" "deleted" "peer"))

(defstruct (column-info (:constructor column-info (cid name type notnull dflt_value pk)))
  cid name type notnull dflt_value pk)

;;; Delectus column-data
;;; ---------------------------------------------------------------------

(defparameter +column-data-keys+
  (fset:set :|id| :|name| :|sort| :|type| :|order| :|title| :|deleted| :|subtitle|))

(defun column-data (&key id name type order sort title subtitle deleted)
  {:|id| (or id :false)
    :|name| (or name :false)
    :|type| (or type :false)
    :|order| (or order :false)
    :|sort| (or sort :false)
    :|title| (or title :false)
    :|subtitle| (or subtitle :false)
    :|deleted| (or deleted :false)})

;;; (column-data :id (makeid))

(defmethod column-data? (thing)
  (declare (ignore thing))
  nil)

(defmethod column-data? ((thing fset:wb-map))
  (eq :equal
      (fset:compare (fset:domain thing)
                    +column-data-keys+)))

(deftype column-data ()
  '(satisfies column-data?))

(defun make-column (&key id name type order sort title subtitle deleted)
  {:|id| (or id :false)
  :|name| (or name :false)
  :|type| (or type :false)
  :|order| (or order :false)
  :|sort| (or sort :false)
  :|title| (or title :false)
  :|subtitle| (or subtitle :false)
  :|deleted| (or deleted :false)})

(defparameter +default-initial-column-attributes+
  {:|name| "Item"
    :|type| "TEXT"
    :|order| 10.0
    :|sort| :false
    :|title| :false
    :|subtitle| :false
    :|deleted| :false})

;;; (to-json +default-initial-column-attributes+)
;;; (fset:with +default-initial-column-attributes+ :|id| (makeid))
