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

;;; constructing columndata
;;; ---------------------------------------------------------------------

(defun columndata (&key id name type order sort title subtitle deleted)
  {:|id| (or id :false)
    :|name| (or name :false)
    :|type| (or type :false)
    :|order| (or order :false)
    :|sort| (or sort :false)
    :|title| (or title :false)
    :|subtitle| (or subtitle :false)
    :|deleted| (or deleted :false)})

;;; (columndata :id (makeid))
