;;;; ***********************************************************************
;;;;
;;;; Name:          data-columns.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with descriptions of delectus columns
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package #:delectus)

(defun column-description (&key
                             (label nil)
                             (name :null)
                             (order nil)
                             (sort :null)
                             (title :null)
                             (subtitle :null)
                             (deleted :null))
  (let ((order (cond ((null order) *minimum-column-order*)
                     ((eq :null order) :null)
                     ((numberp order) order)
                     (t (error "Invalid column order: ~S" order)))))
    [:|label| label
      :|name| name 
      :|order| order
      :|sort| sort
      :|title| title
      :|subtitle| subtitle
      :|deleted| deleted]))

(defmethod column-description-label ((column-description list))
  (getf column-description :|label| nil))

(defmethod column-description-to-json ((desc list))
  (jonathan:to-json desc))

;;; (column-description-to-json (column-description :label (make-column-label) :name "Item"))
;;; (column-description-label (column-description :label (make-column-label) :name "Item"))
