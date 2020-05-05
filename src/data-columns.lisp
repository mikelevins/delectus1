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
                             (id :null)
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
    {:|id| id
      :|name| name 
      :|order| order
      :|sort| sort
      :|title| title
      :|subtitle| subtitle
      :|deleted| deleted}))

(defmethod column-description-id ((column-description wb-map))
  (get-key column-description :|id| nil))

(defmethod column-description-to-json ((desc wb-map))
  (to-json desc))

;;; (column-description-to-json (column-description :id (make-identity-string) :name "Item"))
;;; (column-description-id (column-description :id (make-identity-string) :name "Item"))
