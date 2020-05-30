;;;; ***********************************************************************
;;;;
;;;; Name:          data-column-descriptions.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with descriptions of delectus columns
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package #:delectus)

(defparameter +column-description-keys+
  [:|deleted|
    :|label|
    :|name| 
    :|order|
    :|sort|
    :|subtitle|
    :|title|])

(defun column-description (&key
                             (label nil)
                             (name :null)
                             (order nil)
                             (sort :null)
                             (title :null)
                             (subtitle :null)
                             (deleted :null))
  (assert (column-label? label)() "Invalid column-label: ~S" label)
  (let* ((name (cond ((null name) label)
                     ((eq :null name) :null)
                     ((stringp name) name)
                     (t (error "Invalid column name: ~S" name))))
         (order (cond ((null order) *minimum-column-order*)
                      ((eq :null order) :null)
                      ((numberp order) order)
                      (t (error "Invalid column order: ~S" order))))
         (sort (cond ((null sort) :false)
                     ((eq :null sort) :false)
                     ((eq :false sort) :false)
                     ((equal sort "ASC") sort)
                     ((equal sort "DESC") sort)
                     (t (error "Invalid sort column: ~S" sort))))
         (title (cond ((null title) :false)
                      ((eq :false title) :false)
                      ((eq :null title) :false)
                      (t t)))
         (subtitle (cond ((null subtitle) :false)
                         ((eq :false subtitle) :false)
                         ((eq :null subtitle) :false)
                         (t t)))
         (deleted (cond ((null deleted) :false)
                        ((eq :false deleted) :false)
                        ((eq :null deleted) :false)
                        (t t))))
    [:|label| label
      :|name| name 
      :|order| order
      :|sort| sort
      :|title| title
      :|subtitle| subtitle
      :|deleted| deleted]))

(defmethod column-description? (thing) nil)

(defmethod column-description? ((thing null)) nil)

(defmethod column-description? ((thing list))
  (and (evenp (length thing))
       (every (lambda (k)(find k +column-description-keys+))
              (loop for tail on thing by #'cddr collect (car tail)))))

(defmethod column-description-label ((column-description list))
  (getf column-description :|label| nil))

(defmethod column-description-name ((column-description list))
  (getf column-description :|name| nil))

(defmethod column-description-order ((column-description list))
  (getf column-description :|order| nil))

(defmethod column-description-sort ((column-description list))
  (getf column-description :|sort| nil))

(defmethod column-description-title ((column-description list))
  (getf column-description :|title| nil))

(defmethod column-description-subtitle ((column-description list))
  (getf column-description :|subtitle| nil))

(defmethod column-description-deleted ((column-description list))
  (getf column-description :|deleted| nil))

(defmethod column-description-to-json ((desc list))
  (jonathan:to-json desc))

;;; (column-description-to-json (column-description :label (make-column-label) :name "Item"))
;;; (column-description-label (column-description :label (make-column-label) :name "Item"))
;;; (column-description? (column-description :label (make-column-label) :name "Item"))
