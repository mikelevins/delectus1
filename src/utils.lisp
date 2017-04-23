;;;; ***********************************************************************
;;;;
;;;; Name:          utils.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       general-purpose utility functions 
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; list utilities
;;; ---------------------------------------------------------------------

(defun list-difference  (src-list sublist &key (test 'eql))
  (labels ((discard (discard-list target-list &key (test 'eql))
             (if (null discard-list)
                 target-list
                 (discard (cdr discard-list)
                          (remove (car discard-list)
                                  target-list :test test)
                          :test test))))
    (discard sublist src-list :test test)))

(defun interpose (it things)
  (if (null (cdr things))
      things
    (cons (car things)
          (cons it
                (interpose it (cdr things))))))

