;;;; ***********************************************************************
;;;;
;;;; Name:          utils.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       general-purpose utility functions 
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.utilities)

;;; ---------------------------------------------------------------------
;;; list utilities
;;; ---------------------------------------------------------------------

;;; interpose (it things)
;;; ---------------------------------------------------------------------
;;; returns a new list created by inserting IT between each pair of
;;; elements in THINGS. If THINGS has one or fewer elements then the
;;; returned list is EQ to THINGS.

(defun interpose (it things)
  (if (null (cdr things))
      things
    (cons (car things)
          (cons it
                (interpose it (cdr things))))))


