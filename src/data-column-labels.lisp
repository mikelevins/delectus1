;;;; ***********************************************************************
;;;;
;;;; Name:          data-column-labels.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for working with delectus column-labels
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; A column-label is a Delectus identity string with the letter "I"
;;; prepended; the initial "I" enables us to use them as SQLite
;;; column labels without pain

(defmethod identity->column-label ((id string))
  (concatenate 'string "I" id))

(defmethod identity->column-label ((id vector))
  (assert (identity-string? id)() "Not a valid identity-string: ~S" id)
  (identity->column-label (identity->string id)))

