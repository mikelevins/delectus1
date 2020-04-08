;;;; ***********************************************************************
;;;;
;;;; Name:          sqlgen.lisp
;;;; Project:       delectus 2
;;;; Purpose:       generating SQL for Delectus store operations
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)
(in-readtable :delectus)

;;; ---------------------------------------------------------------------
;;; sql-increment-next-revision
;;; ---------------------------------------------------------------------

(defun sql-increment-next-revision ()
  (values
   "UPDATE `delectus` SET `next_revision` = `next_revision` + 1"
   nil))

;;; ---------------------------------------------------------------------
;;; sql-next-revision
;;; ---------------------------------------------------------------------

(defun sql-next-revision ()
  (values
   "SELECT `next_revision` FROM `delectus` LIMIT 1"
   nil))
