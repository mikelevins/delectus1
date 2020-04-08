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


;;; ---------------------------------------------------------------------
;;; sql-get-latest-listname
;;; ---------------------------------------------------------------------

(defun sql-get-latest-listname ()
  (values
   "SELECT * FROM `list_data` WHERE `optype`='listname' ORDER BY `revision` DESC, `origin` DESC LIMIT 1"
   nil))

;;; ---------------------------------------------------------------------
;;; sql-get-latest-columns
;;; ---------------------------------------------------------------------

(defun sql-get-latest-columns ()
  (values
   "SELECT * FROM `list_data` WHERE `optype`='columns' ORDER BY `revision` DESC, `origin` DESC LIMIT 1"
   nil))

;;; ---------------------------------------------------------------------
;;; sql-get-latest-items
;;; ---------------------------------------------------------------------

(defun sql-get-latest-items ()
  (values
   "SELECT a.*
    FROM (SELECT ROW_NUMBER() OVER (PARTITION BY item ORDER BY revision DESC, origin DESC) rank, *
          FROM `list_data` WHERE optype='item') a
    WHERE a.rank = 1 order by a.revision"
   nil))

;;; ---------------------------------------------------------------------
;;; sql-get-latest-sync
;;; ---------------------------------------------------------------------

(defun sql-get-latest-sync ()
  (values
   "SELECT * FROM `list_data` WHERE `optype`='sync' ORDER BY `revision` DESC, `origin` DESC LIMIT 1"
   nil))
