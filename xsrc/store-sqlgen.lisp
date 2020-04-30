;;;; ***********************************************************************
;;;;
;;;; Name:          store-sqlgen.lisp
;;;; Project:       delectus 2
;;;; Purpose:       generating SQL for Delectus store operations
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; sql
;;; ---------------------------------------------------------------------
;;; generate a trimmed SQL string

(defun sql (sql-strings &rest parameters)
  (let* ((format-string (join-strings " " sql-strings)))
    (apply 'format nil format-string parameters)))

;;; ---------------------------------------------------------------------
;;; sqlgen-create-delectus-table
;;; ---------------------------------------------------------------------

(defun sqlgen-create-delectus-table ()
  (values
   (sql ["CREATE TABLE `~A` ("
         " `listid` TEXT, "
         " `fileid` TEXT, "
         " `origin` TEXT, "
         " `parent` TEXT, "
         " `format` TEXT "
         ")"]
        *delectus-table-name*)
   nil))

;;; (sqlgen-create-delectus-table)


;;; ---------------------------------------------------------------------
;;; sqlgen-init-delectus-table
;;; ---------------------------------------------------------------------

(defun sqlgen-init-delectus-table (list-id file-id list-origin list-parent format-version)
  (values
   (sql ["INSERT INTO `~A` ("
         " `listid`, "
         " `fileid`, "
         " `origin`, "
         " `parent`, "
         " `format` "
         ")"
         "VALUES (?, ?, ?, ?, ?, ?, ?)"]
        *delectus-table-name*)
   (list list-id file-id list-origin list-parent format-version next-opid next-item)))

;;; (sqlgen-init-delectus-table (makeid)(makeid) *origin* (makeid) +delectus-format-version+)

;;; ---------------------------------------------------------------------
;;; sqlgen-create-listdata-table
;;; ---------------------------------------------------------------------

(defun sqlgen-create-listdata-table ()
  (values
   (sql ["CREATE TABLE `~A` ("
         " `optype` TEXT, "
         " `opid` INTEGER, "
         " `origin` TEXT, "
         " `timestamp` TEXT, "
         " `metadata` TEXT )"]
        *listdata-table-name*)
   nil))

;;; (sqlgen-create-listdata-table)


;;; ---------------------------------------------------------------------
;;; sqlgen-create-item-opid-origin-index
;;; ---------------------------------------------------------------------

(defun sqlgen-create-item-opid-origin-index ()
  (values
   (sql ["CREATE INDEX `~A` "
         "ON `~A` (JSON_EXTRACT(metadata, '$.item'), `opid`, `origin`)"
         "WHERE `optype`='item'"]
        *item-opid-origin-index-name* *listdata-table-name*)
   nil))

;;; (sqlgen-create-item-opid-origin-index)

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-sync-op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-sync-op (opid origin timestamp peer)
  )

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-listname-op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-listname-op (list-name opid origin timestamp)
  (assert (stringp list-name)() "You must supply a string list-name parameter; found ~S" list-name)
  (assert (stringp opid)() "You must supply an opid opid parameter; found ~S" opid)
  (assert (stringp origin)() "You must supply an iref origin parameter; found ~S" origin)
  (assert (stringp timestamp)() "You must supply a string timestamp parameter; found ~S" timestamp)
  (values
   (sql ["INSERT INTO `~A` ("
         " `optype`, "
         " `opid`, "
         " `origin`, "
         " `timestamp`, "
         " `metadata`) "
         "VALUES (?, ?, ?, ?, ?)"]
        *listdata-table-name*)
   (list "listname" opid origin timestamp nil)))

;;; (sqlgen-insert-listname-op "Test" (makeid) *origin* (now-timestamp))

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-columns-op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-columns-op (opid origin timestamp columns-data)
  )

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-item-op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-item-op (opid origin timestamp item-data)
  )


