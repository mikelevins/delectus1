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
         " `format` TEXT )"]
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
         " `format``)"
         "VALUES (?, ?, ?, ?, ?)"]
        *delectus-table-name*)
   (list list-id file-id list-origin list-parent format-version)))

;;; (sqlgen-init-delectus-table (makeid)(makeid) *origin* (makeid) +delectus-format-version+)

;;; ---------------------------------------------------------------------
;;; sqlgen-create-listdata-table
;;; ---------------------------------------------------------------------

(defun sqlgen-create-listdata-table ()
  (values
   (sql ["CREATE TABLE `~A` ("
         " `type` TEXT, "
         " `opid` TEXT, "
         " `origin` TEXT, "
         " `timestamp` TEXT, "
         " `metadata` TEXT )"]
        *listdata-table-name*)
   nil))

;;; (sqlgen-create-listdata-table)


;;; ---------------------------------------------------------------------
;;; sqlgen-create-item-revision-origin-index
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

(defun %sqlgen-columns-data-labels (columns-data)
  (mapcar #'as-string (get-keys columns-data)))

(defun %sqlgen-columns-data-objects (columns-data)
  (get-values columns-data))

(defun %sqlgen-columns-data-insert-parameters (columns-data)
  (let ((lbls (columns-data-labels columns-data)))
    (join-strings ", " lbls)))

(defun %sqlgen-columns-data-insert-placeholders (columns-data)
  (let* ((lbls (columns-data-labels columns-data))
         (placeholders (mapcar (constantly "?")
                               lbls)))
    (join-strings ", " placeholders)))

(defun %sqlgen-columns-data-insert-values (columns-data)
  (mapcar #'to-json (columns-data-objects columns-data)))

(defun sqlgen-insert-columns-op (opid origin timestamp columns-data)
  (assert (integerp opid)() "You must supply an iref opid parameter; found ~S" opid)
  (assert (integerp origin)() "You must supply an iref origin parameter; found ~S" origin)
  (assert (stringp timestamp)() "You must supply a string timestamp parameter; found ~S" timestamp)
  (let* ((column-labels (%sqlgen-columns-data-labels columns-data))
         (column-objects (%sqlgen-columns-data-objects columns-data))
         (insert-parameters-string (%sqlgen-columns-data-insert-parameters columns-data))
         (insert-placeholders-string (%sqlgen-columns-data-insert-placeholders columns-data))
         (insert-values (%sqlgen-columns-data-insert-values columns-data)))
    (values
     (delectus::trim
      (format nil
              "

INSERT INTO `~A` (`type`, `opid`, `origin`, `timestamp`, `name`, `item`, `deleted`, `peer`, ~A) 
VALUES (?, ?, ?, ?, ?, ?, ?, ?, ~A) 

" *listdata-table-name* insert-parameters-string insert-placeholders-string))
     `("columns" ,opid ,origin ,timestamp nil nil nil nil ,@insert-values))))

;;; (sqlgen-insert-columns-op 1 0 (now-timestamp) (make-default-columns-data))


