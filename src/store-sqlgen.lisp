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
;;; helper functions
;;; ---------------------------------------------------------------------

;;; generate a string of parameter names joined by ", "
(defun %make-parameters-string (parameter-names)
  (join-strings ", "
                (mapcar (lambda (p)(format nil "`~A`" p))
                        parameter-names)))

;;; generate a "?" for each argument
(defun %make-placeholders-string (parameters)
  (join-strings ", "
                (mapcar (constantly "?") parameters)))

;;; ---------------------------------------------------------------------
;;; sql
;;; ---------------------------------------------------------------------
;;; generate a trimmed SQL string

(defun sql (sql-strings &rest parameters)
  (let* ((format-string (join-strings " " sql-strings)))
    (apply 'format nil format-string parameters)))

;;; ---------------------------------------------------------------------
;;; sqlgen-get-max-opid
;;; ---------------------------------------------------------------------

(defun sqlgen-get-max-opid ()
  (values
   (sql ["SELECT MAX(`~A`) FROM `~A`"]
        *opid-column-name*
        *listdata-table-name*)
   nil))

;;; (sqlgen-get-max-opid)

;;; ---------------------------------------------------------------------
;;; sqlgen-get-max-item
;;; ---------------------------------------------------------------------

(defun sqlgen-get-max-item ()
  (values
   (sql ["SELECT MAX(`~A`) FROM `~A`"]
        *item-column-name*
        *listdata-table-name*)
   nil))

;;; (sqlgen-get-max-item)

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
         " `optype` TEXT, "
         " `opid` INTEGER, "
         " `origin` TEXT, "
         " `timestamp` TEXT, "
         " `peer` TEXT, "
         " `file` TEXT, "
         " `name` TEXT, "
         " `item` INTEGER, "
         " `deleted` INTEGER "
         ")"]
        *listdata-table-name*)
   nil))

;;; (sqlgen-create-listdata-table)


;;; ---------------------------------------------------------------------
;;; sqlgen-add-userdata-column
;;; ---------------------------------------------------------------------

(defun sqlgen-add-userdata-column (column-id)
  (values
   (sql ["ALTER TABLE `~A`"
         "ADD `~A` TEXT"]
        *listdata-table-name* column-id)
   nil))

;;; (sqlgen-add-userdata-column (makeid))


;;; ---------------------------------------------------------------------
;;; sqlgen-create-item-opid-origin-index
;;; ---------------------------------------------------------------------

(defun sqlgen-create-item-opid-origin-index ()
  (values
   (sql ["CREATE INDEX `~A`"
         "ON `~A` (`item`, `opid`, `origin`)"
         "WHERE `optype`='item'"]
        *item-opid-origin-index-name* *listdata-table-name*)
   nil))

;;; (sqlgen-create-item-opid-origin-index)

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-sync-op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-sync-op (opid origin timestamp peer)
  (values
   (sql [])
   (list  *sync-optype* )))

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-listname-op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-listname-op (list-name opid origin timestamp)
  (assert (stringp list-name)() "You must supply a string list-name parameter; found ~S" list-name)
  (assert (integerp opid)() "You must supply an integer opid parameter; found ~S" opid)
  (assert (stringp origin)() "You must supply an iref origin parameter; found ~S" origin)
  (assert (stringp timestamp)() "You must supply a string timestamp parameter; found ~S" timestamp)
  (values
   (sql ["INSERT INTO `~A` ("
         " `optype`, "
         " `opid`, "
         " `origin`, "
         " `timestamp`, "
         " `peer`, "
         " `file`, "
         " `name`, "
         " `item`, "
         " `deleted` "
         ")"
         "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"]
        *listdata-table-name*)
   (list *listname-optype* opid origin timestamp nil nil list-name nil nil)))

;;; (sqlgen-insert-listname-op "Test" (makeid) *origin* (now-timestamp))

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-columns-op
;;; ---------------------------------------------------------------------

;;; (join-strings ", " ["1" "2" "3" "4" "5"])

;;; (%sql-column-json-objects (make-default-columns-data))

(defun sqlgen-insert-columns-op (opid origin timestamp columns-map)
  (let* ((peer nil)
         (file nil)
         (name nil)
         (item nil)
         (deleted nil)
         (column-parameters (get-keys columns-map))
         (column-parameters-string (%make-parameters-string column-parameters))
         (placeholders-string (%make-placeholders-string
                               (append [:optype opid origin timestamp peer file name item deleted]
                                       (get-keys columns-map))))
         (column-objects (get-values columns-map)))
    (values
     (sql ["INSERT INTO `~A` ("
           " `optype`, "
           " `opid`, "
           " `origin`, "
           " `timestamp`, "
           " `peer`, "
           " `file`, "
           " `name`, "
           " `item`, "
           " `deleted`, "
           column-parameters-string
           ") "
           "VALUES (" placeholders-string ")"]
          *listdata-table-name*)
     (append [*columns-optype* opid origin timestamp peer file name item deleted]
             column-objects))))

;;; (sqlgen-insert-columns-op 1 *origin* (now-timestamp) {(makeid) (to-json (make-default-userdata-column))})

;;; ---------------------------------------------------------------------
;;; sqlgen-insert-item-op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-item-op (opid origin timestamp item deleted? values-map)
  (let* ((peer nil)
         (file nil)
         (name nil)
         (column-ids (get-keys values-map))
         (column-values (get-values values-map))
         (item-parameters-string (%make-parameters-string column-ids))
         (placeholders-string (%make-placeholders-string
                               (append [:optype opid origin timestamp peer file name item deleted?]
                                       (get-keys values-map)))))
    (values
     (sql ["INSERT INTO `~A` ("
           " `optype`, "
           " `opid`, "
           " `origin`, "
           " `timestamp`, "
           " `peer`, "
           " `file`, "
           " `name`, "
           " `item`, "
           " `deleted`, "
           item-parameters-string
           ") "
           "VALUES (" placeholders-string ")"]
          *listdata-table-name*)
     (append [*item-optype* opid origin timestamp peer file name item deleted?]
             column-values))))

;;; (sqlgen-insert-item-op 1 *origin* (now-timestamp) 1 nil {(makeid) nil})
