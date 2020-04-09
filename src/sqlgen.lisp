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

;;; =====================================================================
;;; helper functions
;;; =====================================================================

(defun make-metadata-params (optype opid origin revision timestamp item name deleted peer)
  (values (list "optype" "opid" "origin" "revision" "timestamp" "item" "name" "deleted" "peer")
          (list optype opid origin revision timestamp item name deleted peer)))

(defun make-column-params (column-data)
  (values (mapcar (lambda (c)(fset:@ c :|id|)) column-data)
          (mapcar #'to-json column-data)))

(defun make-item-params (column-data vals)
  (assert (= (length column-data)(length vals))()
          "The number of values must equal the number of columns")
  (values (mapcar (lambda (c)(fset:@ c :|id|)) column-data)
          vals))

;;; =====================================================================
;;; SQL constructor functions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; sql-create-delectus-table
;;; ---------------------------------------------------------------------

(defun sql-create-delectus-table ()
  (values
   "CREATE TABLE `delectus` ( `id` TEXT, `origin` TEXT, `format` TEXT, `next_revision` INTEGER )"
   nil))

;;; (sql-create-delectus-table)

;;; ---------------------------------------------------------------------
;;; sql-populate-delectus-table
;;; ---------------------------------------------------------------------

(defun sql-populate-delectus-table (id origin format next-revision)
  (values
   "INSERT INTO `delectus` (`id`, `origin`, `format`, `next_revision`) VALUES (?, ?, ?, ?)"
   (list id origin format next-revision)))

;;; (sql-populate-delectus-table (delectus::makeid) delectus::*origin* delectus::+delectus-format-version+ 3)

;;; ---------------------------------------------------------------------
;;; sql-create-listdata-table
;;; ---------------------------------------------------------------------

(defun sql-create-listdata-table ()
  (values
   "CREATE TABLE `list_data` ( `optype` TEXT, `opid` TEXT, `origin` TEXT, `revision` INTEGER, `timestamp` TEXT, `item` TEXT, `name` TEXT, `deleted` TEXT, `peer` TEXT )"
   nil))

;;; (sql-create-listdata-table)

;;; ---------------------------------------------------------------------
;;; sql-add-userdata-column
;;; ---------------------------------------------------------------------

(defun sql-add-userdata-column (label type)
  (values
   (format nil "ALTER TABLE `list_data` ADD `~A` ~A"
           label type)
   nil))

;;; (sql-add-userdata-column (makeid) "TEXT")

;;; ---------------------------------------------------------------------
;;; sql-list-id
;;; ---------------------------------------------------------------------

(defun sql-list-id ()
  (values
   "SELECT `id` FROM `delectus` LIMIT 1"
   nil))

;;; ---------------------------------------------------------------------
;;; sql-list-origin
;;; ---------------------------------------------------------------------

(defun sql-list-origin ()
  (values
   "SELECT `origin` FROM `delectus` LIMIT 1"
   nil))

;;; ---------------------------------------------------------------------
;;; sql-list-format
;;; ---------------------------------------------------------------------

(defun sql-list-format ()
  (values
   "SELECT `format` FROM `delectus` LIMIT 1"
   nil))

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

;;; ---------------------------------------------------------------------
;;; sql-assert-listname
;;; ---------------------------------------------------------------------

(defun sql-assert-listname (opid origin revision timestamp item name deleted peer)
  (bind ((optype "listname")
         (params vals (make-metadata-params optype opid origin revision timestamp item name deleted peer))
         (placeholders (mapcar (constantly "?") params))
         (params-string (join-strings ", " params))
         (placeholders-string (join-strings ", " placeholders))
         (sql (format nil "INSERT INTO `list_data` (~A) VALUES (~A)"
                      params-string placeholders-string)))
    (values sql vals)))

;;; (sql-assert-listname (makeid)(makeid) 3 (now-timestamp) nil "A List" nil nil)

;;; ---------------------------------------------------------------------
;;; sql-assert-columns
;;; ---------------------------------------------------------------------

(defun sql-assert-columns (opid origin revision timestamp item name deleted peer &key column-data)
  (bind ((optype "columns")
         (meta-params meta-vals (make-metadata-params optype opid origin revision timestamp item name deleted peer))
         (column-params column-vals (make-column-params column-data))
         (params (append meta-params column-params))
         (vals (append meta-vals column-vals))
         (placeholders (mapcar (constantly "?") params))
         (params-string (join-strings ", " params))
         (placeholders-string (join-strings ", " placeholders))
         (sql (format nil "INSERT INTO `list_data` (~A) VALUES (~A)"
                      params-string placeholders-string)))
    (values sql vals)))

;;; (setf $col0 {:|id| (makeid) :|name| "Column 0" :|type| :false :|order| 10.0 :|sort| "ASC" :|title| :true :|subtitle| :false :|deleted| :false})
;;; (setf $col1 {:|id| (makeid) :|name| "Column 1" :|type| :false :|order| 20.0 :|sort| :false :|title| :false :|subtitle| :false :|deleted| :false})
;;; (setf $col2 {:|id| (makeid) :|name| "Column 2" :|type| :false :|order| 30.0 :|sort| :false :|title| :false :|subtitle| :false :|deleted| :false})
;;; (sql-assert-columns (makeid)(makeid) 3 (now-timestamp) nil nil nil nil :column-data (list $col0 $col1 $col2))


;;; ---------------------------------------------------------------------
;;; sql-assert-item
;;; ---------------------------------------------------------------------

(defun sql-assert-item (opid origin revision timestamp item name deleted peer &key column-data column-values)
  (bind ((optype "item")
         (meta-params meta-vals (make-metadata-params optype opid origin revision timestamp item name deleted peer))
         (item-params item-vals (make-item-params column-data column-values))
         (params (append meta-params item-params))
         (vals (append meta-vals item-vals))
         (placeholders (mapcar (constantly "?") params))
         (params-string (join-strings ", " params))
         (placeholders-string (join-strings ", " placeholders))
         (sql (format nil "INSERT INTO `list_data` (~A) VALUES (~A)"
                      params-string placeholders-string)))
    (values sql vals)))

;;; (setf $col0 {:|id| (makeid) :|name| "Column 0" :|type| :false :|order| 10.0 :|sort| "ASC" :|title| :true :|subtitle| :false :|deleted| :false})
;;; (setf $col1 {:|id| (makeid) :|name| "Column 1" :|type| :false :|order| 20.0 :|sort| :false :|title| :false :|subtitle| :false :|deleted| :false})
;;; (setf $col2 {:|id| (makeid) :|name| "Column 2" :|type| :false :|order| 30.0 :|sort| :false :|title| :false :|subtitle| :false :|deleted| :false})
;;; (sql-assert-item (makeid)(makeid) 3 (now-timestamp) (makeid) nil nil nil :column-data (list $col0 $col1 $col2) :column-values [0 1 2])


;;; ---------------------------------------------------------------------
;;; sql-assert-sync
;;; ---------------------------------------------------------------------

(defun sql-assert-sync (opid origin revision timestamp item name deleted peer)
  (bind ((optype "sync")
         (params vals (make-metadata-params optype opid origin revision timestamp item name deleted peer))
         (placeholders (mapcar (constantly "?") params))
         (params-string (join-strings ", " params))
         (placeholders-string (join-strings ", " placeholders))
         (sql (format nil "INSERT INTO `list_data` (~A) VALUES (~A)"
                      params-string placeholders-string)))
    (values sql vals)))

;;; (sql-assert-sync (makeid)(makeid) 3 (now-timestamp) nil nil nil (makeid))