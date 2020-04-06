;;;; ***********************************************************************
;;;;
;;;; Name:          delectus-sql.lisp
;;;; Project:       delectus 2
;;;; Purpose:       model-specific sql constructors
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

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
;;; sql-create-list_data-table
;;; ---------------------------------------------------------------------

(defun sql-create-list_data-table ()
  (values
   "CREATE TABLE `list_data` ( `optype` TEXT, `opid` TEXT, `origin` TEXT, `revision` INTEGER, `timestamp` TEXT, `item` TEXT, `name` TEXT, `deleted` TEXT, `peer` TEXT )"
   nil))

;;; (sql-create-list_data-table)

;;; ---------------------------------------------------------------------
;;; sql-add-userdata-column
;;; ---------------------------------------------------------------------

(defun sql-add-userdata-column (label type)
  (values
   (format nil "ALTER TABLE `list_data` ADD `~A` ~A"
           label type)
   nil))

;;; (sql-add-userdata-column (delectus::makeid) "TEXT")

;;; ---------------------------------------------------------------------
;;; sql-assert-op
;;; ---------------------------------------------------------------------

(defun sql-assert-op (optype opid origin revision timestamp item name deleted peer &key column-data)
  (let* ((column-ids (mapcar #'car column-data))
         (column-values (mapcar #'cdr column-data))
         (insert-args (append `("optype" "opid" "origin" "revision" "timestamp"
                                         "item" "name" "deleted" "peer" ,@column-ids)))
         (insert-arg-strings (mapcar (lambda (a)(format nil "`~A`" a))
                                     insert-args))
         (insert-arg-placeholders (mapcar (constantly "?") insert-args))
         (insert-args-text (delectus::join-strings ", " insert-arg-strings))
         (insert-placeholders-text (delectus::join-strings ", " insert-arg-placeholders))
         (sql (format nil "INSERT INTO `list_data` (~A) VALUES (~A)"
                      insert-args-text insert-placeholders-text)))
    (values sql
            `(,optype ,opid ,origin ,revision ,timestamp ,item ,name ,deleted ,peer ,@column-values))))

;;; (sql-assert-op "item" (makeid) *origin* 2 (now-timestamp) (makeid) nil nil nil :column-data `((,(makeid) . 1)))

;;; ---------------------------------------------------------------------
;;; sql-get-column-attributes
;;; ---------------------------------------------------------------------

(defun sql-get-column-attributes ()
  (values "PRAGMA table_info(list_data);"
          nil))

;;; ---------------------------------------------------------------------
;;; sql-get-column-values
;;; ---------------------------------------------------------------------

(defun sql-get-column-values ()
  )

;;; ---------------------------------------------------------------------
;;; sql-get-latest-listname-op
;;; ---------------------------------------------------------------------

(defun sql-get-latest-listname-op ()
  (values
   "SELECT * FROM `list_data` WHERE `optype`='listname' ORDER BY `revision` DESC, `origin` DESC LIMIT 1"
   nil))

;;; (sql-get-latest-listname-op)

;;; ---------------------------------------------------------------------
;;; sql-get-latest-columns-op
;;; ---------------------------------------------------------------------

(defun sql-get-latest-columns-op ()
  (values
   "SELECT * FROM `list_data` WHERE `optype`='columns' ORDER BY `revision` DESC, `origin` DESC LIMIT 1"
   nil))

;;; (sql-get-latest-columns-op)


;;; ---------------------------------------------------------------------
;;; sql-get-latest-item-ops
;;; ---------------------------------------------------------------------

(defun sql-get-latest-item-ops ()
  (values
   "SELECT a.*
    FROM (SELECT ROW_NUMBER() OVER (PARTITION BY item ORDER BY revision DESC, origin DESC) rank, *
          FROM `list_data` WHERE optype='item') a
    WHERE a.rank = 1 order by a.revision"
   nil))

;;; (sql-get-latest-item-ops)

;;; ---------------------------------------------------------------------
;;; sql-get-latest-sync-op
;;; ---------------------------------------------------------------------

(defun sql-get-latest-sync-op ()
  (values
   "SELECT * FROM `list_data` WHERE `optype`='sync' ORDER BY `revision` DESC, `origin` DESC LIMIT 1"
   nil))

;;; (sql-get-latest-sync-op)
