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
(in-readtable :interpol-syntax)


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
;;; SQL constructors
;;; =====================================================================
;;; each sql-constructor function returns two values: a SQL string, and
;;; a list of values to be bound to any '?' placeholders in the SQL
;;; string. If there are no placeholder variables in the SQL, then
;;; the second returned value is NIL.
;;;
;;; The function definitions look a bit unorthodox because they use
;;; interpolation syntax from cl-interpol to enable us to write the
;;; SQL code as clearly and readably as possible. This tends to make
;;; Lisp code wrapped around the SQL look a little awkward.


;;; ---------------------------------------------------------------------
;;; sql-create-delectus-table
;;; ---------------------------------------------------------------------

(defun sql-create-delectus-table ()
  (values
   (trim #?|

CREATE TABLE `delectus` ( `id` TEXT, `origin` TEXT, `format` TEXT, `next_revision` INTEGER )

|)
   nil))


;;; (sql-create-delectus-table)


;;; ---------------------------------------------------------------------
;;; sql-populate-delectus-table
;;; ---------------------------------------------------------------------

(defun sql-populate-delectus-table (id origin format next-revision)
  (values
   (trim #?|

INSERT INTO `delectus` (`id`, `origin`, `format`, `next_revision`) VALUES (?, ?, ?, ?)

|)
   (list id origin format next-revision)))

;;; (sql-populate-delectus-table (delectus::makeid) delectus::*origin* delectus::+delectus-format-version+ 3)


;;; ---------------------------------------------------------------------
;;; sql-create-listdata-table
;;; ---------------------------------------------------------------------

(defun sql-create-listdata-table ()
  (values
   (trim #?|

CREATE TABLE `list_data` ( 
  `optype` TEXT, 
  `opid` TEXT, 
  `origin` TEXT, 
  `revision` INTEGER, 
  `timestamp` TEXT, 
  `item` TEXT, 
  `name` TEXT, 
  `deleted` TEXT, 
  `peer` TEXT )

|)
   nil))

;;; (sql-create-listdata-table)


;;; ---------------------------------------------------------------------
;;; sql-create-item-revision-origin-index
;;; ---------------------------------------------------------------------

(defun sql-create-item-revision-origin-index ()
  (values
   (trim #?|

CREATE INDEX `idx_item_revision_origin` ON `list_data` (`item`, `revision`, `origin`)

|)
   nil))

;;; (sql-create-item-revision-origin-index)


;;; ---------------------------------------------------------------------
;;; sql-add-userdata-column
;;; ---------------------------------------------------------------------

(defun sql-add-userdata-column (label type)
  (values
   (trim #?|

ALTER TABLE `list_data` ADD `${label}` ${type}

|)
   nil))

;;; (sql-add-userdata-column (makeid) "TEXT")


;;; ---------------------------------------------------------------------
;;; sql-get-column-info
;;; ---------------------------------------------------------------------

(defun sql-get-column-info ()
  (values "PRAGMA table_info(list_data);"
          nil))

;;; (sql-get-column-info)


;;; ---------------------------------------------------------------------
;;; sql-list-id
;;; ---------------------------------------------------------------------

(defun sql-list-id ()
  (values
   (trim #?|

SELECT `id` FROM `delectus` LIMIT 1

|)
   nil))

;;; (sql-list-id)


;;; ---------------------------------------------------------------------
;;; sql-list-origin
;;; ---------------------------------------------------------------------

(defun sql-list-origin ()
  (values
   (trim #?|

SELECT `origin` FROM `delectus` LIMIT 1

|)
   nil))

;;; (sql-list-origin)


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
;;; ORDER BY revision DESC means the rows are sorted in descending
;;; order of revision
;;; PARTITION BY item means results rows are next sorted so that all rows
;;; with the same item are together
;;; ROW_NUMBER() assigns a row number to each row in a result
;;; ROW_NUMBER() OVER restarts the numbering for each partition in the argument to OVER()
;;;
;;; the outer SELECT a.* FROM ... WHERE a.rank = 1 ORDER BY a.revision
;;; then selects all of the rows with rank 1 in the inner SELECT ROW_NUMBER()

(defun sql-get-latest-items (&key (offset 0)(limit nil))
  (let ((offset-clause (if (and limit offset)
                           (format nil "OFFSET ~D " offset)
                           ""))
        (limit-clause (if limit
                          (format nil "LIMIT ~D " limit)
                          "")))
    (values
     (format nil
             "SELECT a.*
    FROM (SELECT ROW_NUMBER() OVER (PARTITION BY item ORDER BY revision DESC, origin DESC) rank, *
          FROM `list_data` WHERE optype='item') a
    WHERE a.rank = 1 order by a.revision ~A ~A"
             limit-clause offset-clause)
     nil)))

;;; (sql-get-latest-items)


;;; ---------------------------------------------------------------------
;;; sql-count-latest-items
;;; ---------------------------------------------------------------------

(defun sql-count-latest-items ()
  (values
   (format nil
           "SELECT COUNT(*)
    FROM (SELECT ROW_NUMBER() OVER (PARTITION BY item ORDER BY revision DESC, origin DESC) rank, *
          FROM `list_data` WHERE optype='item') a
    WHERE a.rank = 1 order by a.revision")
   nil))


;;; ---------------------------------------------------------------------
;;; sql-get-latest-userdata

(defun sql-get-latest-userdata (&key (column-ids nil)(like nil)(offset 0)(limit nil))
  (let* ((column-selector (if (null column-ids)
                              "a.*"
                              (join-strings ", "
                                            (mapcar (lambda (cid) (format nil "a.~A" cid))
                                                    column-ids))))
         (like-clauses (if (null like)
                           nil
                           (concatenate 'string "( "
                                        (join-strings " OR "
                                                      (mapcar (lambda (cid)
                                                                (format nil "`~A` LIKE '%~A%'"
                                                                        cid like))
                                                              column-ids))
                                        " ) ")))
         (where-clause (if (null like-clauses)
                           " WHERE `optype` = 'item' "
                           (format nil " WHERE `optype` = 'item' AND ~A" like-clauses)))
         (offset-clause (if (and limit offset)
                            (format nil "OFFSET ~D " offset)
                            ""))
         (limit-clause (if limit
                           (format nil "LIMIT ~D " limit)
                           "")))
    (values
     (format nil
             "SELECT ~A
    FROM (SELECT ROW_NUMBER() OVER (PARTITION BY item ORDER BY revision DESC, origin DESC) rank, *
          FROM `list_data` ~A) a
    WHERE a.rank = 1 order by a.revision ~A ~A"
             column-selector where-clause limit-clause offset-clause)
     nil)))

;;; (sql-get-latest-items)


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
