;;;; ***********************************************************************
;;;;
;;;; Name:          store-sqlgen.lisp
;;;; Project:       delectus 2
;;;; Purpose:       generating SQL for Delectus store operations
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :sqlgen)
(in-readtable :interpol-syntax)

;;; =====================================================================
;;; SQL constructors
;;; =====================================================================
;;; each sqlgen::constructor function returns two values: a SQL string, and
;;; a list of values to be bound to any '?' placeholders in the SQL
;;; string. If there are no placeholder variables in the SQL, then
;;; the second returned value is NIL.
;;;
;;; Each function definition is structured like this:
;;; (defun sqlgen::FUNCTION-NAME (arg0 arg1 ... argN)
;;;   (let (BINDING* ...)
;;;     (values
;;;       (SQL #?| ... some SQL code ... |)
;;;       (value-expression* ...))))
;;;
;;; The LET bindings are optional
;;; The SQL expression contains a CL-INTERPOL string
;;; that gives literal SQL code, optionally with
;;; interpolation expressions referring to variables
;;; from the enclosing environment.
;;; ---------------------------------------------------------------------
;;; sqlgen::create-delectus-table
;;; ---------------------------------------------------------------------

(defun sqlgen::create-delectus-table ()
  (values
   (SQL #?|

CREATE TABLE `delectus` ( `id` TEXT, `origin` TEXT, `format` TEXT, `next_revision` INTEGER )

|)
   nil))


;;; (sqlgen::create-delectus-table)


;;; ---------------------------------------------------------------------
;;; sqlgen::populate-delectus-table
;;; ---------------------------------------------------------------------

(defun sqlgen::populate-delectus-table (id origin format next-revision)
  (values
   (SQL #?|

INSERT INTO `delectus` (`id`, `origin`, `format`, `next_revision`) VALUES (?, ?, ?, ?)

|)
   (list id origin format next-revision)))

;;; (sqlgen::populate-delectus-table (delectus::makeid) delectus::*origin* delectus::+delectus-format-version+ 3)


;;; ---------------------------------------------------------------------
;;; sqlgen::create-listdata-table
;;; ---------------------------------------------------------------------

(defun sqlgen::create-listdata-table ()
  (values
   (SQL #?|

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

;;; (sqlgen::create-listdata-table)


;;; ---------------------------------------------------------------------
;;; sqlgen::create-item-revision-origin-index
;;; ---------------------------------------------------------------------

(defun sqlgen::create-item-revision-origin-index ()
  (values
   (SQL #?|

CREATE INDEX `idx_item_revision_origin` ON `list_data` (`item`, `revision`, `origin`)

|)
   nil))

;;; (sqlgen::create-item-revision-origin-index)


;;; ---------------------------------------------------------------------
;;; sqlgen::add-userdata-column
;;; ---------------------------------------------------------------------

(defun sqlgen::add-userdata-column (label type)
  (values
   (SQL #?|

ALTER TABLE `list_data` ADD `${label}` ${type}

|)
   nil))

;;; (sqlgen::add-userdata-column (makeid) "TEXT")


;;; ---------------------------------------------------------------------
;;; sqlgen::get-column-info
;;; ---------------------------------------------------------------------

(defun sqlgen::get-column-info ()
  (values "PRAGMA table_info(list_data);"
          nil))

;;; (sqlgen::get-column-info)


;;; ---------------------------------------------------------------------
;;; sqlgen::list-id
;;; ---------------------------------------------------------------------

(defun sqlgen::list-id ()
  (values
   (SQL #?|

SELECT `id` FROM `delectus` LIMIT 1

|)
   nil))

;;; (sqlgen::list-id)


;;; ---------------------------------------------------------------------
;;; SQLGEN::list-origin
;;; ---------------------------------------------------------------------

(defun sqlgen::list-origin ()
  (values
   (SQL #?|

SELECT `origin` FROM `delectus` LIMIT 1

|)
   nil))

;;; (sqlgen::list-origin)


;;; ---------------------------------------------------------------------
;;; sqlgen::list-format
;;; ---------------------------------------------------------------------

(defun sqlgen::list-format ()
  (values
   (SQL #?|

SELECT `format` FROM `delectus` LIMIT 1

|)
   nil))

;;; (sqlgen::list-format)


;;; ---------------------------------------------------------------------
;;; sqlgen::increment-next-revision
;;; ---------------------------------------------------------------------

(defun sqlgen::increment-next-revision ()
  (values
   (SQL #?|

UPDATE `delectus` SET `next_revision` = `next_revision` + 1

|)
   nil))

;;; (sqlgen::increment-next-revision)

;;; ---------------------------------------------------------------------
;;; sqlgen::next-revision
;;; ---------------------------------------------------------------------

(defun sqlgen::next-revision ()
  (values
   (SQL #?|

SELECT `next_revision` FROM `delectus` LIMIT 1

|)
   nil))

;;; (sqlgen::next-revision)

;;; ---------------------------------------------------------------------
;;; sqlgen::get-latest-listname
;;; ---------------------------------------------------------------------

(defun sqlgen::get-latest-listname ()
  (values
   (SQL #?|

SELECT * FROM `list_data` 
WHERE `optype`='listname' 
ORDER BY `revision` DESC, `origin` DESC 
LIMIT 1

|)
   nil))

;;; (sqlgen::get-latest-listname)


;;; ---------------------------------------------------------------------
;;; sqlgen::get-latest-columns
;;; ---------------------------------------------------------------------

(defun sqlgen::get-latest-columns ()
  (values
   (SQL #?|

SELECT * FROM `list_data` 
WHERE `optype`='columns' 
ORDER BY `revision` DESC, `origin` DESC 
LIMIT 1

|)
   nil))


;;; ---------------------------------------------------------------------
;;; sqlgen::get-latest-items
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

(defun sqlgen::get-latest-items (&key (offset 0)(limit nil))
  (let ((offset-clause (if (and limit offset)
                           (format nil "OFFSET ~D " offset)
                           ""))
        (limit-clause (if limit
                          (format nil "LIMIT ~D " limit)
                          "")))
    (values
     (SQL #?|

SELECT a.* 
FROM (SELECT ROW_NUMBER() 
      OVER (PARTITION BY item ORDER BY revision DESC, origin DESC) rank, * 
      FROM `list_data` 
      WHERE optype='item') a 
WHERE a.rank = 1 order by a.revision ${limit-clause} ${offset-clause}

|)
     nil)))

;;; (sqlgen::get-latest-items)
;;; (sqlgen::get-latest-items :offset 100 :limit 10)


;;; ---------------------------------------------------------------------
;;; sqlgen::count-latest-items
;;; ---------------------------------------------------------------------

(defun sqlgen::count-latest-items ()
  (values
   (SQL #?|

SELECT COUNT(*)
FROM (SELECT ROW_NUMBER() 
      OVER (PARTITION BY item ORDER BY revision DESC, origin DESC) rank, *
      FROM `list_data` 
      WHERE optype='item') a
WHERE a.rank = 1 order by a.revision

|)
   nil))


;;; ---------------------------------------------------------------------
;;; sqlgen::get-latest-userdata

(defun sqlgen::get-latest-userdata (&key (column-ids nil)(like nil)(offset 0)(limit nil))
  (let* ((column-selector
          (if (null column-ids)
              "a.*"
              (delectus::join-strings ", "
                                      (mapcar (lambda (cid) (format nil "a.~A" cid))
                                              column-ids))))
         (like-clauses
          (if (null like)
              nil
              (concatenate 'string "( "
                           (delectus::join-strings " OR "
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
     (SQL #?|

SELECT ${column-selector}
FROM (SELECT ROW_NUMBER() 
      OVER (PARTITION BY item ORDER BY revision DESC, origin DESC) rank, *
      FROM `list_data` ${where-clause}) a
WHERE a.rank = 1 order by a.revision ${limit-clause} ${offset-clause}

|)
     nil)))

;;; (sqlgen::get-latest-userdata)
;;; (sqlgen::get-latest-userdata :column-ids '("1" "2" "3"))

;;; ---------------------------------------------------------------------
;;; sqlgen::count-latest-userdata
;;; ---------------------------------------------------------------------

(defun sqlgen::count-latest-userdata (&key (column-ids nil)(like nil))
  (let* ((like-clauses
          (if (null like)
              ""
              (let ((like-columns-string
                     (delectus::join-strings " OR "
                                             (mapcar
                                              (lambda (cid) (format nil "`~A` LIKE '%~A%'" cid like))
                                              column-ids))))
                (concatenate 'string " AND ( " like-columns-string " ) ")))))
    (values
     (SQL #?|

SELECT COUNT(*)
FROM (SELECT ROW_NUMBER() 
      OVER (PARTITION BY item ORDER BY revision DESC, origin DESC) rank, *
      FROM `list_data` 
      WHERE optype='item' ${like-clauses}) a
WHERE a.rank = 1 order by a.revision

|)
     nil)))

;;; (sqlgen::count-latest-userdata)
;;; (sqlgen::count-latest-userdata :column-ids (list (delectus::makeid)) :like "Foo")

;;; ---------------------------------------------------------------------
;;; sqlgen::get-latest-sync
;;; ---------------------------------------------------------------------

(defun sqlgen::get-latest-sync ()
  (values
   (SQL #?|

SELECT * FROM `list_data` 
WHERE `optype`='sync' 
ORDER BY `revision` DESC, `origin` DESC 
LIMIT 1

|)
   nil))

;;; ---------------------------------------------------------------------
;;; sqlgen::assert-listname
;;; ---------------------------------------------------------------------

(defun sqlgen::assert-listname (opid origin revision timestamp item name deleted peer)
  (delectus::bind ((optype "listname")
                   (params vals (make-metadata-params optype opid origin revision timestamp item name deleted peer))
                   (placeholders (mapcar (constantly "?") params))
                   (params-string (delectus::join-strings ", " params))
                   (placeholders-string (delectus::join-strings ", " placeholders)))
    (values
     (SQL #?|

INSERT INTO `list_data` (${params-string}) VALUES (${placeholders-string})

|)
     vals)))

;;; (sqlgen::assert-listname (makeid)(makeid) 3 (now-timestamp) nil "A List" nil nil)


;;; ---------------------------------------------------------------------
;;; sqlgen::assert-columns
;;; ---------------------------------------------------------------------

(defun sqlgen::assert-columns (opid origin revision timestamp item name deleted peer &key column-data)
  (delectus::bind ((optype "columns")
                   (meta-params meta-vals (make-metadata-params optype opid origin revision timestamp item name deleted peer))
                   (column-params column-vals (make-column-params column-data))
                   (params (append meta-params column-params))
                   (vals (append meta-vals column-vals))
                   (placeholders (mapcar (constantly "?") params))
                   (params-string (delectus::join-strings ", " params))
                   (placeholders-string (delectus::join-strings ", " placeholders)))
    (values
     (SQL #?|

INSERT INTO `list_data` (${params-string}) VALUES (${placeholders-string})

|)
     vals)))

;;; ---------------------------------------------------------------------
;;; sqlgen::assert-item
;;; ---------------------------------------------------------------------

(defun sqlgen::assert-item (opid origin revision timestamp item name deleted peer &key column-data column-values)
  (delectus::bind ((optype "item")
                   (meta-params meta-vals (make-metadata-params optype opid origin revision timestamp item name deleted peer))
                   (item-params item-vals (make-item-params column-data column-values))
                   (params (append meta-params item-params))
                   (vals (append meta-vals item-vals))
                   (placeholders (mapcar (constantly "?") params))
                   (params-string (delectus::join-strings ", " params))
                   (placeholders-string (delectus::join-strings ", " placeholders)))
    (values
     (SQL #?|

INSERT INTO `list_data` (${params-string}) VALUES (${placeholders-string})

|)
     vals)))

;;; ---------------------------------------------------------------------
;;; sqlgen::assert-sync
;;; ---------------------------------------------------------------------

(defun sqlgen::assert-sync (opid origin revision timestamp item name deleted peer)
  (delectus::bind ((optype "sync")
                   (params vals (make-metadata-params optype opid origin revision timestamp item name deleted peer))
                   (placeholders (mapcar (constantly "?") params))
                   (params-string (delectus::join-strings ", " params))
                   (placeholders-string (delectus::join-strings ", " placeholders)))
    (values
     (SQL #?|

INSERT INTO `list_data` (${params-string}) VALUES (${placeholders-string})

|)
     vals)))

;;; (sqlgen::assert-sync (makeid)(makeid) 3 (now-timestamp) nil nil nil (makeid))
