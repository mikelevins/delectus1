;;;; ***********************************************************************
;;;;
;;;; Name:          delectus-file.lisp
;;;; Project:       delectus 2
;;;; Purpose:       model-specific operations on delectus sqlite files
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; This file provides functions for working with Delectus model
;;; objects (lists, columns, items, and so on) that are stored in
;;; SQLite files.

;;; TODO: use CREATE INDEX to make the common queries go fast
;;; TODO: use CREATE VIEW to simplify the most common queries
;;;       (get the latest listname; get the latest columns; get the
;;;       latest version of each distinct item)
;;; TODO: check ops before they are asserted; if an op is identical
;;;       to the latest op of its type, don't assert it

;;; ---------------------------------------------------------------------
;;; utility functions
;;; ---------------------------------------------------------------------

(defun next-revision (db-path)
  (with-open-database (db db-path)
    (with-transaction db
      (bind ((sqlupdate ignore-vals (sql-increment-next-revision))
             (sqlget ignore-vals (sql-next-revision)))
        (apply 'execute-non-query db sqlupdate ignore-vals)
        (apply 'execute-single db sqlget ignore-vals)))))

;;; (next-revision "/Users/mikel/Desktop/testlist.delectus2")

;;; requires an open db reference
(defun add-userdata-column (db column-id)
  (bind ((sql vals (sql-add-userdata-column column-id "TEXT")))
    (apply 'execute-non-query db sql vals)))

;;; ---------------------------------------------------------------------
;;; create-delectus-file
;;; ---------------------------------------------------------------------
;;; creates a list file and sets up the SQLite tables for its data

(defmethod create-delectus-file ((list-name string)(path pathname))
  (when (probe-file path)
    (error "file exists: ~S" path))
  (let* ((listid (makeid))
         (listname-rev 0)
         (columns-rev 1)
         (item-rev 2)
         (next-rev 3)
         (listname-opid (makeid))
         (columns-opid (makeid))
         (item-opid (makeid))
         (initial-column-id (makeid))
         (initial-column-name "Item")
         (initial-column-order op::*minimum-column-order*)
         (initial-column-sort "ASC")
         (initial-column-attributes (jonathan:to-json
                                     (op::columndata :id initial-column-id
                                                     :name initial-column-name
                                                     :type "TEXT"
                                                     :order initial-column-order
                                                     :sort initial-column-sort
                                                     :title t
                                                     :subtitle nil
                                                     :deleted nil)))
         (initial-item-id (makeid))
         (initial-field-value nil))

    (with-open-database (db path)
      ;; create the delectus table
      (bind ((sql vals (sql-create-delectus-table)))
        (apply 'execute-non-query db sql vals))
      ;; assert values into it
      (bind ((sql vals (sql-populate-delectus-table listid *origin* +delectus-format-version+ next-rev)))
        (apply 'execute-non-query db sql vals))

      ;; create the list_data table
      (bind ((sql vals (sql-create-list_data-table)))
        (apply 'execute-non-query db sql vals))

      ;; 0. initial listname op
      (bind ((sql vals
                  (sql-assert-op "listname" listname-opid *origin* listname-rev
                                 (now-timestamp) nil list-name nil nil)))
        (apply 'execute-non-query db sql vals))
      ;; create the initial userdata column
      (add-userdata-column db initial-column-id)

      ;; 1. initial columns op
      (bind ((sql vals
                  (sql-assert-op "columns" columns-opid *origin* columns-rev (now-timestamp)
                                 nil nil nil nil
                                 :column-data `((,initial-column-id . ,initial-column-attributes)))))
        (apply 'execute-non-query db sql vals))

      ;; 2. initial item op
      (bind ((sql vals
                  (sql-assert-op "item"
                                 item-opid *origin* item-rev (now-timestamp) initial-item-id nil nil nil
                                 :column-data `((,initial-column-id . ,initial-field-value)))))
        (apply 'execute-non-query db sql vals)))))


(defmethod create-delectus-file ((list-name string)(path string))
  (create-delectus-file list-name (pathname path)))

;;; (create-delectus-file "Test List" "/Users/mikel/Desktop/testlist.delectus2")

;;; ---------------------------------------------------------------------
;;; getting ops
;;; ---------------------------------------------------------------------
;;; functions for retrieving op records.

(defun get-latest-listname-op (db-path)
  (bind ((sql vals (sql-get-latest-listname-op)))
    (with-open-database (db db-path)
      (first (execute-to-list db sql)))))

;;; (get-latest-listname-op "/Users/mikel/Desktop/testlist.delectus2")

(defun get-latest-columns-op (db-path)
  (bind ((sql vals (sql-get-latest-columns-op)))
    (with-open-database (db db-path)
      (first (execute-to-list db sql)))))

;;; (get-latest-columns-op "/Users/mikel/Desktop/testlist.delectus2")

;;; NOTE: returns a list of (1 . row), because it's partitioning by
;;; id, then sorting descending by revision, then returning the rows
;;; whose rank is 1; so the CDR of each item is the actual result
(defun get-latest-item-ops (db-path)
  (bind ((sql vals (sql-get-latest-item-ops)))
    (let ((latest-item-results (with-open-database (db db-path)
                                 (execute-to-list db sql))))
      ;; discard the rank field from the returned result
      (mapcar #'cdr latest-item-results))))

;;; (get-latest-item-ops "/Users/mikel/Desktop/testlist.delectus2")

(defun get-latest-sync-op (db-path)
  (bind ((sql vals (sql-get-latest-sync-op)))
    (with-open-database (db db-path)
      (first (execute-to-list db sql)))))

;;; (get-latest-sync-op "/Users/mikel/Desktop/testlist.delectus2")


;;; ---------------------------------------------------------------------
;;; getting columns
;;; ---------------------------------------------------------------------
;;; "columns" asserts column attributes. "item" asserts column values.
;;; even when these ops do not change anything, they must contain the
;;; unchanged column values, because they become the authoritative
;;; source of what those values are. we therefore need a function to
;;; retrieve the current column info for columns, and one to retrieve
;;; the current values for item.

(defparameter +delectus-metadata-column-names+
    '("optype" "opid" "origin" "revision" "timestamp" "item" "name" "deleted" "peer"))

;;; make a list defstruct for accessors to use on column-info lists
(defstruct (column-info (:type list))
  cid name type notnull dflt_value pk)

(defun get-sqlite-column-info (db-path)
  (with-open-database (db db-path)
    (bind ((sql vals (sql-get-column-info)))
                     (apply 'execute-to-list db sql vals))))

;;; (defparameter $colinfo (get-sqlite-column-info "/Users/mikel/Desktop/testlist.delectus2"))
;;; (column-info-name (first $colinfo))
;;; (mapcar 'column-info-name $colinfo)

;;; NOTE: not guaranteed to preserve the order of columns
(defun get-userdata-column-labels (db-path)
  (let* ((column-info (get-sqlite-column-info db-path))
         (column-names (mapcar #'column-info-name column-info)))
    (set-difference column-names +delectus-metadata-column-names+
                    :test #'equal)))

;;; (get-userdata-column-labels "/Users/mikel/Desktop/testlist.delectus2")

;;; returns an list of <column-attributes>
;;; <column-attributes> is an FSET:WB-MAP parsed from the JSON string
;;; stored on the column. the :|id| field of each attributes object is
;;; also the column's SQLite label
(defun get-userdata-column-attributes (db-path)
  (let* ((latest-columns-op (get-latest-columns-op db-path))
         (json-strings (op::op-field latest-columns-op :columns)))
    (mapcar #'from-json json-strings)))

;;; (get-userdata-column-attributes "/Users/mikel/Desktop/testlist.delectus2")

;;; ---------------------------------------------------------------------
;;; asserting ops
;;; ---------------------------------------------------------------------

(defun assert-listname (db-path &key opid (origin *origin*) revision timestamp name)
  (assert (stringp name)() "The :NAME parameter must be a text string")
  (let ((optype "listname")
        (opid (or opid (makeid)))
        (revision (or revision (next-revision db-path)))
        (timestamp (or timestamp (now-timestamp))))
    (with-open-database (db db-path)
      (bind ((sql vals
                  (sql-assert-op optype opid origin
                                 revision
                                 (now-timestamp)
                                 nil name nil nil)))
        (apply 'execute-non-query db sql vals)))))

;;; (time (assert-listname "/Users/mikel/Desktop/testlist.delectus2" :name "Sample List"))
;;; (get-latest-listname-op "/Users/mikel/Desktop/testlist.delectus2")

(defun add-missing-column (db-path column-attributes)
  )

(defun merge-column-attributes (existing-column-attributes new-column-attributes)
  )

(defun assert-columns (db-path &key opid origin revision timestamp columns)
  (let* ((optype "columns")
         (existing-columns (get-userdata-column-attributes db-path))
         (columns-to-add (set-difference columns existing-columns
                                         :key (lambda (c)(fset:@ c :|id|))
                                         :test #'equal)))
    (with-open-database (db db-path)
      ;; add the missing columns
      (loop for col in columns-to-add
         do (let ((col-label (fset:@ col :|id|))
                  (col-json (to-json col)))
              (add-userdata-column db col-label)))
      ;; construct and post the columns op
      ;; we have to ensure that newly-added columns end up with valid attributes
      ;; that means we have to check the data provided in the op and, if it doesn't contain
      ;; attributes JSON, we have to construct a default JSON string, using the new column's ID
      ;; for the name, and assigning an order that doesn't conflict with exiting solumns
      (let* ((columns-opid (makeid))
             (columns-rev (next-revision db-path))
             (columns-data (merge-column-attributes existing-column-attributes columns)))
        (bind ((sql vals
                    (sql-assert-op "columns" columns-opid *origin* columns-rev (now-timestamp)
                                   nil nil nil nil
                                   :column-data column-data)))
          (apply 'execute-non-query db sql vals))))))

(defun assert-item (db-path &key opid origin revision timestamp item deleted columns)
  ;;; 1. check that the columns in the op match the columns in the
  ;;;    file; if not, it's an error (we need a "columns" op to
  ;;;    reconcile them first
  ;;; 2. assert the op!
  (let ((optype "item"))
    ;; TODO: write it!
    ))

(defun assert-sync (db-path &key opid origin revision timestamp peer)
  (assert (stringp peer)() "The :PEER parameter must be a valid Delectus identity")
  (let ((optype "sync")
        (opid (or opid (makeid)))
        (revision (or revision (next-revision db-path)))
        (timestamp (or timestamp (now-timestamp))))
    (with-open-database (db db-path)
      (bind ((sql vals
                  (sql-assert-op optype opid origin
                                 revision
                                 (now-timestamp)
                                 nil nil nil peer)))
        (apply 'execute-non-query db sql vals)))))

;;; (time (assert-sync "/Users/mikel/Desktop/testlist.delectus2" :peer *origin*))
;;; (get-latest-sync-op "/Users/mikel/Desktop/testlist.delectus2")