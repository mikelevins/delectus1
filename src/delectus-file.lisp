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

;;; TODO: use CREATE INDEX to make the common queries go fast
;;; TODO: use CREATE VIEW to simplify the most common queries
;;;       (get the latest listname; get the latest columns; get the
;;;       latest version of each distinct item)

;;; ---------------------------------------------------------------------
;;; create-delectus-file
;;; ---------------------------------------------------------------------

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
      (bind ((sql vals (sql-add-userdata-column initial-column-id "TEXT")))
        (apply 'execute-non-query db sql vals))

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

(defun get-latest-item-ops (db-path)
  (bind ((sql vals (sql-get-latest-item-ops)))
    (with-open-database (db db-path)
      (execute-to-list db sql))))

;;; (get-latest-item-ops "/Users/mikel/Desktop/testlist.delectus2")


;;; ---------------------------------------------------------------------
;;; getting columns
;;; ---------------------------------------------------------------------
;;; columns asserts column attributes. item asserts column values.
;;; even when these ops do not change anything, they must contain
;;; the unchanged column values, because they become the authoritative
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
    (bind ((sql vals (sql-get-column-attributes)))
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

;;; returns an alist with this format:
;;; (<columnid> . <column-sttributes-plist>)
;;; the attributes are parsed from the JSON data stored in the column
;;; and include the id, so the id appears twice
(defun get-userdata-column-attributes (db-path)
  (let* ((latest-columns-op (get-latest-columns-op db-path))
         (json-strings (op::op-field latest-columns-op :columns))
         (column-attrs (mapcar #'jonathan:parse json-strings)))
    (mapcar (lambda (attrs)
              (cons (getf attrs :|id|)
                    attrs))
            column-attrs)))

;;; (get-userdata-column-attributes "/Users/mikel/Desktop/testlist.delectus2")

;;; ---------------------------------------------------------------------
;;; asserting ops
;;; ---------------------------------------------------------------------

(defun assert-listname (db-path &key opid origin revision timestamp name)
  (let ((optype "listname"))
    ))

(defun assert-columns (db-path &key opid origin revision timestamp columns)
  ;;; 1. get the existing column info
  ;;; 2. compute the new set of columns = old columns + new columns
  ;;; 3. create any columns that don't already exist
  ;;; 4. assert the new columns op
  (let ((optype "columns"))
    ))

(defun assert-item (db-path &key opid origin revision timestamp item deleted columns)
  (let ((optype "item"))
    ))

(defun assert-sync (db-path &key opid origin revision timestamp peer)
  (let ((optype "sync"))
    ))
