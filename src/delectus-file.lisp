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
                  (sql-assert-op "listname" listname-opid *origin*
                                   listname-rev (now-timestamp) nil
                                   list-name nil nil)))
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
;;; assert-op
;;; ---------------------------------------------------------------------

(defmethod assert-op ((path pathname) op)
  (op::ensure-well-formed-op op)
  )

(defmethod assert-op ((path string) op)
  (assert-op (pathname path) op))
