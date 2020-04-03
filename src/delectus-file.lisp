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

(defmethod create-delectus-file ((list-name string)(path pathname))
  (when (probe-file path)
    (error "file exists: ~S" path))
  (let* ((listid (makeid))
         (origin *origin*)
         (format +delectus-format-version+)
         (default-column-id (makeid))
         (default-column-data (op::columndata :id default-column-id :name "Item"
                                              :type 'text :order 10.0 :sort "ASC"
                                              :title t :subtitle nil :deleted nil))
         (listname-op (op::listname :opid (makeid)
                                    :origin *origin*
                                    :name list-name
                                    :revision 0
                                    :timestamp (now-timestamp)))
         (column-op (op::columns :opid (makeid)
                                 :origin *origin*
                                 :revision 1
                                 :timestamp (now-timestamp)
                                 :columns (list default-column-data)))
         (create-delectus-table-statement
          (create-table :delectus
              ((id :type 'text)
               (origin :type 'text)
               (format :type 'text)
               (next_revision :type 'integer))))
         (create-delectus-table-sql (yield create-delectus-table-statement))
         (insert-delectus-info-statement
          (insert-into :delectus
            (:id :origin :format :next_revision)
            `(,listid ,*origin* ,format 2)))
         (insert-delectus-info-sql (yield insert-delectus-info-statement))
         (create-list-table-statement
          (create-table :list_data
              ((optype :type 'text)
               (opid :type 'text)
               (origin :type 'text)
               (revision :type 'integer)
               (timestamp :type 'text)
               (item :type 'text)
               (name :type 'text)
               (deleted :type 'integer)
               (peer :type 'text))))
         (create-list-table-sql (yield create-list-table-statement))
         (insert-listname-statement
          (insert-into :delectus
            (set= :optype "listname" :opid (makeid) :origin *origin* :revision 0
                       :timestamp (now-timestamp) :item nil :name list-name
                       :deleted nil :peer nil))))
    (with-open-database (db path)
      (execute-non-query db create-delectus-table-sql)
      (execute-non-query db create-list-table-sql)
      (execute-non-query db insert-delectus-info-sql))
    
    insert-delectus-info-sql))

(defmethod create-delectus-file ((list-name string)(path pathname))
  (when (probe-file path)
    (error "file exists: ~S" path))
  (let* ((listid (makeid))
         (create-delectus-table-statement
          (create-table :delectus
              ((id :type 'text)
               (origin :type 'text)
               (format :type 'text)
               (next_revision :type 'integer))))
         (populate-delectus-table-statement
          (insert-into :delectus
            (set= :id listid
                  :origin *origin*
                  :format +delectus-format-version+
                  ;; 0 is the listname op
                  ;; 1 is the first columns op
                  ;; 2 is the first item op
                  :next_revision 3)))
         (create-list-table-statement
          (create-table :list_data
              ((optype :type 'text)
               (opid :type 'text)
               (origin :type 'text)
               (revision :type 'integer)
               (timestamp :type 'text)
               (item :type 'text)
               (name :type 'text)
               (deleted :type 'text)
               (peer :type 'text))))
         (assert-listname-statement
          (insert-into :list_data
            (set= :optype "listname"
                  :opid (makeid)
                  :origin *origin*
                  :revision 0
                  :timestamp (now-timestamp)
                  :item nil
                  :name list-name
                  :deleted nil
                  :peer nil)))
         (first-item-id (makeid))
         (default-column-id (makeid))
         (default-column-attributes
          (jonathan:to-json
           (op::columndata :id default-column-id
                           :name "Item"
                           :type "TEXT"
                           :order op::*minimum-column-order*
                           :sort "ASC"
                           :title t
                           :subtitle nil
                           :deleted nil))))

    (with-open-database (db path)
      ;; create the delectus table
      (multiple-value-bind (sql vals)(yield create-delectus-table-statement)
        (apply 'execute-non-query db sql vals))
      ;; assert values in the delectus table
      (multiple-value-bind (sql vals)(yield populate-delectus-table-statement)
        (apply 'execute-non-query db sql vals))
      ;; create the list_data table
      (multiple-value-bind (sql vals)(yield create-list-table-statement)
        (apply 'execute-non-query db sql vals))
      ;; 0. initial listname
      (multiple-value-bind (sql vals)(yield assert-listname-statement)
        (apply 'execute-non-query db sql vals))
      ;; create the initial userdata column
      (execute-non-query db (format nil "ALTER TABLE list_data ADD ~A TEXT" default-column-id))
      ;; 1. initial columns op
      (let ((sql (concatenate
                  'string
                  (format nil "INSERT INTO list_data ")
                  (format nil "(optype, opid, origin, revision, timestamp, item, name, deleted, peer, ~A) "
                          default-column-id)
                  (format nil "VALUES (?, ?, ?, ?, ?, NULL, NULL, NULL, NULL, ?)"))))
        (execute-non-query db sql "columns" (makeid) *origin* 1 (now-timestamp) default-column-attributes))
      ;; 2. initial item op
      (let ((sql (concatenate
                  'string
                  (format nil "INSERT INTO list_data ")
                  (format nil "(optype, opid, origin, revision, timestamp, item, name, deleted, peer, ~A) "
                          default-column-id)
                  (format nil "VALUES (?, ?, ?, ?, ?, ?, NULL, NULL, NULL, NULL)"))))
        (execute-non-query db sql "item" (makeid) *origin* 2 (now-timestamp) (makeid))))))

(defmethod create-delectus-file ((list-name string)(path string))
  (create-delectus-file list-name (pathname path)))

;;; (create-delectus-file "Test List" "/Users/mikel/Desktop/testlist.delectus2")
