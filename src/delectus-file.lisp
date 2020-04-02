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
  (let* ((create-delectus-table-statement
          (sxql:create-table :delectus
              ((id :type 'text)
               (origin :type 'text)
               (format :type 'text))))
         (create-delectus-table-sql (sxql:yield create-delectus-table-statement))
         (create-list-table-statement
          (sxql:create-table :list_data
              ((optype :type 'text)
               (opid :type 'text)
               (origin :type 'text)
               (revision :type 'integer)
               (timestamp :type 'text)
               (item :type 'text)
               (name :type 'text)
               (deleted :type 'integer)
               (peer :type 'text)))))
    (with-open-database (db path)
      (execute-to-list db create-delectus-table-sql))
    path))

(defmethod create-delectus-file ((list-name string)(path string))
  (create-delectus-file list-name (pathname path)))

;;; (create-delectus-file "Test List" "/Users/mikel/Desktop/testdb.delectus2")
