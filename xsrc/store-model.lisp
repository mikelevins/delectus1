;;;; ***********************************************************************
;;;;
;;;; Name:          store-model.lisp
;;;; Project:       delectus 2
;;;; Purpose:       operations on Delectus model objects stored in SQLite files
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; =====================================================================
;;;
;;; ABOUT
;;;
;;; =====================================================================
;;;
;;; naming conventions:
;;;
;;; - db-foo:
;;;   A function whose name starts with "db-" operates on a SQLite
;;;   database handle. It must be called within a WITH-TRANSACTION
;;;   form that is, in turn, within a WITH-OPEN-DATABASE form.
;;;
;;; - foo:
;;;   A generic function that operates on a database, and whose name
;;;   does not start with "db-", instead takes as its first argument a
;;;   pathname or string that identifies a SQLite database. The method
;;;   specialized on STRING converts the string to a pathname and
;;;   called the method that is specialized on PATHNAME; the method
;;;   specialized on PATHNAME calls the "db-foo" function within
;;;   a WITH-TRANSACTION, within a WITH-OPEN-DATABASE.

;;; ---------------------------------------------------------------------
;;; helpers
;;; ---------------------------------------------------------------------

(defun make-default-userdata-column (&optional column-id)
  (let ((default-column-identity (or column-id (makeid))))
    {:|id| default-column-identity
      :|name| "Item"
      :|type| "TEXT"
      :|order| *default-initial-column-order*
      :|title| :false
      :|subtitle| :false
      :|sort| :false
      :|deleted| :false}))

;;; (setf $coldata (make-default-userdata-column))
;;; (to-json $coldata)

(defmethod db-get-next-opid ((db sqlite-handle))
  (bind ((max-opid-sql max-opid-vals (sqlgen-get-max-opid))
         (max-opid (apply 'execute-single db max-opid-sql max-opid-vals)))
    (if max-opid
        (1+ max-opid)
        0)))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2")(db-get-next-opid db))

(defmethod db-get-next-item ((db sqlite-handle))
  (bind ((max-item-sql max-item-vals (sqlgen-get-max-item))
         (max-item (apply 'execute-single db max-item-sql max-item-vals)))
    (if max-item
        (1+ max-item)
        0)))

(defmethod db-add-userdata-column ((db sqlite-handle) (column-id string))
  (bind ((sql vals (sqlgen-add-userdata-column column-id)))
    (apply 'execute-non-query db sql vals)))

(defmethod db-ensure-columns-exist ((db sqlite-handle) supplied-column-ids)
  ;; identify missing columns in the file or the columns-data argument
  (let* ((found-columns-info (db-sqlite-table-column-info db *listdata-table-name*))
         (found-column-ids (mapcar 'column-info-name found-columns-info))
         (missing-from-found (remove-list-elements found-column-ids supplied-column-ids :test 'equal)))
    ;; if there are columns in the columns-data that are missing from
    ;; the file, create them
    (when missing-from-found
      (loop for colid in missing-from-found
         do (db-add-userdata-column db colid)))))

;;; =====================================================================
;;;
;;; list files
;;;
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; creating the standard tables
;;; ---------------------------------------------------------------------

;;; delectus table
;;; --------------

(defmethod db-initialize-delectus-table ((db sqlite-handle)
                                         &key
                                           (list-id nil)
                                           (file-id nil)
                                           (parent-id nil)
                                           (origin *origin*)
                                           (format-version +delectus-format-version+))
  (bind ((list-id (or list-id (makeid)))
         (file-id (or file-id (makeid)))
         (sql vals (sqlgen-init-delectus-table list-id file-id origin parent-id format-version)))
    (apply 'execute-non-query db sql vals)))

(defmethod db-create-delectus-table ((db sqlite-handle)
                                     &key
                                       (list-id nil)
                                       (file-id nil)
                                       (origin *origin*)
                                       (parent-id nil)
                                       (format-version +delectus-format-version+))
  (let* ((list-id (or list-id (makeid)))
         (file-id (or file-id (makeid))))
    ;; create the delectus table
    (bind ((sql vals (sqlgen-create-delectus-table)))
      (apply 'execute-non-query db sql vals))
    ;; populate it
    (db-initialize-delectus-table db :list-id list-id :file-id file-id :parent-id parent-id
                                  :origin origin :format-version format-version)))


;;; listdata table
;;; --------------

(defmethod db-create-listdata-table ((db sqlite-handle))
  (bind ((sql vals (sqlgen-create-listdata-table)))
    (apply 'execute-non-query db sql vals)))


;;; default listdata ops
;;; --------------------

(defmethod db-insert-default-listdata-ops ((db sqlite-handle)
                                           &key
                                             (list-name nil)
                                             (origin *origin*))
  (assert (stringp list-name) ()
          "You must supply a string :LIST-NAME parameter; found ~S"
          list-name)
  ;; insert the default listdata op
  (bind ((opid (db-get-next-opid db))
         (timestamp (now-timestamp))
         (listname-sql listname-vals (sqlgen-insert-listname-op list-name opid origin timestamp)))
    (apply 'execute-non-query db listname-sql listname-vals))
  (let ((default-column-id (makeid)))
    ;; insert the default columns op
    (bind ((default-column-json (to-json (make-default-userdata-column default-column-id)))
           (opid (db-get-next-opid db))
           (timestamp (now-timestamp))
           (columns-sql columns-vals (sqlgen-insert-columns-op opid origin (now-timestamp)
                                                               {default-column-id default-column-json})))
      (db-ensure-columns-exist db (list default-column-id))
      (apply 'execute-non-query db columns-sql columns-vals))
    ;; insert the default item op
    (bind ((opid (db-get-next-opid db))
           (item (db-get-next-item db))
           (timestamp (now-timestamp))
           (deleted? nil)
           (default-value nil)
           (item-sql item-vals (sqlgen-insert-item-op opid origin timestamp item deleted?
                                                      {default-column-id default-value})))
      (apply 'execute-non-query db item-sql item-vals)
      )))

;;; ---------------------------------------------------------------------
;;; creating the list file
;;; ---------------------------------------------------------------------

(defmethod create-delectus-file ((db-path pathname)
                                 &key
                                   (list-name nil)
                                   (list-id nil)
                                   (file-id nil)
                                   (parent-id nil)
                                   (local-origin *origin*)
                                   (format-version +delectus-format-version+)
                                   (create-default-userdata t))
  (assert (not (probe-file db-path)) () "file exists: ~S" db-path)
  (let ((list-id (or list-id (makeid)))
        (file-id (or file-id (makeid))))
    (with-open-database (db db-path)
      (with-transaction db
        (db-create-delectus-table db :list-id list-id :file-id file-id :parent-id parent-id
                                  :origin local-origin :format-version format-version)
        (db-create-listdata-table db)
        (when create-default-userdata
         (db-insert-default-listdata-ops db :list-name list-name))
        ;;(db-create-item-opid-origin-index db)
        )))
  db-path)

(defmethod create-delectus-file ((db-path string)
                                 &key
                                   (list-name nil)
                                   (list-id nil)
                                   (file-id nil)
                                   (parent-id nil)
                                   (local-origin *origin*)
                                   (format-version +delectus-format-version+)
                                   (create-default-userdata t))
  (create-delectus-file (pathname db-path) :list-name list-name :list-id list-id :file-id file-id
                        :parent-id parent-id :local-origin local-origin :format-version format-version
                        :create-default-userdata create-default-userdata)
  db-path)

;;; (create-delectus-file "/Users/mikel/Desktop/testlist.delectus2" :list-name "Test List")
;;; (delete-file "/Users/mikel/Desktop/testlist.delectus2")
