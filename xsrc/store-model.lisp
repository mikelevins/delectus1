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

(defun make-default-columns-data ()
  (let ((default-column-identity (makeid)))
    {(as-keyword default-column-identity)
     {:|id| default-column-identity
       :|name| "Item"
       :|type| "TEXT"
       :|order| *default-initial-column-order*
       :|title| :false
       :|subtitle| :false
       :|sort| :false
       :|deleted| :false}}))

;;; (make-default-columns-data)


(defmethod db-get-next-opid ((db sqlite-handle))
  (bind ((max-opid-sql max-opid-vals (sqlgen-get-max-opid))
         (max-opid (apply 'execute-single db max-opid-sql max-opid-vals)))
    (if max-opid
        (1+ max-opid)
        0)))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2")(db-get-next-opid db))

(defmethod db-ensure-columns-exist ((db sqlite-handle) (columns-data wb-map))
  )

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
                                             (opid nil)
                                             (origin *origin*)
                                             (timestamp nil))
  (assert (stringp list-name) ()
          "You must supply a string :LIST-NAME parameter; found ~S"
          list-name)
  ;; insert the default listdata op
  (bind ((opid (db-get-next-opid db))
         (timestamp (now-timestamp))
         (listname-sql listname-vals (sqlgen-insert-listname-op list-name opid origin timestamp)))
    (apply 'execute-non-query db listname-sql listname-vals))
  ;; insert the default columns op
  ;; (bind ((coldata (make-default-columns-data))
  ;;        (timestamp (now-timestamp))
  ;;        (columns-sql columns-vals (sqlgen-insert-columns-op opid origin timestamp coldata)))
  ;;   (db-ensure-columns-exist db coldata)
  ;;   (apply 'execute-non-query db columns-sql columns-vals))
  ;; insert the default item op
  )

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
