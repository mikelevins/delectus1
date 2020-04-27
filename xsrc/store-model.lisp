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
(in-readtable :delectus)

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
;;;
;;; Ops:
;;;
;;; an op is a database row that specifies one of four operations:
;;;
;;; - listname: updates the user-defined name of the list
;;; - columns: updates the list's userdata columns and their attributes
;;; - item: adds or update an item in the list
;;; - sync: records a successful synchronization with another copy
;;;   of the list

;;; =====================================================================
;;;
;;; list files
;;;
;;; =====================================================================


;;; ---------------------------------------------------------------------
;;; creating standard tables
;;; ---------------------------------------------------------------------

;;; `delectus`
;;; ---------------------------------------------------------------------
;;; the list metadata, including:
;;; - list id
;;; - list origin
;;; - data-format version
;;; - the next revision number
;;; - the next iref (identity index)

(defmethod db-initialize-delectus-table ((db sqlite-handle)
                                         &key
                                           (listid nil)
                                           (origin *origin*)
                                           (format-version +delectus-format-version+))
  ;; - insert the list id
  ;; - insert the list origin
  ;; - insert 0 for the `next_revision` and `next_iref` fields
  )

(defmethod db-create-delectus-table ((db sqlite-handle)
                                     &key
                                       (id nil)
                                       (origin *origin*)
                                       (format-version +delectus-format-version+))
  (let* ((listid (or listid (makeid))))
    ;; create the delectus table
    (bind ((sql vals (sqlgen::create-delectus-table)))
      (apply 'execute-non-query db sql vals))
    ;; populate it
    (db-initialize-delectus-table db :id id :origin origin :format-version format-version)))

;;; `identities`
;;; ---------------------------------------------------------------------
;;; maps irefs to identities

(defmethod db-initialize-identities-table ((db sqlite-handle)
                                           &key
                                             (local-origin *origin*))
  ;; - insert local-origin with the next iref as its key
  ;; - update the next iref
  )

(defmethod db-create-identities-table ((db sqlite-handle)
                                       &key
                                         (local-origin *origin*))
  (bind ((sql vals (sqlgen::create-identities-table)))
    (apply 'execute-non-query db sql vals)
    (db-initialize-identities-table db :local-origin local-origin)))

;;; `listdata`
;;; ---------------------------------------------------------------------
;;; the log of ops defining the contents of the list
(defmethod db-create-listdata-table ((db sqlite-handle))
  (bind ((sql vals (sqlgen::create-listdata-table)))
    (apply 'execute-non-query db sql vals)))


;;; ---------------------------------------------------------------------
;;; `idx_item_revision_origin`
;;; ---------------------------------------------------------------------
;;; the standard index

(defun db-create-item-revision-origin-index (db)
  (bind ((sql vals (sqlgen::create-item-revision-origin-index)))
    (apply 'execute-non-query db sql vals)))

;;; ---------------------------------------------------------------------
;;; the default initial listname, columns, and item ops
;;; ---------------------------------------------------------------------
;;; optional; when importing an existing list, we do not add these

(defmethod db-insert-default-listdata-ops ((db sqlite-handle))
  )

;;; ---------------------------------------------------------------------
;;; creating the list file
;;; ---------------------------------------------------------------------

(defmethod create-delectus-file ((db-path pathname)
                                 &key
                                   (list-name nil)
                                   (list-id nil)
                                   (local-origin *origin*)
                                   (format-version +delectus-format-version+)
                                   (create-default-userdata t))
  (assert (not (probe-file db-path)) () "file exists: ~S" db-path)
  (assert (stringp list-name) () "String argument required for :LIST-NAME. Found ~S" list-name)
  (let ((list-id (or list-id (makeid))))
    (with-open-database (db db-path)
      (with-transaction db
        (db-create-delectus-table db :id list-id :origin origin :format-version format-version)
        (db-create-identities-table db :local-origin local-origin)
        (db-create-listdata-table db)
        (db-create-item-revision-origin-index db)
        (when create-default-userdata
          (db-insert-default-listdata-ops db)))))
  db-path)

(defmethod create-delectus-file ((db-path string)(list-name string)(listid string) &key (create-default-userdata t))
  (create-delectus-file (pathname db-path) list-name listid :create-default-userdata create-default-userdata)
  db-path)

;;; (create-delectus-file "/Users/mikel/Desktop/testlist.delectus2" "Test List" (makeid))
;;; (delete-file "/Users/mikel/Desktop/testlist.delectus2")

;;; ---------------------------------------------------------------------
;;; adding userdata columns
;;; ---------------------------------------------------------------------

(defmethod db-add-userdata-column ((db sqlite-handle) (column-id string))
  (bind ((sql vals (sqlgen::add-userdata-column column-id "TEXT")))
    (apply 'execute-non-query db sql vals)))

(defmethod add-userdata-column ((db-path pathname) (column-id string))
  (with-open-database (db db-path)
    (with-transaction db
      (db-add-userdata-column db column-id))))

(defmethod add-userdata-column ((db-path string) (column-id string))
  (add-userdata-column (pathname db-path) column-id))


