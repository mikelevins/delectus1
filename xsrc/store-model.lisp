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
;;; list-metadata operations
;;; ---------------------------------------------------------------------

(defmethod db-allocate-next-revision ((db sqlite-handle))
  (let ((rev (execute-single db (sqlgen::get-next-revision))))
    (execute-single db (sqlgen::increment-next-revision))
    rev))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-allocate-next-revision db))

(defmethod db-allocate-next-iref ((db sqlite-handle))
  (let ((iref (execute-single db (sqlgen::get-next-iref))))
    (execute-single db (sqlgen::increment-next-iref))
    ;; allocating an iref is a change in the list file; we must
    ;; therefore also increment the revision counter
    (execute-single db (sqlgen::increment-next-revision))
    iref))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-allocate-next-iref db))

(defmethod db-register-identity ((db sqlite-handle)(identity string))
  (bind ((iref (db-allocate-next-iref db))
         (id-sql id-vals (sqlgen::insert-identity iref identity)))
    (apply 'execute-non-query db id-sql id-vals)
    iref))

(defmethod db-iref-to-identity ((db sqlite-handle)(iref integer))
  (execute-single db (format nil "SELECT `identity` FROM `identities` WHERE `iref`='~A'" iref)))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-iref-to-identity db 0))
;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-identity-to-iref db *origin*))

(defmethod db-identity-to-iref ((db sqlite-handle)(identity string))
  (execute-single db (format nil "SELECT `iref` FROM `identities` WHERE `identity`='~A'" identity)))

;;; ---------------------------------------------------------------------
;;; creating standard tables
;;; ---------------------------------------------------------------------

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
                                           (list-id nil)
                                           (origin *origin*)
                                           (format-version +delectus-format-version+))
  ;; - insert the list id
  ;; - insert the list origin
  ;; - insert 0 for the `next_revision` and `next_iref` fields
  (bind ((listid (or list-id (makeid)))
         (sql vals (sqlgen::init-delectus-table list-id origin format-version 0 0)))
    (apply 'execute-non-query db sql vals)))

(defmethod db-create-delectus-table ((db sqlite-handle)
                                     &key
                                       (list-id nil)
                                       (origin *origin*)
                                       (format-version +delectus-format-version+))
  (let* ((list-id (or list-id (makeid))))
    ;; create the delectus table
    (bind ((sql vals (sqlgen::create-delectus-table)))
      (apply 'execute-non-query db sql vals))
    ;; populate it
    (db-initialize-delectus-table db :list-id list-id :origin origin :format-version format-version)))

;;; `identities`
;;; ---------------------------------------------------------------------
;;; maps irefs to identities

(defmethod db-initialize-identities-table ((db sqlite-handle)
                                           &key
                                             (local-origin *origin*)
                                             (list-id nil))
  (assert list-id ()
          "You must supply a valid list identity as the value of :LIST-ID; found ~S" list-id)
  ;; - insert local-origin with the next iref as its key
  ;; - insert the list-id with the next iref as its key
  (bind ((origin-iref (db-allocate-next-iref db))
         (origin-sql origin-vals (sqlgen::insert-identity origin-iref local-origin))
         (list-iref (db-allocate-next-iref db))
         (list-sql list-vals (sqlgen::insert-identity list-iref list-id)))
    (apply 'execute-non-query db origin-sql origin-vals)
    (apply 'execute-non-query db list-sql list-vals)))

(defmethod db-create-identities-table ((db sqlite-handle)
                                       &key
                                         (local-origin *origin*)
                                         (list-id nil))
  (bind ((sql vals (sqlgen::create-identities-table)))
    (apply 'execute-non-query db sql vals)
    (db-initialize-identities-table db :local-origin local-origin :list-id list-id)))

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

(defmethod db-insert-default-listdata-ops ((db sqlite-handle) &key (list-name nil))
  (let* ((op-identity (makeid))
         (op-iref (db-register-identity db op-identity))
         (origin-iref (db-identity-to-iref db *origin*))
         (list-name-timestamp (now-timestamp)))
    ;; insert listname
    (bind ((sql vals (sqlgen::insert-listname list-name op-iref origin-iref list-name-timestamp)))
      (apply 'execute-non-query db sql vals))
    ;; insert columns with default column
    ;; insert default item
    ))

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
        (db-create-delectus-table db :list-id list-id :origin local-origin :format-version format-version)
        (db-create-identities-table db :local-origin local-origin :list-id list-id)
        (db-create-listdata-table db)
        (when create-default-userdata
          (db-insert-default-listdata-ops db :list-name list-name))
        ;;(db-create-item-revision-origin-index db)
        )))
  db-path)

(defmethod create-delectus-file ((db-path string)
                                 &key
                                   (list-name nil)
                                   (list-id nil)
                                   (local-origin *origin*)
                                   (format-version +delectus-format-version+)
                                   (create-default-userdata t))
  (create-delectus-file (pathname db-path) :list-name list-name :list-id list-id :local-origin local-origin
                        :format-version format-version :create-default-userdata create-default-userdata)
  db-path)

;;; (create-delectus-file "/Users/mikel/Desktop/testlist.delectus2" :list-name "Test List")
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


