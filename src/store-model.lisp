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

;;; naming conventions:
;;;
;;; - db-foo:
;;;   A function whose name starts with "db-" operates on a SQLite
;;;   database handle. Thatmeans it must be called within a
;;;   WITH-OPEN-DATABASE form, and within WITH-TRANSACTION if
;;;   transaction preotection is needed.
;;;
;;; - foo:
;;;   A function whose name does not start with "db-"  does not
;;;   operate on a database handle, and so needs no special
;;;   protection from enclosing database forms.

;;; =====================================================================
;;;
;;; list files
;;;
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; creating the 'delectus' table
;;; ---------------------------------------------------------------------

(defmethod db-create-delectus-table ((db sqlite-handle) (listid string))
  (assert (identity-string? listid)()
          "Expected an identity-string for the :LISTID paramter, but found ~S"
          listid)
  (let* ((list-identity (string->identity listid)))
    (bind ((create-sql create-vals (sqlgen-create-delectus-table))
           (init-sql init-vals (sqlgen-init-delectus-table listid +delectus-format-version+)))
      (apply 'execute-non-query db create-sql create-vals)
      (apply 'execute-non-query db init-sql init-vals))))

;;; ---------------------------------------------------------------------
;;; creating the 'listnames' table
;;; ---------------------------------------------------------------------

(defmethod db-create-listnames-table ((db sqlite-handle))
  (bind ((create-sql create-vals (sqlgen-create-listnames-table)))
    (apply 'execute-non-query db create-sql create-vals)))

;;; ---------------------------------------------------------------------
;;; creating the 'comments' table
;;; ---------------------------------------------------------------------

(defmethod db-create-comments-table ((db sqlite-handle))
  (bind ((create-sql create-vals (sqlgen-create-comments-table)))
    (apply 'execute-non-query db create-sql create-vals)))

;;; ---------------------------------------------------------------------
;;; creating the 'columns' table
;;; ---------------------------------------------------------------------

(defmethod db-create-columns-table ((db sqlite-handle))
  (bind ((create-sql create-vals (sqlgen-create-columns-table)))
    (apply 'execute-non-query db create-sql create-vals)))

;;; ---------------------------------------------------------------------
;;; creating the 'items' table
;;; ---------------------------------------------------------------------

(defmethod db-create-items-table ((db sqlite-handle))
  (bind ((create-sql create-vals (sqlgen-create-items-table)))
    (apply 'execute-non-query db create-sql create-vals)))

;;; ---------------------------------------------------------------------
;;; creating the 'items' main index
;;; ---------------------------------------------------------------------

(defmethod db-create-item-revision-origin-index ((db sqlite-handle))
  (bind ((create-sql create-vals (sqlgen-create-item-revision-origin-index)))
    (apply 'execute-non-query db create-sql create-vals)))

;;; ---------------------------------------------------------------------
;;; the next revision
;;; ---------------------------------------------------------------------

(defmethod db-get-next-revision ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-next-revision)))
    (apply 'execute-single db sql vals)))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-get-next-revision db))

(defmethod db-inc-next-revision ((db sqlite-handle))
  (bind ((sql vals (sqlgen-inc-next-revision)))
    (apply 'execute-non-query db sql vals)))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-inc-next-revision db))

;;; ---------------------------------------------------------------------
;;; the next item
;;; ---------------------------------------------------------------------

(defmethod db-get-next-item ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-next-item)))
    (apply 'execute-single db sql vals)))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-get-next-item db))

(defmethod db-inc-next-item ((db sqlite-handle))
  (bind ((sql vals (sqlgen-inc-next-item)))
    (apply 'execute-non-query db sql vals)))

;;; (with-open-database (db "/Users/mikel/Desktop/testlist.delectus2") (db-inc-next-item db))

;;; ---------------------------------------------------------------------
;;; inserting ops
;;; ---------------------------------------------------------------------

(defmethod db-insert-listname ((db sqlite-handle)
                               &key
                                 origin
                                 revision
                                 timestamp
                                 name)
  (bind ((sql vals (sqlgen-insert-listname origin revision timestamp name)))
    (apply 'execute-non-query db sql vals)))

;;; ---------------------------------------------------------------------
;;; creating a list file
;;; ---------------------------------------------------------------------

(defmethod create-delectus-file ((db-path pathname)
                                 &key
                                   (listname nil)
                                   (listid nil)
                                   (origin nil)
                                   (format +delectus-format-version+)
                                   (create-default-userdata t))
  (assert (not (probe-file db-path)) () "file exists: ~S" db-path)
  (assert (stringp listname) () "Expected a string :LISTNAME parameter, but found ~S" listname)
  (let ((listid (or listid (make-identity-string))))
    (with-open-database (db db-path)
      (with-transaction db
        (db-create-delectus-table db listid)
        (db-create-listnames-table db)
        (db-create-comments-table db)
        (db-create-columns-table db)
        (db-create-items-table db)
        (db-create-item-revision-origin-index db)
        (when create-default-userdata
          (let ((origin (make-origin (process-identity) db-path))
                (revision (db-get-next-revision db)))
            (db-insert-listname db :origin origin :revision revision :timestamp (now-utc) :name listname)
            )))))
  db-path)

(defmethod create-delectus-file ((db-path string)
                                 &key
                                   (listname nil)
                                   (listid nil)
                                   (format +delectus-format-version+)
                                   (create-default-userdata t))
  (create-delectus-file (pathname db-path)
                        :listname listname
                        :listid listid
                        :create-default-userdata create-default-userdata)
  db-path)

;;; (setf $testlist (pathname "/Users/mikel/Desktop/testlist.delectus2"))
;;; (create-delectus-file $testlist :listname "Test List" :listid (make-identity-string))
;;; (delete-file $testlist)
