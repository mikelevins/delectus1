;;;; ***********************************************************************
;;;;
;;;; Name:          store.lisp
;;;; Project:       delectus 2
;;;; Purpose:       operations on Delectus model objects stored in SQLite files
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)
(in-readtable :delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; functions whose names start with "db-" require a valid open
;;; SQLITE-HANDLE; in other words, they operate on open SQLite
;;; databases.
;;;
;;; the corresponding functions whose names omit the "db-" prefix
;;; operate on strings or pathnames, and automatically open and
;;; close the database connection
;;;
;;; an OP is a database row that specifies one of four operations:
;;; - listname: to update the user-defined name of the list
;;; - columns: to update the list's userdata columns and their attributes
;;; - item: to add or update an item in the list
;;; - sync: to record a successful synchronization with another copy
;;;   of the list

;;; ---------------------------------------------------------------------
;;; creating the list file
;;; ---------------------------------------------------------------------

(defmethod db-create-delectus-table ((db sqlite-handle))
  )

(defmethod db-create-listdata-table ((db sqlite-handle))
  )

(defmethod create-delectus-file ((list-name string)(db-path pathname))
  (assert (not (probe-file db-path)) () "file exists: ~S" db-path)
  (with-open-database (db db-path)
    (with-transaction db
      (db-create-delectus-table db)
      (db-create-listdata-table db))))

(defmethod create-delectus-file ((list-name string)(db-path string))
  (create-delectus-file list-name (pathname db-path)))

;;; ---------------------------------------------------------------------
;;; fetching data
;;; ---------------------------------------------------------------------

;;; listid
;;; ---------------------------------------------------------------------

(defmethod db-get-listid ((db sqlite-handle))
  )

(defmethod get-listid ((db-path pathname))
  (with-open-database (db db-path)
    (with-transaction db
      (db-get-listid db))))

(defmethod get-listid ((path string))
  (get-listid (pathname path)))

;;; revision number
;;; ---------------------------------------------------------------------

(defmethod db-get-next-revision ((db sqlite-handle))
  (bind ((sqlupdate _ignore (sql-increment-next-revision))
         (sqlget _ignore (sql-next-revision)))
    (execute-non-query db sqlupdate)
    (execute-single db sqlget)))

(defmethod get-next-revision ((db-path pathname))
  (with-open-database (db db-path)
    (with-transaction db
      (db-get-next-revision db))))

(defmethod get-next-revision ((db-path string))
  (get-next-revision (pathname db-path)))

;;; (get-next-revision "/Users/mikel/Desktop/testlist.delectus2")


;;; listname ops
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-listname ((db sqlite-handle))
  (bind ((sql vals (sql-get-latest-listname)))
    (first (execute-to-list db sql))))

(defmethod get-latest-listname ((db-path pathname))
  (with-open-database (db db-path)
    (with-transaction db
      (db-get-latest-listname db))))

(defmethod get-latest-listname ((db-path string))
  (get-latest-listname (pathname db-path)))

;;; (get-latest-listname "/Users/mikel/Desktop/testlist.delectus2")


;;; columns ops
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-columns ((db sqlite-handle))
  (bind ((sql vals (sql-get-latest-columns)))
    (first (execute-to-list db sql))))

(defmethod get-latest-columns ((db-path pathname))
  (with-open-database (db db-path)
    (with-transaction db
      (db-get-latest-columns db))))

(defmethod get-latest-columns ((db-path string))
  (get-latest-columns (pathname db-path)))

;;; (get-latest-columns "/Users/mikel/Desktop/testlist.delectus2")

;;; item ops
;;; ---------------------------------------------------------------------

;;; NOTE: the query returns a list of (1 . row), because it's
;;; partitioning by id, then sorting descending by revision, then
;;; returning all rows whose rank is 1; so the CDR of each item is the
;;; actual result
(defmethod db-get-latest-items ((db sqlite-handle))
  (bind ((sql vals (sql-get-latest-items)))
    (let ((latest-item-results (execute-to-list db sql)))
      ;; discard the rank field from the returned result
      (mapcar #'cdr latest-item-results))))

(defmethod get-latest-items ((db-path pathname))
  (with-open-database (db db-path)
    (with-transaction db
      (db-get-latest-items db))))

(defmethod get-latest-items ((db-path string))
  (get-latest-items (pathname db-path)))

;;; (get-latest-items "/Users/mikel/Desktop/testlist.delectus2")


;;; sync ops
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-sync ((db sqlite-handle))
  (bind ((sql vals (sql-get-latest-sync)))
    (first (execute-to-list db sql))))

(defmethod get-latest-sync ((db-path pathname))
  (with-open-database (db db-path)
    (with-transaction db
      (db-get-latest-sync db))))

(defmethod get-latest-sync ((db-path string))
  (get-latest-sync (pathname db-path)))

;;; (get-latest-sync "/Users/mikel/Desktop/testlist.delectus2")

;;; ---------------------------------------------------------------------
;;; asserting ops
;;; ---------------------------------------------------------------------
;;; the general model for assertions is:
;;; 1. compute the mutations we're going to make, signaling an error
;;;    if any prerequisite is not met
;;; 2. execute the computed mutations

;;; listname ops
;;; ---------------------------------------------------------------------

(defmethod db-assert-listname ((db sqlite-handle)
                               &key opid (origin *origin*) revision timestamp name)
  )

(defmethod assert-listname ((db-path pathname)
                            &key opid (origin *origin*) revision timestamp name)
  )

(defmethod assert-listname ((db-path string)
                            &key opid (origin *origin*) revision timestamp name)
  (assert-listname (pathname db-path)
                   :opid opid :origin origin :revision revision :timestamp timestamp :name name))

;;; columns ops
;;; ---------------------------------------------------------------------


(defmethod db-assert-columns ((db sqlite-handle)
                              &key opid origin revision timestamp columns)
  )

(defmethod assert-columns ((db-path pathname)
                           &key opid origin revision timestamp columns)
  )

(defmethod assert-columns ((db-path string)
                           &key opid origin revision timestamp columns)
  )

;;; item ops
;;; ---------------------------------------------------------------------


(defmethod db-assert-item ((db sqlite-handle)
                           &key opid origin revision timestamp item deleted columns)
  )

(defmethod assert-item ((db-path pathname)
                        &key opid origin revision timestamp item deleted columns)
  )

(defmethod assert-item ((db-path string)
                        &key opid origin revision timestamp item deleted columns)
  )


;;; sync ops
;;; ---------------------------------------------------------------------


(defmethod db-assert-sync ((db sqlite-handle)
                           &key opid origin revision timestamp peer)
  )

(defmethod assert-sync ((db-path pathname)
                        &key opid origin revision timestamp peer)
  )

(defmethod assert-sync ((db-path string)
                        &key opid origin revision timestamp peer)
  )
