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
;;; SQLITE-HANDLE; in other words, they operate within transactions on
;;; open SQLite databases.
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
;;; initial data
;;; ---------------------------------------------------------------------

(defparameter +default-initial-column-attributes+
  {:|name| "Item"
    :|type| "TEXT"
    :|order| 10.0
    :|sort| "ASC"
    :|title| t
    :|subtitle| :false
    :|deleted| :false})

;;; (to-json +default-initial-column-attributes+)
;;; (fset:with +default-initial-column-attributes+ :|id| (makeid))

;;; ---------------------------------------------------------------------
;;; creating the list file
;;; ---------------------------------------------------------------------

(defmethod db-create-delectus-table ((db sqlite-handle)(listid string))
  (let* ((next-rev 3)) ; 0 is initial listname; 1 is initial columns; 2 is initial item
    ;; create the delectus table
    (bind ((sql vals (sql-create-delectus-table)))
      (apply 'execute-non-query db sql vals))
    ;; populate it
    (bind ((sql vals (sql-populate-delectus-table listid *origin* +delectus-format-version+ next-rev)))
      (apply 'execute-non-query db sql vals))))

(defmethod db-create-listdata-table ((db sqlite-handle)(list-name string)(listid string)
                                     &key origin)
  (let* ((listname-opid (makeid))
         (columns-opid (makeid))
         (item-opid (makeid))
         (origin (or origin (makeid)))
         (listname-rev 0)
         (columns-rev 1)
         (item-rev 2)
         (userdata-column-id (makeid))
         (userdata-column-data (fset:with +default-initial-column-attributes+ :|id| userdata-column-id)))
    ;; create the table
    (bind ((sql vals (sql-create-listdata-table)))
      (apply 'execute-non-query db sql vals))
    ;; assert the initial listname op
    (bind ((sql vals (sql-assert-listname listname-opid origin listname-rev (now-timestamp) nil list-name nil nil)))
      (apply 'execute-non-query db sql vals))
    ;; add the initial userdata column
    (db-add-userdata-column db userdata-column-id)
    ;; assert the initial columns op
    (db-assert-columns db :opid columns-opid :origin origin :revision columns-rev :timestamp (now-timestamp)
                       :column-data (list userdata-column-data))
    ;; assert the initial item op
    (db-assert-item db :opid item-opid :origin origin :revision item-rev :timestamp (now-timestamp)
                    :column-data (list userdata-column-data) :column-values (list nil))))

(defmethod create-delectus-file ((db-path pathname)(list-name string)(listid string))
  (assert (not (probe-file db-path)) () "file exists: ~S" db-path)
  (with-open-database (db db-path)
    (with-transaction db
      (db-create-delectus-table db listid)
      (db-create-listdata-table db list-name listid))))

(defmethod create-delectus-file ((db-path string)(list-name string)(listid string))
  (create-delectus-file (pathname db-path) list-name listid))

;;; (create-delectus-file "/Users/mikel/Desktop/testlist.delectus2" "Test List" (makeid))

;;; ---------------------------------------------------------------------
;;; adding userdata columns
;;; ---------------------------------------------------------------------

(defmethod db-add-userdata-column ((db sqlite-handle) (column-id string))
  (bind ((sql vals (sql-add-userdata-column column-id "TEXT")))
    (apply 'execute-non-query db sql vals)))

(defmethod add-userdata-column ((db-path pathname) (column-id string))
  (with-open-database (db db-path)
    (with-transaction db
      (db-add-userdata-column db column-id))))

(defmethod add-userdata-column ((db-path string) (column-id string))
  (add-userdata-column (pathname db-path) column-id))

;;; ---------------------------------------------------------------------
;;; fetching data
;;; ---------------------------------------------------------------------

;;; list-id
;;; ---------------------------------------------------------------------

(defmethod db-get-list-id ((db sqlite-handle))
  (bind ((sqlget vals (sql-list-id)))
    (apply 'execute-single db sqlget vals)))

(defmethod get-list-id ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-list-id db)))

(defmethod get-list-id ((path string))
  (get-list-id (pathname path)))

;;; (get-list-id "/Users/mikel/Desktop/testlist.delectus2")

;;; list-origin
;;; ---------------------------------------------------------------------

(defmethod db-get-list-origin ((db sqlite-handle))
  (bind ((sqlget vals (sql-list-origin)))
    (apply 'execute-single db sqlget vals)))

(defmethod get-list-origin ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-list-origin db)))

(defmethod get-list-origin ((path string))
  (get-list-origin (pathname path)))

;;; (get-list-origin "/Users/mikel/Desktop/testlist.delectus2")

;;; list-format
;;; ---------------------------------------------------------------------

(defmethod db-get-list-format ((db sqlite-handle))
  (bind ((sqlget vals (sql-list-format)))
    (apply 'execute-single db sqlget vals)))

(defmethod get-list-format ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-list-format db)))

(defmethod get-list-format ((path string))
  (get-list-format (pathname path)))

;;; (get-list-format "/Users/mikel/Desktop/testlist.delectus2")

;;; revision number
;;; ---------------------------------------------------------------------

(defmethod db-get-next-revision ((db sqlite-handle))
  (bind ((sqlupdate upvals (sql-increment-next-revision))
         (sqlget getvals (sql-next-revision)))
    (apply 'execute-non-query db sqlupdate upvals)
    (apply 'execute-single db sqlget getvals)))

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
    (first (apply 'execute-to-list db sql vals))))

(defmethod get-latest-listname ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-latest-listname db)))

(defmethod get-latest-listname ((db-path string))
  (get-latest-listname (pathname db-path)))

;;; (time (get-latest-listname "/Users/mikel/Desktop/testlist.delectus2"))


;;; columns ops
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-columns ((db sqlite-handle))
  (bind ((sql vals (sql-get-latest-columns)))
    (first (apply 'execute-to-list db sql vals))))

(defmethod get-latest-columns ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-latest-columns db)))

(defmethod get-latest-columns ((db-path string))
  (get-latest-columns (pathname db-path)))

;;; (time (get-latest-columns "/Users/mikel/Desktop/testlist.delectus2"))

;;; item ops
;;; ---------------------------------------------------------------------

;;; NOTE: the query returns a list of (1 . row), because it's
;;; partitioning by id, then sorting descending by revision, then
;;; returning all rows whose rank is 1; so the CDR of each item is the
;;; actual result
(defmethod db-get-latest-items ((db sqlite-handle))
  (bind ((sql vals (sql-get-latest-items)))
    (let ((latest-item-results (apply 'execute-to-list db sql vals)))
      ;; discard the rank field from the returned result
      (mapcar #'cdr latest-item-results))))

(defmethod get-latest-items ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-latest-items db)))

(defmethod get-latest-items ((db-path string))
  (get-latest-items (pathname db-path)))

;;; (time (get-latest-items "/Users/mikel/Desktop/testlist.delectus2"))


;;; sync ops
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-sync ((db sqlite-handle))
  (bind ((sql vals (sql-get-latest-sync)))
    (first (apply 'execute-to-list db sql vals))))

(defmethod get-latest-sync ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-latest-sync db)))

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
  (let ((opid (or opid (makeid)))
        (revision (or revision (db-get-next-revision db)))
        (timestamp (or timestamp (now-timestamp))))
    (bind ((sql vals
                (sql-assert-listname opid origin revision timestamp nil name nil nil)))
      (apply 'execute-non-query db sql vals))))

(defmethod assert-listname ((db-path pathname)
                            &key opid (origin *origin*) revision timestamp name)
  (assert (stringp name)() "The :NAME parameter must be a text string")
  (with-open-database (db db-path)
    (with-transaction db
      (db-assert-listname db :opid opid :origin origin :revision revision :timestamp timestamp :name name))))

(defmethod assert-listname ((db-path string)
                            &key opid (origin *origin*) revision timestamp name)
  (assert-listname (pathname db-path)
                   :opid opid :origin origin :revision revision :timestamp timestamp :name name))

;;; columns ops
;;; ---------------------------------------------------------------------


(defmethod db-assert-columns ((db sqlite-handle)
                              &key opid origin revision timestamp column-data)
  (let ((opid (or opid (makeid)))
        (revision (or revision (db-get-next-revision db)))
        (timestamp (or timestamp (now-timestamp))))
    (bind ((sql vals
                (sql-assert-columns opid origin revision timestamp nil nil nil nil :column-data column-data)))
      (apply 'execute-non-query db sql vals))))

(defmethod assert-columns ((db-path pathname)
                           &key opid origin revision timestamp column-data)
  (with-open-database (db db-path)
    (with-transaction db
      (db-assert-columns db :opid opid :origin origin :revision revision :timestamp timestamp :column-data column-data))))

(defmethod assert-columns ((db-path string)
                           &key opid origin revision timestamp column-data)
  (assert-columns (pathname db-path)
                  :opid opid :origin origin :revision revision :timestamp timestamp :column-data column-data))

;;; item ops
;;; ---------------------------------------------------------------------


(defmethod db-assert-item ((db sqlite-handle)
                           &key opid origin revision timestamp item deleted column-data column-values)
  (let ((opid (or opid (makeid)))
        (revision (or revision (db-get-next-revision db)))
        (timestamp (or timestamp (now-timestamp))))
    (bind ((sql vals
                (sql-assert-item opid origin revision timestamp item deleted nil nil
                                 :column-data column-data :column-values column-values)))
      (apply 'execute-non-query db sql vals))))

(defmethod assert-item ((db-path pathname)
                        &key opid origin revision timestamp item deleted column-data column-values)
  (with-open-database (db db-path)
    (with-transaction db
      (db-assert-item db :opid opid :origin origin :revision revision
                      :timestamp timestamp :item item :deleted deleted
                      :column-data column-data :column-values column-values))))

(defmethod assert-item ((db-path string)
                        &key opid origin revision timestamp item deleted column-data column-values)
  (assert-item (pathname db-path)
               :opid opid :origin origin :revision revision
               :timestamp timestamp :item item :deleted deleted
               :column-data column-data :column-values column-values))


;;; sync ops
;;; ---------------------------------------------------------------------


(defmethod db-assert-sync ((db sqlite-handle)
                           &key opid origin revision timestamp peer)
  (let ((opid (or opid (makeid)))
        (revision (or revision (db-get-next-revision db)))
        (timestamp (or timestamp (now-timestamp))))
    (bind ((sql vals
                (sql-assert-sync opid origin revision timestamp nil nil nil peer)))
      (apply 'execute-non-query db sql vals))))

(defmethod assert-sync ((db-path pathname)
                        &key opid origin revision timestamp peer)
  (assert (stringp peer)() "The :PEER parameter must be an identity")
  (with-open-database (db db-path)
    (with-transaction db
      (db-assert-sync db :opid opid :origin origin :revision revision :timestamp timestamp :peer peer))))

(defmethod assert-sync ((db-path string)
                        &key opid origin revision timestamp peer)
  (assert-listname (pathname db-path)
                   :opid opid :origin origin :revision revision :timestamp timestamp :peer peer))
