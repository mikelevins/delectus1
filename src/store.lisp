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

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
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

;;; =====================================================================
;;; checking columns
;;; =====================================================================

;;; columns-missing-from-file
;;; ---------------------------------------------------------------------
;;; returns a list of column-data objects that describe columns that
;;; don't exist in the SQLite file.
;;;
;;; In the columns op we use it to identify columns that must be
;;; created before the op can be asserted. In the item op we use it to
;;; idenfity situations where we must signal an error because the op
;;; references columns that don't exist.

(defmethod db-columns-missing-from-file ((db sqlite-handle) (column-data-list list))
  (let* ((found-userdata-info (db-get-userdata-column-info db))
         (found-userdata-labels (mapcar #'column-info-name found-userdata-info)))
    (remove-if (lambda (column-data)
                 (member (fset:@ column-data :|id|)
                         found-userdata-labels
                         :test #'equal))
               column-data-list)))

(defmethod columns-missing-from-file ((db-path pathname) (column-data-list list))
  (with-open-database (db db-path)
    (db-columns-missing-from-file db column-data-list)))

(defmethod columns-missing-from-file ((db-path string) (column-data-list list))
  (columns-missing-from-file (pathname db-path) column-data-list))

;;; (setf $existing (column-data :id "Ibe8f18857a7611ea909e38c9864ebde0" :name "Existing column"))
;;; (setf $nonexistent (column-data :id "BOGUS" :name "Nonexistent column"))
;;; (columns-missing-from-file "/Users/mikel/Desktop/testlist.delectus2" (list $existing $nonexistent))

;;; columns-missing-from-input
;;; ---------------------------------------------------------------------
;;; returns a list of column IDs that are found in the list file but
;;; not in the input column-data list. If this list isn't empty in
;;; columns or item ops, then one or more userdata columns was added
;;; after the op was constructed, but before it was asserted. This is
;;; an error.

(defmethod db-columns-missing-from-input ((db sqlite-handle) (column-data-list list))
  (let* ((found-userdata-info (db-get-userdata-column-info db))
         (found-userdata-labels (mapcar #'column-info-name found-userdata-info))
         (column-data-labels (mapcar (lambda (cdl)(fset:@ cdl :|id|))
                                     column-data-list)))
    (remove-if (lambda (ful)(member ful column-data-labels :test #'equal))
               found-userdata-labels)))

(defmethod columns-missing-from-input ((db-path pathname) (column-data-list list))
  (with-open-database (db db-path)
    (db-columns-missing-from-input db column-data-list)))

(defmethod columns-missing-from-input ((db-path string) (column-data-list list))
  (columns-missing-from-input (pathname db-path) column-data-list))

;;; (setf $existing (column-data :id "Ia56197057b5e11ea909e38c9864ebde0" :name "Existing column"))
;;; (setf $nonexistent (column-data :id "BOGUS" :name "Nonexistent column"))
;;; (columns-missing-from-input "/Users/mikel/Desktop/testlist.delectus2" (list $existing $nonexistent))
;;; (columns-missing-from-input "/Users/mikel/Desktop/testlist.delectus2" (list $nonexistent))

;;; =====================================================================
;;; creating the list file
;;; =====================================================================

(defmethod db-create-delectus-table ((db sqlite-handle)(listid string))
  (let* ((listid (or listid (makeid)))
         (next-rev 3)) ; 0 is initial listname; 1 is initial columns; 2 is initial item
    ;; create the delectus table
    (bind ((sql vals (sql-create-delectus-table)))
      (apply 'execute-non-query db sql vals))
    ;; populate it
    (bind ((sql vals (sql-populate-delectus-table listid *origin* +delectus-format-version+ next-rev)))
      (apply 'execute-non-query db sql vals))))

(defmethod db-create-listdata-table ((db sqlite-handle)(list-name string)(listid string)
                                     &key origin (create-default-userdata t))
  (let* ((listname-opid (makeid))
         (columns-opid (makeid))
         (item-opid (makeid))
         (origin (or origin *origin*))
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
    ;; create the default userdata, if it's requested
    (when create-default-userdata
      ;; assert the initial columns op
      (db-assert-columns db :opid columns-opid :origin origin :revision columns-rev :timestamp (now-timestamp)
                         :column-data (list userdata-column-data))
      ;; assert the initial item op
      (db-assert-item db :opid item-opid :origin origin :revision item-rev :timestamp (now-timestamp)
                      :column-data (list userdata-column-data) :column-values (list nil)))))

(defmethod create-delectus-file ((db-path pathname)(list-name string)(listid string) &key (create-default-userdata t))
  (assert (not (probe-file db-path)) () "file exists: ~S" db-path)
  (with-open-database (db db-path)
    (with-transaction db
      (db-create-delectus-table db listid)
      (db-create-listdata-table db list-name listid :create-default-userdata create-default-userdata))))

(defmethod create-delectus-file ((db-path string)(list-name string)(listid string) &key (create-default-userdata t))
  (create-delectus-file (pathname db-path) list-name listid :create-default-userdata create-default-userdata))

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

;;; =====================================================================
;;; fetching data
;;; =====================================================================

;;; column-info
;;; ---------------------------------------------------------------------

(defmethod db-get-column-info ((db sqlite-handle))
  (bind ((sqlget vals (sql-get-column-info)))
    (mapcar (lambda (info)(apply 'column-info info))
            (apply 'execute-to-list db sqlget vals))))

(defmethod get-column-info ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-column-info db)))

(defmethod get-column-info ((path string))
  (get-column-info (pathname path)))

;;; (get-column-info "/Users/mikel/Desktop/testlist.delectus2")

;;; userdata-column-info
;;; ---------------------------------------------------------------------
;;; returns the column-info only for userdata columns

(defmethod db-get-userdata-column-info ((db sqlite-handle))
  (let ((all-info (db-get-column-info db)))
    ;; remove the metadata columns
    (remove-if (lambda (info)
                 (find (column-info-name info)
                       +metadata-column-labels+
                       :test #'equal))
               all-info)))

(defmethod get-userdata-column-info ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-userdata-column-info db)))

(defmethod get-userdata-column-info ((path string))
  (get-userdata-column-info (pathname path)))

;;; (get-userdata-column-info "/Users/mikel/Desktop/testlist.delectus2")


;;; ---------------------------------------------------------------------
;;; getting list metadata
;;; ---------------------------------------------------------------------

;; list-id
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

;;; ---------------------------------------------------------------------
;;; getting list ops
;;; ---------------------------------------------------------------------

;;; op fields
;;; ---------------------------------------------------------------------

(defun op-type (op)(elt op 0))
(defun op-id (op)(elt op 1))
(defun op-origin (op)(elt op 2))
(defun op-revision (op)(elt op 3))
(defun op-timestamp (op)(elt op 4))
(defun op-item (op)(elt op 5))
(defun op-name (op)(elt op 6))
(defun op-deleted (op)(elt op 7))
(defun op-peer (op)(elt op 8))
(defun op-userdata-ids (op)(nthcdr 9 op))

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


;;; =====================================================================
;;; ops
;;; =====================================================================
;;; the general model for assertions is:
;;; 1. compute the mutations we're going to make, signaling an error
;;;    if any prerequisite is not met
;;; 2. execute the computed mutations

;;; ---------------------------------------------------------------------
;;; asserting ops
;;; ---------------------------------------------------------------------

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
;;; column-data is a list of column-data objects

(defmethod db-assert-columns ((db sqlite-handle)
                              &key opid origin revision timestamp column-data)
  (let ((missing-input (db-columns-missing-from-input db column-data)))
    (assert (null missing-input)()
            "Userdata columns in the list file are missing from the column data in the op: ~S"
            missing-input))
  (let ((columns-missing-from-file (db-columns-missing-from-file db column-data))
        (opid (or opid (makeid)))
        (revision (or revision (db-get-next-revision db)))
        (timestamp (or timestamp (now-timestamp))))
    (when columns-missing-from-file
      (loop for cd in columns-missing-from-file
         do (let ((colid (fset:@ cd :|id|)))
              (db-add-userdata-column db colid))))
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

;;; should fail because there is a column already defined in the file and we don't mention it
;;; (setf $cdata (fset:with +default-initial-column-attributes+ :|id| (makeid)))
;;; (assert-columns "/Users/mikel/Desktop/testlist.delectus2" :opid (makeid) :origin *origin* :revision (get-next-revision "/Users/mikel/Desktop/testlist.delectus2") :timestamp (now-timestamp) :column-data (list $cdata))

;;; item ops
;;; ---------------------------------------------------------------------
;;; column-data is a list of column-data objects. column-values is a
;;; list of field values, one for each column

(defmethod db-assert-item ((db sqlite-handle)
                           &key opid origin revision timestamp item deleted column-data column-values)
  (let ((missing-input (db-columns-missing-from-input db column-data)))
    (assert (null missing-input)()
            "Userdata columns in the list file are missing from the column data in the op: ~S"
            missing-input))
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

;;; should fail because there is a column already defined in the file and we don't mention it
;;; (setf $cdata (fset:with +default-initial-column-attributes+ :|id| (makeid)))
;;; (assert-item "/Users/mikel/Desktop/testlist.delectus2" :opid (makeid) :origin *origin* :revision (get-next-revision "/Users/mikel/Desktop/testlist.delectus2") :timestamp (now-timestamp) :item (makeid) :column-data (list $cdata) :column-values '(nil))

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
