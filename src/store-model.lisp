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

(defmethod db-set-next-revision ((db sqlite-handle)(rev integer))
  (bind ((sql vals (sqlgen-set-next-revision rev)))
    (apply 'execute-single db sql vals)))

;;; ---------------------------------------------------------------------
;;; the next item
;;; ---------------------------------------------------------------------

(defmethod db-get-next-item ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-next-item)))
    (apply 'execute-single db sql vals)))

(defmethod db-set-next-item ((db sqlite-handle)(rev integer))
  (bind ((sql vals (sqlgen-set-next-item rev)))
    (apply 'execute-single db sql vals)))

;;; ---------------------------------------------------------------------
;;; inserting ops
;;; ---------------------------------------------------------------------

(defmethod db-insert-listname ((db sqlite-handle)
                               &key
                                 origin
                                 revision
                                 timestamp
                                 name)
  (bind ((revision (or revision (db-get-next-revision db)))
         (sql vals (sqlgen-insert-listname origin revision timestamp name)))
    (apply 'execute-non-query db sql vals)
    (db-set-next-revision db (1+ revision))))

(defmethod db-insert-columns ((db sqlite-handle)
                              &key
                                origin
                                revision
                                timestamp
                                column-descriptions)
  (bind ((revision (or revision (db-get-next-revision db)))
         (sql vals (sqlgen-insert-columns origin revision timestamp column-descriptions)))
    (apply 'execute-non-query db sql vals)
    (db-set-next-revision db (1+ revision))))

(defmethod db-insert-item ((db sqlite-handle)
                              &key
                                origin
                                revision
                                timestamp
                                item
                                deleted
                                column-values)
  (bind ((revision (or revision (db-get-next-revision db)))
         (item (or item (db-get-next-item db)))
         (sql vals (sqlgen-insert-item origin revision timestamp item deleted column-values)))
    (apply 'execute-non-query db sql vals)
    (db-set-next-revision db (1+ revision))
    (db-set-next-item db (1+ item))))

;;; ---------------------------------------------------------------------
;;; checking columns
;;; ---------------------------------------------------------------------

;;; (sqlite-table-column-info "/Users/mikel/Desktop/testlist.delectus2" "listnames")

(defmethod db-ensure-columns-exist ((db sqlite-handle) (column-descriptions list))
  (let* ((supplied-column-labels (mapcar (lambda (desc)(identity->column-label (getf desc :|id|)))
                                         column-descriptions))
         (columns-column-labels (mapcar 'column-info-name
                                        (db-sqlite-table-column-info db *columns-table-name*)))
         (items-column-labels (mapcar 'column-info-name
                                      (db-sqlite-table-column-info db *items-table-name*)))
         (missing-columns-column-labels (remove-list-elements columns-column-labels supplied-column-labels))
         (missing-items-column-labels (remove-list-elements items-column-labels supplied-column-labels)))
    (when missing-columns-column-labels
      (loop for label in missing-columns-column-labels
         do (bind ((sql vals (sqlgen-add-columns-userdata-column label)))
              (apply 'execute-non-query db sql vals))))
    (when missing-items-column-labels
      (loop for label in missing-items-column-labels
         do (bind ((sql vals (sqlgen-add-items-userdata-column label)))
              (apply 'execute-non-query db sql vals))))))

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
  (let* ((listid (or listid (make-identity-string))))
    (with-open-database (db db-path)
      (with-transaction db
        (db-create-delectus-table db listid)
        (db-create-listnames-table db)
        (db-create-comments-table db)
        (db-create-columns-table db)
        (db-create-items-table db)
        (db-create-item-revision-origin-index db)
        (when create-default-userdata
          (let* ((origin (make-origin (process-identity) db-path))
                 (default-column (column-description :id (make-identity-string)
                                                                   :name "Item"
                                                                   :order 10.0
                                                                   :sort :null
                                                                   :title :false
                                                                   :subtitle :false
                                                                   :deleted :false))
                 (default-column-descriptions (list default-column))
                 (default-column-id (getf default-column :|id|)))
            (db-ensure-columns-exist db default-column-descriptions)
            (db-insert-listname db :origin origin :timestamp (now-utc) :name listname)
            (db-insert-columns db :origin origin :timestamp (now-utc)
                               :column-descriptions default-column-descriptions)
            (db-insert-item db :origin origin :timestamp (now-utc)
                            :column-values [default-column-id nil])
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
;;; (create-delectus-file $testlist :listname "Test List" :listid (make-identity-string) :create-default-userdata nil)
;;; (delete-file $testlist)

;;; ---------------------------------------------------------------------
;;; getting the latest items
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-items ((db sqlite-handle)
                                &key
                                  (offset 0)
                                  (limit 100))
  (bind ((sql vals (sqlgen-get-latest-items :limit limit :offset offset)))
    (apply 'execute-to-list db sql vals)))

(defmethod get-latest-items ((db-path pathname)
                             &key
                               (offset 0)
                               (limit 100))
  (assert (probe-file db-path) () "No such file: ~S" db-path)
  (with-open-database (db db-path)
    (db-get-latest-items db :offset offset :limit limit)))

;;; (setf $movies-test-path "/Users/mikel/Desktop/Movies-test.delectus2")
;;; (time (get-latest-items (pathname $movies-test-path)))
;;; (time (get-latest-items (pathname $movies-test-path) :offset 1000))

;;; (setf $zips-test-path "/Users/mikel/Desktop/Zipcodes.delectus2")
;;; (time (get-latest-items (pathname $zips-test-path)))
;;; (time (get-latest-items (pathname $zips-test-path) :offset 30000))

