;;;; ***********************************************************************
;;;;
;;;; Name:          store-listfile.lisp
;;;; Project:       delectus 2
;;;; Purpose:       operations on Delectus list files
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

;;; ---------------------------------------------------------------------
;;; ensuring columns exist
;;; ---------------------------------------------------------------------

(defmethod db-ensure-columns-exist ((db sqlite-handle) column-descriptions)
  (let* ((supplied-column-labels (mapcar (lambda (desc)(getf desc :|label|))
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
;;; creating the main index
;;; ---------------------------------------------------------------------

(defmethod db-create-items-itemid-timestamp-index ((db sqlite-handle))
  (bind ((sql vals (sqlgen-create-items-itemid-timestamp-index)))
    (apply 'execute-non-query db sql vals)))

;;; ---------------------------------------------------------------------
;;; inserting ops
;;; ---------------------------------------------------------------------

(defmethod db-insert-listname ((db sqlite-handle)
                               &key
                                 revision
                                 origin
                                 timestamp
                                 name)
  (bind ((revision (or revision (db-get-next-revision db)))
         (origin (or origin (error "Missing :ORIGIN argument")))
         (timestamp (or timestamp (delectus-timestamp-now)))
         (name (or name (error "Missing :NAME argument")))
         (sql vals (sqlgen-insert-listname revision origin timestamp name)))
    (apply 'execute-non-query db sql vals)
    (db-set-next-revision db (1+ revision))))


(defmethod db-insert-columns ((db sqlite-handle)
                              &key
                                revision
                                origin
                                timestamp
                                column-descriptions)
  (bind ((revision (or revision (db-get-next-revision db)))
         (origin (or origin (error "Missing :ORIGIN argument")))
         (timestamp (or timestamp (delectus-timestamp-now)))
         (sql vals (sqlgen-insert-columns revision origin timestamp column-descriptions)))
    (apply 'execute-non-query db sql vals)
    (db-set-next-revision db (1+ revision))))


(defmethod db-insert-item ((db sqlite-handle)
                           &key
                             revision
                             origin
                             timestamp
                             itemid
                             deleted
                             column-values)
  (bind ((revision (or revision (db-get-next-revision db)))
         (origin (or origin (error "Missing :ORIGIN argument")))
         (timestamp (or timestamp (delectus-timestamp-now)))
         (itemid (or itemid (db-get-next-itemid db)))
         (sql vals (sqlgen-insert-item revision origin timestamp itemid deleted column-values)))
    (apply 'execute-non-query db sql vals)
    (db-set-next-revision db (1+ revision))
    (db-set-next-itemid db (1+ itemid))))


;;; ---------------------------------------------------------------------
;;; fetching ops
;;; ---------------------------------------------------------------------

;;; listname
;;; --------

(defmethod db-get-latest-listname ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-latest-listname)))
    (apply 'execute-single db sql vals)))

;;; (setf $movies-test-path "/Users/mikel/Desktop/Movies-test.delectus2")
;;; (with-open-database (db $movies-test-path)(db-get-latest-listname db))

;;; columns
;;; -------

(defmethod db-get-latest-columns ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-latest-columns)))
    (first (apply 'execute-to-list db sql vals))))

;;; (setf $movies-test-path "/Users/mikel/Desktop/Movies-test.delectus2")
;;; (with-open-database (db $movies-test-path)(db-get-latest-listname db))

;;; items
;;; -------


(defmethod db-check-latest-items-table-exists ((db sqlite-handle))
  (bind ((sql vals (sqlgen-check-latest-items-table-exists))
         (found-table (apply 'execute-to-list db sql vals)))
    (if found-table t nil)))

;;; (setf $words-test-path "/Users/mikel/Desktop/wordtest100k.delectus2")
;;; (with-open-database (db $words-test-path) (db-check-latest-items-table-exists db))

(defmethod db-create-latest-items-table ((db sqlite-handle))
  (bind ((sql vals (sqlgen-create-latest-items-table)))
    (apply 'execute-to-list db sql vals)))

;;; (setf $words-test-path "/Users/mikel/Desktop/wordtest100k.delectus2")

(defmethod db-get-latest-items ((db sqlite-handle)
                                &key
                                  (offset 0)
                                  (limit 100))
  (unless (db-check-latest-items-table-exists db)
    (db-create-latest-items-table db))
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
;;; (delete-file $movies-test-path)
;;; (time (get-latest-items (pathname $movies-test-path)))
;;; (time (get-latest-items (pathname $movies-test-path) :offset 1000 :limit 500))


;;; ---------------------------------------------------------------------
;;; creating the list file
;;; ---------------------------------------------------------------------

(defmethod create-delectus-file ((db-path pathname)
                                 &key
                                   (listname nil)
                                   (listid nil)
                                   (format +delectus-format-version+)
                                   (create-default-userdata t))
  (assert (not (probe-file db-path)) () "file exists: ~S" db-path)
  (assert (stringp listname) () "Expected a string :LISTNAME parameter, but found ~S" listname)
  (let* ((listid (or listid (make-identity-string))))
    (with-open-database (db db-path)
      (with-transaction db
        (db-create-delectus-table db listid format)
        (db-create-listnames-table db)
        (db-create-comments-table db)
        (db-create-columns-table db)
        (db-create-items-table db)
        (db-create-items-itemid-timestamp-index db)
        (when create-default-userdata
          (let* ((origin (make-origin (delectus-node-identity)
                                      (osicat-posix:getpid)
                                      db-path))
                 (column-label (make-column-label))
                 (default-column (column-description :label (make-column-label)
                                                     :name "Item"
                                                     :order 10.0
                                                     :sort :null
                                                     :title :false
                                                     :subtitle :false
                                                     :deleted :false))
                 (default-column-descriptions (list default-column))
                 (default-column-label (getf default-column :|label|)))
            (db-ensure-columns-exist db default-column-descriptions)
            (db-insert-listname db :origin origin :timestamp (delectus-timestamp-now) :name listname)
            (db-insert-columns db :origin origin :timestamp (delectus-timestamp-now)
                               :column-descriptions default-column-descriptions)
            (db-insert-item db :origin origin :timestamp (delectus-timestamp-now)
                            :column-values [default-column-label nil])
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
;;; (delete-file $testlist)
;;; (create-delectus-file $testlist :listname "Test List" :listid (make-identity-string))
;;; (create-delectus-file $testlist :listname "Test List" :listid (make-identity-string) :create-default-userdata nil)
;;; (time (get-latest-items (pathname $testlist)))
