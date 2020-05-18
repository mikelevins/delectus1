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

;;; =====================================================================
;;; naming conventions:
;;; =====================================================================
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
;;; the list file
;;; =====================================================================

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
                        :format format
                        :create-default-userdata create-default-userdata)
  db-path)
