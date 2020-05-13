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
;;; creating a list file
;;; ---------------------------------------------------------------------

(defmethod create-delectus-file ((db-path pathname)
                                 &key
                                   (listname nil)
                                   (listid nil)
                                   (opid nil)
                                   (format +delectus-format-version+)
                                   (create-default-userdata t))
  (assert (not (probe-file db-path)) () "file exists: ~S" db-path)
  (assert (stringp listname) () "Expected a string :LISTNAME parameter, but found ~S" listname)
  (let* ((listid (or listid (make-identity-string))))
    (with-open-database (db db-path)
      (with-transaction db
        (db-create-delectus-table db listid format)
        ;; (db-create-listnames-table db)
        ;; (db-create-comments-table db)
        ;; (db-create-columns-table db)
        ;; (db-create-items-table db)
        ;; (db-create-items-origin-itemid-revision-index db)
        ;; (when create-default-userdata
        ;;   (let* ((opid (makeid))
        ;;          (default-column (column-description :id (make-identity-string)
        ;;                                                            :name "Item"
        ;;                                                            :order 10.0
        ;;                                                            :sort :null
        ;;                                                            :title :false
        ;;                                                            :subtitle :false
        ;;                                                            :deleted :false))
        ;;          (default-column-descriptions (list default-column))
        ;;          (default-column-id (getf default-column :|id|)))
        ;;     (db-ensure-columns-exist db default-column-descriptions)
        ;;     (db-insert-listname db :opid opid :timestamp (delectus-timestamp-now) :name listname)
        ;;     (db-insert-columns db :opid opid :timestamp (delectus-timestamp-now)
        ;;                        :column-descriptions default-column-descriptions)
        ;;     (db-insert-item db :opid opid :timestamp (delectus-timestamp-now)
        ;;                     :column-values [default-column-id nil])
        ;;     ))
        )))
  db-path)

(defmethod create-delectus-file ((db-path string)
                                 &key
                                   (listname nil)
                                   (listid nil)
                                   (opid nil)
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
