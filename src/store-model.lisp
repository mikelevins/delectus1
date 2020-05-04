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

;;; (with-open-database (db ":memory:") (db-create-delectus-table db (make-identity-string)))

;;; ---------------------------------------------------------------------
;;; creating a list file
;;; ---------------------------------------------------------------------

(defmethod create-delectus-file ((db-path pathname)
                                 &key
                                   (listname nil)
                                   (listid nil)
                                   (format +delectus-format-version+)
                                   (create-default-userdata t))
  (assert (not (probe-file db-path)) () "file exists: ~S" db-path)
  (assert (stringp listname) () "Expected a string :LISTNAME parameter, but found ~S" listname)
  (let ((listid (or listid (make-identity-string))))
    (with-open-database (db db-path)
      (with-transaction db
        (db-create-delectus-table db listid)
        ;; (db-create-comments-table db )
        ;; (db-create-listnames-table db )
        ;; (db-create-columns-table db )
        ;; (db-create-items-table db )
        ;; (db-create-item-revision-origin-index db)
        )))
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

;;; (create-delectus-file "/Users/mikel/Desktop/testlist.delectus2" :listname "Test List" :listid (make-identity-string))
;;; (delete-file "/Users/mikel/Desktop/testlist.delectus2")
