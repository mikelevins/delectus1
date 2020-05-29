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
;;; revisions
;;; =====================================================================

;;; return the next revision number to use for the specified op target
(defmethod db-get-next-revision ((db sqlite-handle) (target string))
  (bind ((sql vals (sqlgen-get-next-revision target))
         (rev (apply 'execute-single db sql vals)))
    (if rev
        (1+ rev)
        0)))

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (with-open-database (db $testfile-path) (db-get-next-revision db "listname"))

;;; =====================================================================
;;; orders
;;; =====================================================================

(defmethod db-get-next-item-order ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-next-item-order))
         (order (apply 'execute-single db sql vals)))
    (if order
        (+ order *op-order-interval*)
        *minimum-op-order*)))

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (with-open-database (db $testfile-path) (db-get-next-item-order db))

;;; =====================================================================
;;; creating the list file
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
        (db-create-editlog-table db)

        (when create-default-userdata
          (let* ((origin (make-origin-string (process-identity) db-path))
                 (listname-order *minimum-op-order*)
                 (listname-revision (db-get-next-revision db "listname"))
                 (default-column (column-description :label (make-column-label)
                                                     :name "Item"
                                                     :order *minimum-column-order*
                                                     :sort :null
                                                     :title :false
                                                     :subtitle :false
                                                     :deleted :false))
                 (default-column-descriptions (list default-column))
                 (default-column-label (getf default-column :|label|)))
            (db-insert-listname-op db :origin origin :revision listname-revision :item-order listname-order
                                   :timestamp (delectus-timestamp-now) :listname listname)
            ;; (db-insert-columns db :origin origin :timestamp (delectus-timestamp-now)
            ;;                    :column-descriptions default-column-descriptions)
            ;; (db-insert-item db :origin origin :timestamp (delectus-timestamp-now)
            ;;                 :column-values [default-column-label nil])
            ))

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
                        :format format
                        :create-default-userdata create-default-userdata)
  db-path)

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (create-delectus-file $testfile-path :listname "Test List")
;;; (delete-file $testfile-path)
