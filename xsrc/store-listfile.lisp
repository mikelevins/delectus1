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
;;; method for "listnames", "comments", "columns"
(defmethod db-get-next-revision ((db sqlite-handle) (target string))
  (bind ((sql vals (sqlgen-get-next-revision target))
         (rev (apply 'execute-single db sql vals)))
    (or rev 0)))

;;; method for itemids
(defmethod db-get-next-revision ((db sqlite-handle) (target vector))
  (bind ((sql vals (sqlgen-get-next-revision target))
         (rev (apply 'execute-single db sql vals)))
    (or rev 0)))

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (with-open-database (db $testfile-path) (db-get-next-revision db "listnames"))

;;; =====================================================================
;;; orders
;;; =====================================================================

(defmethod db-get-next-item-order ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-next-item-order))
         (order (apply 'execute-single db sql vals)))
    (or order *minimum-item-order*)))

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (with-open-database (db $testfile-path) (db-get-next-item-order db))

;;; =====================================================================
;;; columns data
;;; =====================================================================

;;; convert a list of column descriptons to JSON columns data

(defun column-descriptions->data (column-descriptions)
  (assert (every #'column-description? column-descriptions)()
          "Invalid column description found in ~S" column-descriptions)
  (let* ((clabels (mapcar #'column-description-label column-descriptions))
         (ckeys (mapcar #'as-keyword clabels))
         (cols-plist (interleave ckeys column-descriptions)))
    (jonathan:to-json cols-plist)))

;;; (column-descriptions->data [(column-description :label (make-column-label) :name "Item")])

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
        (db-create-listnames-table db)
        (db-create-comments-table db)
        (db-create-columns-table db)
        (db-create-items-table db)

        (when create-default-userdata
          (let* ((origin (make-origin (process-identity) db-path))
                 ;; used twice: in the columns op and in the item op
                 (default-column (column-description :label (make-column-label)
                                                     :name "Item"
                                                     :order *minimum-column-order*
                                                     :sort :null
                                                     :title :false
                                                     :subtitle :false
                                                     :deleted :false))
                 (default-column-descriptions (list default-column))
                 (default-column-label (column-description-label default-column)))
            ;; insert listname op
            (let* ((listname-order *minimum-item-order*)
                   (listname-revision (db-get-next-revision db "listnames")))
              (db-insert-listname-op db :origin origin :revision listname-revision :item-order listname-order
                                     :timestamp (delectus-timestamp-now) :listname listname))
            ;; insert comment op
            (let* ((comment-revision (db-get-next-revision db "comments"))
                   (comment-text "A Delectus List"))
              (db-insert-comment-op db :origin origin :revision comment-revision
                                    :timestamp (delectus-timestamp-now) :comment comment-text))
            ;; (let* ((columns-order (db-get-next-item-order db))
            ;;        (columns-revision (db-get-next-revision db "columns"))
            ;;        (columns-data (column-descriptions->data default-column-descriptions)))
            ;;   (db-insert-columns-op db :origin origin :revision columns-revision
            ;;                         :timestamp (delectus-timestamp-now)
            ;;                         :columns columns-data))
            ;; (db-insert-item db :origin origin :timestamp (delectus-timestamp-now)
            ;;                 :column-values [default-column-label nil])
            ;; (let* ((item-target (make-identity-string))
            ;;        (item-order (db-get-next-item-order db))
            ;;        (item-revision (db-get-next-revision db item-target))
            ;;        (item-data (jonathan:to-json [(as-keyword default-column-label)
            ;;                                      ""])))
            ;;   (db-insert-item-op db :origin origin :revision item-revision
            ;;                      :timestamp (delectus-timestamp-now)
            ;;                      :data item-data))

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

;;; (with-open-database (db $testfile-path)(db-get-next-revision db "listnames"))
;;; (with-open-database (db $testfile-path)(db-get-next-revision db "comments"))
;;; (with-open-database (db $testfile-path)(db-get-next-revision db "columns"))
;;; (with-open-database (db $testfile-path)(db-get-next-revision db (makeid)))
