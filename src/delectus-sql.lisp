;;;; ***********************************************************************
;;;;
;;;; Name:          delectus-sql.lisp
;;;; Project:       delectus 2
;;;; Purpose:       model-specific sql constructors
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; the sql templates path
;;; ---------------------------------------------------------------------

(defun sql-template-path (subpath)
  (merge-pathnames subpath *sql-templates-pathname*))

;;; (sql-template-path "create-delectus-table.tmpl.sql")

;;; ---------------------------------------------------------------------
;;; sql-create-delectus-table
;;; ---------------------------------------------------------------------

(defun sql-create-delectus-table ()
  (values
   (process-template (sql-template-path "create-delectus-table.tmpl.sql"))
   nil))

;;; (sql-create-delectus-table)

;;; ---------------------------------------------------------------------
;;; sql-populate-delectus-table
;;; ---------------------------------------------------------------------

(defun sql-populate-delectus-table (id origin format next-revision)
  (values
   (process-template (sql-template-path "populate-delectus-table.tmpl.sql"))
   (list id origin format next-revision)))

;;; (sql-populate-delectus-table (delectus::makeid) delectus::*origin* delectus::+delectus-format-version+ 3)

;;; ---------------------------------------------------------------------
;;; sql-create-list_data-table
;;; ---------------------------------------------------------------------

(defun sql-create-list_data-table ()
  (values
   (process-template (sql-template-path "create-list_data-table.tmpl.sql"))
   nil))

;;; (sql-create-list_data-table)

;;; ---------------------------------------------------------------------
;;; sql-add-userdata-column
;;; ---------------------------------------------------------------------

(defun sql-add-userdata-column (label type)
  (values
   (process-template (sql-template-path "add-userdata-column.tmpl.sql")
                     [:column-label label :column-type type])
   nil))

;;; (sql-add-userdata-column (delectus::makeid) "TEXT")

;;; ---------------------------------------------------------------------
;;; sql-assert-op
;;; ---------------------------------------------------------------------

(defun sql-assert-op (optype opid origin revision timestamp item name deleted peer &key column-data)
  (let* ((column-ids (mapcar #'car column-data))
         (column-values (mapcar #'cdr column-data))
         (insert-args (append `("optype" "opid" "origin" "revision" "timestamp"
                                         "item" "name" "deleted" "peer" ,@column-ids)))
         (insert-arg-strings (mapcar (lambda (a)(format nil "`~A`" a))
                                     insert-args))
         (insert-arg-placeholders (mapcar (constantly "?") insert-args))
         (insert-args-text (delectus::join-strings ", " insert-arg-strings))
         (insert-placeholders-text (delectus::join-strings ", " insert-arg-placeholders))
         (sql (format nil "INSERT INTO `list_data` (~A) VALUES (~A)"
                      insert-args-text insert-placeholders-text)))
    (values sql
            `(,optype ,opid ,origin ,revision ,timestamp ,item ,name ,deleted ,peer ,@column-values))))

;;; (sql-assert-op "item" (makeid) *origin* 2 (now-timestamp) (makeid) nil nil nil :column-data `((,(makeid) . 1)))

;;; ---------------------------------------------------------------------
;;; sql-get-column-attributes
;;; ---------------------------------------------------------------------

(defun sql-get-column-attributes ()
  (values "PRAGMA table_info(list_data);"
          nil))

;;; ---------------------------------------------------------------------
;;; sql-get-column-values
;;; ---------------------------------------------------------------------

(defun sql-get-column-values ()
  )

;;; ---------------------------------------------------------------------
;;; sql-get-latest-listname-op
;;; ---------------------------------------------------------------------

(defun sql-get-latest-listname-op ()
  (values
   (process-template (sql-template-path "get-latest-listname.tmpl.sql"))
   nil))

;;; (sql-get-latest-listname-op)

;;; ---------------------------------------------------------------------
;;; sql-get-latest-columns-op
;;; ---------------------------------------------------------------------

(defun sql-get-latest-columns-op ()
  (values
   (process-template (sql-template-path "get-latest-columns.tmpl.sql"))
   nil))

;;; (sql-get-latest-columns-op)


;;; ---------------------------------------------------------------------
;;; sql-get-latest-item-ops
;;; ---------------------------------------------------------------------


(defun sql-get-latest-item-ops ()
  (values
   (process-template (sql-template-path "get-latest-items.tmpl.sql"))
   nil))

;;; (sql-get-latest-item-ops)

