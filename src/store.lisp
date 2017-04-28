;;;; ***********************************************************************
;;;;
;;;; Name:          store.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Delectus data model
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.store)

;;; ---------------------------------------------------------------------
;;; store
;;; ---------------------------------------------------------------------
;;; a class that represents data storage

;;; store
;;; ---------------------------------------------------------------------
;;; EXPORTED CLASS
;;; The type of objects that represent Delectus store files.

(defclass store ()
  ;; pathname of a SQLite3 file with appropriate Delectus tables
  ((data-path :accessor data-path :initform nil :initarg :data-path)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))

;;; build-sql-text-filter (filter-text cols)
;;; ---------------------------------------------------------------------
;;; PRIVATE FUNCTION
;;; returns a SQL string that selects columns given by COLS and matching
;;; the text given by FILTER-TEXT. A row matches FILTER-TEXT if the
;;; string in FILTER-TEXT appears anywhere in the row. 

(defun build-sql-text-filter (filter-text cols)
  (reduce #'(lambda (l r)(concatenate 'string l r))
          (delectus.utilities:interpose 
           " OR "
           (loop for col in cols collect (format nil "\"~A\" LIKE \"%~A%\"" col filter-text)))))

;;; store-get-rows (store &key (column-labels nil)
;;;                            (count-limit nil)
;;;                            (start-index 0)
;;;                            (filter-text "")
;;;                            (sort-column "rowid")
;;;                            (sort-order :ascending)
;;; ---------------------------------------------------------------------
;;; EXPORTED GENERIC FUNCTION
;;; returns all columns matching COLUMN-LABELS of all rows from STORE
;;; that match FILTER-TEXT. A row matches FILTER-TEXT if the string in
;;; FILTER-TEXT appears anywhere in the row. Results are limited to
;;; COUNT-LIMIT rows, beginning with the row index START-INDEX. If
;;; COUNT-LIMIT is NIL then all matching rows are returned. Rows are
;;; sorted by SORT-COLUMN in the order given by SORT-ORDER, which can
;;; be :ASCENDING or :DESCENDING.

(defmethod store-get-rows ((store store) &key 
                           (column-labels nil)
                           (count-limit nil)
                           (start-index 0)
                           (filter-text "")
                           (sort-column "rowid")
                           (sort-order :ascending))
  (let* ((column-order (or column-labels (store-get-column-labels store)))
         (selector (format nil " ~{~s~^, ~} " column-order))
         (order-by-expr (format nil " ORDER BY \"~A\" ~A" 
                                sort-column
                                (if (equal :descending sort-order)
                                    "DESC" "ASC")))
         (limit-expr (if count-limit
                         (format nil " limit ~A " count-limit)
                       ""))
         (offset-expr (if count-limit
                          (format nil " offset ~A " start-index)
                        ""))
         (like-expr (if (and filter-text (not (equal "" filter-text)))
                        (build-sql-text-filter filter-text column-order)
                      nil))
         (query (if like-expr
                    (format nil "select ~A from contents WHERE ~A ~A ~A ~A"
                            selector like-expr order-by-expr limit-expr offset-expr)
                  (format nil "select ~A from contents ~A ~A ~A"
                          selector order-by-expr limit-expr offset-expr))))
    (with-open-database (db (data-path store))
      (with-transaction db
        (execute-to-list db query)))))

;;; store-get-column-labels (store)
;;; ---------------------------------------------------------------------
;;; EXPORTED GENERIC FUNCTION
;;;
;;; returns the visible column labels of the store in user-defined
;;; order. The invisible column "rowid" is not included. The column
;;; order is determined by rows in the column_order table.

(defmethod store-get-column-labels ((store store))
  (with-open-database (db (data-path store))
    (with-transaction db
      (mapcar #'first (execute-to-list db "select * from column_order")))))

;;; store-count-rows (store &key (column-labels nil)
;;;                              (filter-text "")
;;; ---------------------------------------------------------------------
;;; EXPORTED GENERIC FUNCTION
;;;
;;; returns a count of all columns matching COLUMN-LABELS of all rows
;;; from STORE that match FILTER-TEXT. A row matches FILTER-TEXT if
;;; the string in FILTER-TEXT appears anywhere in the row. 

(defmethod store-count-rows ((store store) &key (column-labels nil)(filter-text ""))
  (let* ((column-order (or column-labels (store-get-column-labels store)))
         (like-expr (if (and filter-text (not (equal "" filter-text)))
                        (build-sql-text-filter filter-text column-order)
                      nil))
         (query (if like-expr
                    (format nil "select count(*) from (select * from contents WHERE ~A )" like-expr)
                  (format nil "select count(*) from (select * from contents)"))))
    (with-open-database (db (data-path store))
      (with-transaction db
        (first (first (execute-to-list db query)))))))

;;; TEST CODE
;;; ---------------------------------------------------------------------

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (store-get-rows $store :column-labels '("Title") :count-limit 50  :filter-text "Fo")
;;; (time (store-count-rows $store))

