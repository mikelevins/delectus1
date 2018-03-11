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
;;; *exported class*
;;;
;;; The type of objects that represent Delectus store files.

(defclass store ()
  ;; pathname of a SQLite3 file with appropriate Delectus tables
  ((data-path :accessor data-path :initform nil :initarg :data-path)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))

;;; ---------------------------------------------------------------------
;;; build-sql-text-filter (filter-text cols)
;;; ---------------------------------------------------------------------
;;; *private function*
;;;
;;; Builds and returns a SQL LIKE expression that tries to match a
;;; FILTER-STRING to each column in COLS.

(defun build-sql-text-filter (filter-text cols)
  (reduce #'(lambda (l r)(concatenate 'string l r))
          (delectus.utilities:interpose 
           " OR "
           (loop for col in cols collect (format nil "\"~A\" LIKE \"%~A%\"" col filter-text)))))

;;; ---------------------------------------------------------------------
;;; store-get-rows (store &key (column-labels nil)
;;;                            (count-limit nil)
;;;                            (start-index 0)
;;;                            (filter-text "")
;;;                            (sort-column "rowid")
;;;                            (sort-order :ascending)
;;; ---------------------------------------------------------------------
;;; *exported generic function*
;;;
;;; Returns a list of all rows of the `contents` table of STORE that
;;; match FILTER-TEXT or, if FILTER-TEXT is NIL, all rows. If
;;; COUNT-LIMIT is not NIL then it must be an integer; the result
;;; list's length is limited to COUNT-LIMIT, and the returned results
;;; begin at START-INDEX in the result set. Results are sorted by
;;; SORT-COLUMN. Their order, given by SORT-ORDER, may be :ascending
;;; or :descending.

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

;;; ---------------------------------------------------------------------
;;; store-get-column-labels (store)
;;; ---------------------------------------------------------------------
;;; *exported generic function*
;;;
;;; Returns the visible column labels of STORE in user-defined
;;; order. The invisible column `rowid` is not included. The column
;;; order is given by the rows in the `column_order` table.

(defmethod store-get-column-labels ((store store))
  (with-open-database (db (data-path store))
    (with-transaction db
      (mapcar #'first (execute-to-list db "select * from column_order")))))

;;; ---------------------------------------------------------------------
;;; store-count-rows (store &key (column-labels nil)
;;;                              (filter-text "")
;;; ---------------------------------------------------------------------
;;; *exported generic function*
;;;
;;; Returns a count of all matching rows in STORE. Matching rows are
;;; either all rows or, if FILTER-TEXT is not NIL, all rows for which
;;; the contents of some column in COLUMN-LABELS contains
;;; FILTER-TEXT. If FILTER-TEXT is empty then it matches any
;;; contents. If COLUMN-LABELS is NIL then all columns are searched.

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
;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (store-get-rows $store :column-labels '("Title") :count-limit 50  :filter-text "Cree")
;;; (time (store-count-rows $store))

