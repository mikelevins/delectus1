;;;; ***********************************************************************
;;;;
;;;; Name:          store.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Delectus data model
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; system-column-labels
;;; ---------------------------------------------------------------------
;;; columns that every delectus contents table possesses, regardless
;;; of what columns a user supplies.

(defparameter +system-column-labels+ '("rowid"))

(defparameter +reserved-column-labels '("rowid" "oid" "_rowid_" "id"))

;;; ---------------------------------------------------------------------
;;; store
;;; ---------------------------------------------------------------------
;;; a class that represents data storage

;;; store
;;; ---------------------------------------------------------------------
(defclass store ()
  ;; pathname of a SQLite3 file with appropriate Delectus tables
  ((data-path :accessor data-path :initform nil :initarg :data-path)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))

(defmethod store-column-labels ((store store))
  (with-open-database (db (data-path store))
    (with-transaction db
      (append +system-column-labels+
              (mapcar #'first (execute-to-list db "select * from column_order"))))))

(defmethod visible-column-labels ((store store))
  (store-column-labels store))

(defun build-sql-text-filter (filter-text cols)
  (reduce #'(lambda (l r)(concatenate 'string l r))
          (interpose " OR "
                     (loop for col in cols collect (format nil "\"~A\" LIKE \"%~A%\"" col filter-text)))))

;;; (format t "~%~%~A" (build-sql-text-filter "Fif" (visible-column-labels $store)))

(defmethod store-get-rows ((store store) &key (column-labels nil)(count-limit nil)(start-index 0)(filter-text ""))
  (let* ((column-order (or column-labels (visible-column-labels store)))
         (selector (format nil " ~{~s~^, ~} " column-order))
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
                    (format nil "select ~A from contents WHERE ~A ~A ~A"
                            selector like-expr limit-expr offset-expr)
                  (format nil "select ~A from contents ~A ~A"
                          selector limit-expr offset-expr))))
    (with-open-database (db (data-path store))
      (with-transaction db
        (execute-to-list db query)))))

(defmethod store-count-all-rows ((store store))
  (with-open-database (db (data-path store))
    (with-transaction db
      (first (first (execute-to-list db (format nil "select Count(*) from contents")))))))

(defmethod store-count-rows ((store store) &key (column-labels nil)(filter-text ""))
  (let* ((column-order (or column-labels (visible-column-labels store)))
         (like-expr (if (and filter-text (not (equal "" filter-text)))
                        (build-sql-text-filter filter-text column-order)
                      nil))
         (query (if like-expr
                    (format nil "select count(*) from (select * from contents WHERE ~A )" like-expr)
                  (format nil "select count(*) from (select * from contents)"))))
    (with-open-database (db (data-path store))
      (with-transaction db
        (first (first (execute-to-list db query)))))))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (store-get-rows $store :column-labels '("Title") :count-limit 50  :filter-text "Fo")
;;; (time (store-count-rows $store))

