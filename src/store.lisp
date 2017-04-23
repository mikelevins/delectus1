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

(defparameter +system-column-labels+
  '("rowid" "deleted"))

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

(defmethod store-deleted-labels ((store store))
  (with-open-database (db (data-path store))
    (with-transaction db
      (execute-to-list db "select * from deleted_columns"))))

(defmethod visible-column-labels ((store store))
  (let* ((all-labels (store-column-labels store))
         (hidden-labels (append +system-column-labels+
                                (store-deleted-labels store))))
    (list-difference all-labels hidden-labels :test #'equalp)))

(defun build-sql-text-filter (filter-text cols)
  (reduce #'(lambda (l r)(concatenate 'string l r))
          (interpose " OR "
                     (loop for col in cols collect (format nil "\"~A\" LIKE \"%~A%\"" col filter-text)))))

;;; (format t "~%~%~A" (build-sql-text-filter "Fif" (visible-column-labels $store)))

(defmethod store-get-rows ((store store) &key (column-labels nil)(count-limit nil)(start-index 0)(include-deleted nil)(filter-text ""))
  (let* ((selector (if column-labels
                       (format nil " ~{~s~^, ~} " column-labels)
                     " * "))
         (limit-expr (if count-limit
                         (format nil " limit ~A " count-limit)
                       ""))
         (offset-expr (if count-limit
                          (format nil " offset ~A " start-index)
                        ""))
         (like-expr (if (and filter-text (not (equal "" filter-text)))
                        (let ((cols (or column-labels (visible-column-labels store))))
                          (build-sql-text-filter filter-text cols))
                      nil)))
    (with-open-database (db (data-path store))
      (with-transaction db
        (if include-deleted
            (if like-expr
                (execute-to-list db (format nil "select ~A from contents WHERE ~A ~A ~A"
                                            selector like-expr limit-expr offset-expr))
              (execute-to-list db (format nil "select ~A from contents ~A ~A"
                                          selector limit-expr offset-expr)))
          (if like-expr
              (execute-to-list db (format nil "select ~A from contents where deleted = 0 AND ~A ~A ~A"
                                          selector like-expr limit-expr offset-expr))
            (execute-to-list db (format nil "select ~A from contents where deleted = 0 ~A ~A"
                                          selector limit-expr offset-expr))))))))

(defmethod store-count-rows ((store store) &key (column-labels nil)(count-limit nil)(start-index 0)(include-deleted nil))
  (length (store-get-rows store
                          :column-labels column-labels
                          :count-limit count-limit
                          :start-index start-index
                          :include-deleted include-deleted)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (store-get-rows $store :column-labels '("Title") :count-limit 5 :start-index 200 :include-deleted t :filter-text "F")
;;; (time (store-count-rows $store :start-index 300 :count-limit 1000))
