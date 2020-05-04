;;;; ***********************************************************************
;;;;
;;;; Name:          store-sqlgen.lisp
;;;; Project:       delectus 2
;;;; Purpose:       functions for generating SQL statements
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; creating Delectus tables
;;; ---------------------------------------------------------------------

;;; 'delectus' table
;;; ----------------

(defun sqlgen-create-delectus-table ()
  (yield
   (create-table :delectus
       ((listid :type 'text)
        (format :type 'text)
        (modified :type 'integer)
        (next_revision :type 'integer)
        (next_item :type 'integer)))))

;;; (sqlgen-create-delectus-table)

(defun sqlgen-init-delectus-table (list-identity format-string
                                   &key
                                     (next-revision 0)
                                     (next-item 0))
  (let ((timestamp (now-utc)))
    (yield
     (insert-into :delectus
       (set= :listid list-identity
             :format format-string
             :modified timestamp
             :next_revision next-revision
             :next_item next-item)))))

;;; 'comments' table
;;; ----------------

(defun sqlgen-create-comments-table ()
  (yield
   (create-table :comments
       ((origin :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)
        (comment :type 'text)))))

;;; (sqlgen-create-comments-table)


;;; 'listnames' table
;;; ----------------

(defun sqlgen-create-listnames-table ()
  (yield
   (create-table :listnames
       ((origin :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)
        (name :type 'text)))))

;;; (sqlgen-create-listnames-table)


;;; 'columns' table
;;; ----------------

(defun sqlgen-create-columns-table ()
  (yield
   (create-table :columns
       ((origin :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)))))

;;; (sqlgen-create-columns-table)


;;; 'items' table
;;; ----------------

(defun sqlgen-create-items-table ()
  (yield
   (create-table :items
       ((origin :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)
        (item :type 'integer)
        (deleted :type 'integer)))))

;;; (sqlgen-create-items-table)

;; (yield
;;  (alter-table :columns
;;    (add-column (delectus::as-keyword (delectus::make-column-label)) :type 'text)))
