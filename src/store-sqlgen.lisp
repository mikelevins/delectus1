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

;;; 'delectus' table
;;; ----------------

(defun sqlgen-create-delectus-table ()
  (yield
   (create-table :delectus
       ((listid :type 'blob)
        (format :type 'text)
        (created :type 'integer)
        (modified :type 'integer)
        (next_revision :type 'integer)
        (next_itemid :type 'integer)))))

;;; (sqlgen-create-delectus-table)

(defun sqlgen-init-delectus-table (list-identity
                                   &key
                                     (format +delectus-format-version+)
                                     (created (delectus-timestamp-now))
                                     (modified (delectus-timestamp-now))
                                     (next-revision 0)
                                     (next-itemid 0))
  (yield
   (insert-into :delectus
     (set= :listid list-identity
           :format format
           :created created
           :modified modified
           :next_revision next-revision
           :next_itemid next-itemid))))

;;; (sqlgen-init-delectus-table (makeid))


;;; 'listnames' table
;;; ----------------

(defun sqlgen-create-listnames-table ()
  (yield
   (create-table :listnames
       ((revision :type 'integer)
        (origin :type 'blob)
        (timestamp :type 'integer)
        (name :type 'text)))))

;;; (sqlgen-create-listnames-table)


;;; 'comments' table
;;; ----------------

(defun sqlgen-create-comments-table ()
  (yield
   (create-table :comments
       ((revision :type 'integer)
        (origin :type 'blob)
        (timestamp :type 'integer)
        (comment :type 'text)))))

;;; (sqlgen-create-comments-table)


;;; 'columns' table
;;; ----------------

(defun sqlgen-create-columns-table ()
  (yield
   (create-table :columns
       ((revision :type 'integer)
        (origin :type 'blob)
        (timestamp :type 'integer)))))

;;; (sqlgen-create-columns-table)

;;; 'items' table
;;; ----------------

(defun sqlgen-create-items-table ()
  (yield
   (create-table :items
       ((revision :type 'integer)
        (origin :type 'blob)
        (timestamp :type 'integer)
        (itemid :type 'blob)
        (deleted :type 'integer)))))

;;; (sqlgen-create-items-table)
