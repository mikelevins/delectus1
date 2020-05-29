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
;;; creating tables
;;; ---------------------------------------------------------------------

;;; 'delectus' table
;;; ----------------

(defun sqlgen-create-delectus-table ()
  (yield
   (create-table :delectus
       ((listid :type 'text)
        (format :type 'text)
        (created :type 'integer)
        (modified :type 'integer)))))

;;; (sqlgen-create-delectus-table)

(defun sqlgen-init-delectus-table (listid
                                   &key
                                     (format +delectus-format-version+)
                                     (created (delectus-timestamp-now))
                                     (modified (delectus-timestamp-now)))
  (yield
   (insert-into :delectus
     (set= :listid listid
           :format format
           :created created
           :modified modified))))

;;; (sqlgen-init-delectus-table (makeid))


;;; 'editlog' table
;;; ----------------

(defun sqlgen-create-editlog-table ()
  (yield
   (create-table :editlog
       ((|`target`| :type 'text)
        (|`origin`| :type 'text)
        (|`revision`| :type 'integer)
        (|`order`| :type 'real)
        (|`timestamp`| :type 'integer)
        (|`data`| :type 'text)))))

;;; (sqlgen-create-editlog-table)


;;; ---------------------------------------------------------------------
;;; getting next revision and order
;;; ---------------------------------------------------------------------

(defun sqlgen-get-next-order ()
  (yield
   (select ((:max :|`order`|))
     (from :editlog))))

;;; (sqlgen-get-next-order)

(defun sqlgen-get-next-revision (target)
  (yield
   (select ((:max :|`revision`|))
     (where (:= :|`target`| target))
     (from :editlog))))

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (sqlgen-get-next-revision "listname")
