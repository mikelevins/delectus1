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

(defun sqlgen-get-next-item-order ()
  (yield
   (select ((:max :|`order`|))
     (from :editlog))))

;;; (sqlgen-get-next-item-order)

(defun sqlgen-get-next-revision (target)
  (yield
   (select ((:max :|`revision`|))
     (where (:= :|`target`| target))
     (from :editlog))))

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (sqlgen-get-next-revision "listname")

;;; ---------------------------------------------------------------------
;;; inserting ops
;;; ---------------------------------------------------------------------

;;; listname op
;;; -----------

(defun sqlgen-insert-listname-op (origin revision order timestamp name)
  (yield
   (insert-into :editlog
     (set= :|`target`| "listname"
           :|`origin`| origin
           :|`revision`| revision
           :|`order`| order
           :|`timestamp`| timestamp
           :|`data`| name))))

;;; (sqlgen-insert-listname-op (make-origin-string (process-identity) (path "~/.emacs")) 0 100.0 (delectus-timestamp-now) "Test")

;;; comment op
;;; -----------

(defun sqlgen-insert-comment-op (origin revision order timestamp comment)
  (yield
   (insert-into :editlog
     (set= :|`target`| "comment"
           :|`origin`| origin
           :|`revision`| revision
           :|`order`| order
           :|`timestamp`| timestamp
           :|`data`| comment))))

;;; (sqlgen-insert-comment-op (make-origin-string (process-identity) (path "~/.emacs")) 0 100.0 (delectus-timestamp-now) "A generic test list.")

;;; columns op
;;; -----------

(defun sqlgen-insert-columns-op (origin revision order timestamp columns-data)
  (yield
   (insert-into :editlog
     (set= :|`target`| "columns"
           :|`origin`| origin
           :|`revision`| revision
           :|`order`| order
           :|`timestamp`| timestamp
           :|`data`| columns-data))))

;;; (sqlgen-insert-columns-op (make-origin-string (process-identity) (path "~/.emacs")) 0 200.0 (delectus-timestamp-now) (column-descriptions->data [(column-description :label (make-column-label) :name "Item")]))

;;; item op
;;; -----------

(defun sqlgen-insert-item-op (origin revision order timestamp data)
  (yield
   (insert-into :editlog
     (set= :|`target`| "item"
           :|`origin`| origin
           :|`revision`| revision
           :|`order`| order
           :|`timestamp`| timestamp
           :|`data`| data))))

;;; (sqlgen-insert-item-op (make-origin-string (process-identity) (path "~/.emacs")) 0 200.0 (delectus-timestamp-now) (jonathan:to-json (jonathan:to-json [(as-keyword (make-column-label)) ""])))
