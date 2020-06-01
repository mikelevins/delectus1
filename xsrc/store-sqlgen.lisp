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
  (assert (identity-string? listid)() "Not a valid list identity: ~S" listid)
  (yield
   (insert-into :delectus
     (set= :listid listid
           :format format
           :created created
           :modified modified))))

;;; (sqlgen-init-delectus-table (makeid))

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
        (item_order :type 'real)
        (timestamp :type 'integer)
        (itemid :type 'blob)
        (deleted :type 'integer)))))

;;; (sqlgen-create-items-table)

;;; ---------------------------------------------------------------------
;;; getting next revision and order
;;; ---------------------------------------------------------------------

(defun sqlgen-get-next-revision (target)
  (cond
    ((member target ["listnames" "comments" "columns"] :test #'equal)
     (values (format nil "SELECT MAX(revision)+1 FROM `~A`" target)
             nil))
    ((identity? target)
     (yield
      (select ((:+ (:max :revision) 1))
        (from :items)
        (where (:= :itemid target)))))
    (t (error "Unrecognized target: ~S" target))))

;;; (sqlgen-get-next-revision "listnames")
;;; (sqlgen-get-next-revision "columns")
;;; (setf $id (makeid))
;;; (sqlgen-get-next-revision $id)

(defun sqlgen-get-next-item-order ()
  (yield
   (select ((:+ (:max :item_order) *item-order-interval*))
     (from :items))))

;;; (sqlgen-get-next-item-order)

;;; ---------------------------------------------------------------------
;;; inserting ops
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-listname-op (origin revision timestamp name-json)
  (yield
   (insert-into :listnames
     (set= :origin origin
           :revision revision
           :timestamp timestamp
           :name name-json))))


(defun sqlgen-insert-comment-op (origin revision timestamp comment-json)
  (yield
   (insert-into :comments
     (set= :origin origin
           :revision revision
           :timestamp timestamp
           :comment comment-json))))

(defun sqlgen-insert-columns-op (origin revision timestamp columns-data)
  (yield
   (insert-into :columns
     (set= :origin origin
           :revision revision
           :timestamp timestamp
           ))))
