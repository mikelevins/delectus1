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
       ((listid :type 'string)
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
        (origin :type 'integer)
        (timestamp :type 'integer)
        (name :type 'text)))))

;;; (sqlgen-create-listnames-table)


;;; 'comments' table
;;; ----------------

(defun sqlgen-create-comments-table ()
  (yield
   (create-table :comments
       ((revision :type 'integer)
        (origin :type 'integer)
        (timestamp :type 'integer)
        (comment :type 'text)))))

;;; (sqlgen-create-comments-table)


;;; 'columns' table
;;; ----------------

(defun sqlgen-create-columns-table ()
  (yield
   (create-table :columns
       ((revision :type 'integer)
        (origin :type 'integer)
        (timestamp :type 'integer)))))

;;; (sqlgen-create-columns-table)

;;; 'items' table
;;; ----------------

(defun sqlgen-create-items-table ()
  (yield
   (create-table :items
       ((revision :type 'integer)
        (origin :type 'integer)
        (timestamp :type 'integer)
        (itemid :type 'integer)
        (deleted :type 'integer)))))

;;; (sqlgen-create-items-table)

;;; ---------------------------------------------------------------------
;;; the next revision
;;; ---------------------------------------------------------------------

(defun sqlgen-get-next-revision ()
  (yield
   (select :next_revision
     (from :delectus))))

;;; (sqlgen-get-next-revision)

(defun sqlgen-set-next-revision (rev)
  (values (format nil "UPDATE `delectus` SET `next_revision` = ~A" rev)
          nil))
;;; ---------------------------------------------------------------------
;;; the next itemid
;;; ---------------------------------------------------------------------

(defun sqlgen-get-next-itemid ()
  (yield
   (select :next_itemid
     (from :delectus))))

;;; (sqlgen-get-next-itemid)

(defun sqlgen-set-next-itemid (it)
  (values (format nil "UPDATE `delectus` SET `next_itemid` = ~A" it)
          nil))

;;; ---------------------------------------------------------------------
;;; adding columns
;;; ---------------------------------------------------------------------

(defun sqlgen-add-columns-userdata-column (column-label)
  (yield
   (alter-table :columns
     (add-column (as-keyword column-label) :type 'text))))

(defun sqlgen-add-items-userdata-column (column-label)
  (yield
   (alter-table :items
     (add-column (as-keyword column-label) :type 'text))))

;;; ---------------------------------------------------------------------
;;; inserting ops
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-listname (revision origin timestamp name)
  (yield
   (insert-into :listnames
     (set= :revision revision
           :origin origin
           :timestamp timestamp
           :name name))))

;;; (setf $dbpath (pathname "/Users/mikel/Desktop/testlist.delectus2"))
;;; (setf $origin (make-origin (delectus-node-identity) (osicat-posix:getpid) $dbpath))
;;; (sqlgen-insert-listname 3 $origin (delectus-timestamp-now) "Foobar")

(defun sqlgen-insert-columns (revision origin timestamp column-descriptions)
  (let* ((column-labels (mapcar 'column-description-label column-descriptions))
         (column-ids (mapcar 'as-keyword column-labels))
         (column-json-objects (mapcar 'jonathan:to-json column-descriptions))
         (parameter-names (append [:|revision| :|origin| :|timestamp|] column-ids))
         (parameter-names-string (format nil "~{`~A`~^, ~}" parameter-names))
         (parameter-values (append [revision origin timestamp] column-json-objects))
         (parameter-placeholders (format nil "~{~A~^, ~}" (mapcar (constantly "?") parameter-names)))
         (sql (format nil "INSERT INTO `columns` (~A) VALUES (~A)"
                      parameter-names-string parameter-placeholders)))
    (values sql parameter-values)))

;;; (setf $dbpath (pathname "/Users/mikel/Desktop/testlist.delectus2"))
;;; (setf $origin (make-origin (delectus-node-identity) (osicat-posix:getpid) $dbpath))
;;; (setf $column-descriptions (list (column-description :label (make-column-label) :name "Item")))
;;; (sqlgen-insert-columns 5 $origin (delectus-timestamp-now) $column-descriptions)

(defun sqlgen-insert-item (revision origin timestamp itemid deleted column-values)
  (let* ((column-labels (get-keys column-values))
         (column-ids (mapcar 'as-keyword column-labels))
         (column-values (get-values column-values))
         (parameter-names (append [:|revision| :|origin| :|timestamp| :|itemid| :|deleted|]
                                  column-ids))
         (parameter-names-string (format nil "~{`~A`~^, ~}" parameter-names))
         (parameter-values (append [revision origin timestamp itemid deleted] column-values))
         (parameter-placeholders (format nil "~{~A~^, ~}" (mapcar (constantly "?") parameter-names)))
         (sql (format nil "INSERT INTO `items` (~A) VALUES (~A)"
                      parameter-names-string parameter-placeholders)))
    (values sql parameter-values)))

;;; (setf $dbpath (pathname "/Users/mikel/Desktop/testlist.delectus2"))
;;; (setf $origin (make-origin (delectus-node-identity) (osicat-posix:getpid) $dbpath))
;;; (setf $column-values [(make-column-label) "Foo"])
;;; (sqlgen-insert-item 5 $origin (delectus-timestamp-now) 1 nil $column-values)

