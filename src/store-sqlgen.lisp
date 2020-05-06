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
        (timestamp :type 'integer)
        (item :type 'integer)
        (deleted :type 'integer)))))

;;; (sqlgen-create-items-table)

;;; ---------------------------------------------------------------------
;;; creating the main items index
;;; ---------------------------------------------------------------------

(defun sqlgen-create-item-revision-origin-index ()
  (values "CREATE INDEX `idx_item_revision_origin` ON `items` (`item`, `revision`, `origin`)"
          nil))

;;; ---------------------------------------------------------------------
;;; next_revision and next_item
;;; ---------------------------------------------------------------------

(defun sqlgen-get-next-revision ()
  (yield
   (select :next_revision
     (from :delectus))))

;;; (sqlgen-get-next-revision)

(defun sqlgen-set-next-revision (rev)
  (values (format nil "UPDATE `delectus` SET `next_revision` = ~A" rev)
          nil))

(defun sqlgen-get-next-item ()
  (yield
   (select :next_item
     (from :delectus))))

(defun sqlgen-set-next-item (rev)
  (values (format nil "UPDATE `delectus` SET `next_item` = ~A" rev)
          nil))

;;; ---------------------------------------------------------------------
;;; adding columns
;;; ---------------------------------------------------------------------

(defun sqlgen-add-columns-userdata-column (column-label)
  (yield
   (alter-table :columns
     (add-column (delectus::as-keyword column-label) :type 'text))))

(defun sqlgen-add-items-userdata-column (column-label)
  (yield
   (alter-table :items
     (add-column (delectus::as-keyword column-label) :type 'text))))

;;; ---------------------------------------------------------------------
;;; inserting ops
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-listname (origin revision timestamp name)
  (yield
   (insert-into :listnames
     (set= :origin origin
           :revision revision
           :timestamp timestamp
           :name name))))

;;; (setf $origin (make-origin (process-identity) (pathname "/Users/mikel/Desktop/testlist.delectus2")))
;;; (sqlgen-insert-listname $origin 3 (now-utc) "Foobar")

(defun sqlgen-insert-columns (origin revision timestamp column-descriptions)
  (let* ((column-id-strings (mapcar 'column-description-id column-descriptions))
         (column-labels (mapcar 'identity->column-label column-id-strings))
         (column-ids (mapcar 'as-keyword column-labels))
         (column-json-objects (mapcar 'to-json column-descriptions))
         (parameter-names (append [:|origin| :|revision| :|timestamp|] column-ids))
         (parameter-names-string (format nil "~{`~A`~^, ~}" parameter-names))
         (parameter-values (append [origin revision timestamp] column-json-objects))
         (parameter-placeholders (format nil "~{~A~^, ~}" (mapcar (constantly "?") parameter-names)))
         (sql (format nil "INSERT INTO `columns` (~A) VALUES (~A)"
                      parameter-names-string parameter-placeholders)))
    (values sql parameter-values)))


;;; (setf $origin (make-origin (process-identity) (pathname "/Users/mikel/Desktop/testlist.delectus2")))
;;; (sqlgen-insert-columns $origin 5 (now-utc) (list (column-description :id (make-identity-string) :name "Item")))


(defun sqlgen-insert-item (origin revision timestamp item deleted column-values)
  (let* ((column-id-strings (get-keys column-values))
         (column-labels (mapcar 'identity->column-label column-id-strings))
         (column-ids (mapcar 'as-keyword column-labels))
         (column-values (get-values column-values))
         (parameter-names (append [:|origin| :|revision| :|timestamp| :|item| :|deleted|]
                                  column-ids))
         (parameter-names-string (format nil "~{`~A`~^, ~}" parameter-names))
         (parameter-values (append [origin revision timestamp item deleted] column-values))
         (parameter-placeholders (format nil "~{~A~^, ~}" (mapcar (constantly "?") parameter-names)))
         (sql (format nil "INSERT INTO `items` (~A) VALUES (~A)"
                      parameter-names-string parameter-placeholders)))
    (values sql parameter-values)))


;;; (setf $origin (make-origin (process-identity) (pathname "/Users/mikel/Desktop/testlist.delectus2")))
;;; (sqlgen-insert-item $origin 5 (now-utc) 3 nil {(make-identity-string) 101})
