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

;;; 'identities' table
;;; ----------------

(defun sqlgen-create-identities-table ()
  (yield
   (create-table :identities
       ((iref :type 'integer)
        (identity :type 'blob)))))

;;; (sqlgen-create-identities-table)

;;; 'delectus' table
;;; ----------------

(defun sqlgen-create-delectus-table ()
  (yield
   (create-table :delectus
       ((listid :type 'text)
        (fileid :type 'text)
        (format :type 'text)
        (modified :type 'integer)
        (next_revision :type 'integer)))))

;;; (sqlgen-create-delectus-table)

(defun sqlgen-init-delectus-table (list-identity format-string
                                   &key
                                     (fileid nil)
                                     (next-revision 0))
  (let ((timestamp (delectus-timestamp-now))
        (fileid (or fileid (make-identity-string))))
    (yield
     (insert-into :delectus
       (set= :listid list-identity
             :fileid fileid
             :format format-string
             :modified timestamp
             :next_revision next-revision)))))

;;; 'listnames' table
;;; ----------------

(defun sqlgen-create-listnames-table ()
  (yield
   (create-table :listnames
       ((opid :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)
        (name :type 'text)))))

;;; (sqlgen-create-listnames-table)

;;; 'comments' table
;;; ----------------

(defun sqlgen-create-comments-table ()
  (yield
   (create-table :comments
       ((opid :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)
        (comment :type 'text)))))

;;; (sqlgen-create-comments-table)

;;; 'columns' table
;;; ----------------

(defun sqlgen-create-columns-table ()
  (yield
   (create-table :columns
       ((opid :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)))))

;;; (sqlgen-create-columns-table)


;;; 'items' table
;;; ----------------

(defun sqlgen-create-items-table ()
  (yield
   (create-table :items
       ((opid :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)
        (itemid :type 'blob)
        (deleted :type 'integer)))))

;;; (sqlgen-create-items-table)

;;; ---------------------------------------------------------------------
;;; creating the main items index
;;; ---------------------------------------------------------------------
;;; the index used to fetch the latest versions of each item

(defun sqlgen-create-items-itemid-revision-opid-index ()
  (values "CREATE INDEX idx_items_itemid_revision_opid on `items` (`itemid`, `revision` DESC, `opid`)"
          nil))

;;; ---------------------------------------------------------------------
;;; next_revision
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
;;; registering and finding identities
;;; ---------------------------------------------------------------------

(defun sqlgen-identity-to-iref (identity)
  (yield
   (select :iref
     (from :identities)
     (where (:= :identity identity)))))

;;; (sqlgen-identity-to-iref (makeid))

(defun sqlgen-iref-to-identity (iref)
  (yield
   (select :identity
     (from :identities)
     (where (:= :item iref)))))

;;; (sqlgen-iref-to-identity 0)

(defun sqlgen-register-identity (iref identity)
  (values (format nil
                  "INSERT INTO identities(iref,identity) VALUES (?,?) 
WHERE NOT EXISTS(SELECT 1 FROM identities WHERE iref = ~A AND identity = ~A to insert')"
                  iref identity)
          nil))

;;; (sqlgen-register-identity 1 (makeid))

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

(defun sqlgen-insert-listname (opid revision timestamp name)
  (yield
   (insert-into :listnames
     (set= :opid opid
           :revision revision
           :timestamp timestamp
           :name name))))

;;; (setf $opid (makeid))
;;; (sqlgen-insert-listname $opid 3 (delectus-timestamp-now) "Foobar")

(defun sqlgen-insert-comment (opid revision timestamp comment)
  (yield
   (insert-into :comments
     (set= :opid opid
           :revision revision
           :timestamp timestamp
           :comment comment))))

;;; (setf $opid (makeid))
;;; (sqlgen-insert-comment $opid 3 (delectus-timestamp-now) "foo bar baz...")

(defun sqlgen-insert-columns (opid revision timestamp column-descriptions)
  (let* ((column-id-strings (mapcar 'column-description-id column-descriptions))
         (column-labels (mapcar 'identity->column-label column-id-strings))
         (column-ids (mapcar 'as-keyword column-labels))
         (column-json-objects (mapcar 'jonathan:to-json column-descriptions))
         (parameter-names (append [:|opid| :|revision| :|timestamp|] column-ids))
         (parameter-names-string (format nil "~{`~A`~^, ~}" parameter-names))
         (parameter-values (append [opid revision timestamp] column-json-objects))
         (parameter-placeholders (format nil "~{~A~^, ~}" (mapcar (constantly "?") parameter-names)))
         (sql (format nil "INSERT INTO `columns` (~A) VALUES (~A)"
                      parameter-names-string parameter-placeholders)))
    (values sql parameter-values)))


;;; (setf $opid (makeid))
;;; (sqlgen-insert-columns $opid 5 (delectus-timestamp-now) (list (column-description :id (make-identity-string) :name "Item")))


(defun sqlgen-insert-item (opid revision timestamp itemid deleted column-values)
  (let* ((column-id-strings (get-keys column-values))
         (column-labels (mapcar 'identity->column-label column-id-strings))
         (column-ids (mapcar 'as-keyword column-labels))
         (column-values (get-values column-values))
         (parameter-names (append [:|opid| :|revision| :|timestamp| :|itemid| :|deleted|]
                                  column-ids))
         (parameter-names-string (format nil "~{`~A`~^, ~}" parameter-names))
         (parameter-values (append [opid revision timestamp itemid deleted] column-values))
         (parameter-placeholders (format nil "~{~A~^, ~}" (mapcar (constantly "?") parameter-names)))
         (sql (format nil "INSERT INTO `items` (~A) VALUES (~A)"
                      parameter-names-string parameter-placeholders)))
    (values sql parameter-values)))


;;; (setf $opid (makeid))
;;; (sqlgen-insert-item $opid 5 (delectus-timestamp-now) 3 nil [(make-identity-string) 101])

;;; ---------------------------------------------------------------------
;;; fetching the latest listname
;;; ---------------------------------------------------------------------

(defun sqlgen-get-latest-listname ()
  (values "SELECT name FROM listnames ORDER BY timestamp DESC LIMIT 1"
          nil))

;;; ---------------------------------------------------------------------
;;; fetching the latest items
;;; ---------------------------------------------------------------------

;;; create the latest_items table
;;; -----------------------------

(defparameter $create-latest-items-table-sql
  (trim "
CREATE TEMPORARY TABLE latest_items AS
SELECT ranked.* FROM (
  SELECT ROW_NUMBER() OVER ( PARTITION BY itemid ORDER BY revision DESC, opid ) rank, * 
  FROM `items`) ranked
WHERE ranked.rank = 1
"))

(defun sqlgen-create-latest-items-table ()
  $create-latest-items-table-sql)

;;; check whether the latest_items table exists
;;; -------------------------------------------

(defun sqlgen-check-latest-items-table-exists ()
  (values "SELECT * FROM sqlite_temp_master WHERE type='table' AND name='latest_items'"
          nil))


;;; get  the latest items 
;;; ---------------------

(defun sqlgen-get-latest-items (&key
                                  (order :asc)
                                  (offset 0)
                                  (limit *default-result-items-per-page*))
  (yield
   (select :*
     (from :latest_items)
     (order-by (case order
                 (:asc '(:asc :timestamp))
                 (:desc '(:desc :timestamp))
                 (else (error "Unrecognized order ~S" order))))
     (offset offset)
     (limit limit))))

;;; (sqlgen-get-latest-items)
;;; (sqlgen-get-latest-items :offset 10000)
;;; (sqlgen-get-latest-items :order :desc)
;;; (sqlgen-get-latest-items :order :foo)

;;; count the latest items 
;;; ---------------------

(defun sqlgen-count-latest-items ()
  (yield
   (select ((:count :*))
     (from :latest_items))))

;;; (sqlgen-count-latest-items)
