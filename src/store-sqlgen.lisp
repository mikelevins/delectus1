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
        (fileid :type 'text)
        (format :type 'text)
        (modified :type 'integer)
        (next_revision :type 'integer)))))

;;; (sqlgen-create-delectus-table)

(defun sqlgen-init-delectus-table (list-identity format-string
                                   &key
                                     (fileid nil)
                                     (next-revision 0))
  (let ((timestamp (now-utc))
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

;;; 'syncs' table
;;; ----------------

(defun sqlgen-create-syncs-table ()
  (yield
   (create-table :syncs
       ((opid :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)
        (itemid :type 'blob)
        (peerid :type 'blob)))))

;;; (sqlgen-create-syncs-table)

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

(defun sqlgen-insert-listname (opid revision timestamp name)
  (yield
   (insert-into :listnames
     (set= :opid opid
           :revision revision
           :timestamp timestamp
           :name name))))

;;; (setf $opid (makeid))
;;; (sqlgen-insert-listname $opid 3 (now-utc) "Foobar")

(defun sqlgen-insert-comment (opid revision timestamp comment)
  (yield
   (insert-into :comments
     (set= :opid opid
           :revision revision
           :timestamp timestamp
           :comment comment))))

;;; (setf $opid (makeid))
;;; (sqlgen-insert-comment $opid 3 (now-utc) "foo bar baz...")

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
;;; (sqlgen-insert-columns $opid 5 (now-utc) (list (column-description :id (make-identity-string) :name "Item")))


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
;;; (sqlgen-insert-item $opid 5 (now-utc) 3 nil [(make-identity-string) 101])

;;; ---------------------------------------------------------------------
;;; fetching the latest items
;;; ---------------------------------------------------------------------

(defparameter $get-latest-items-sql
  (trim "
SELECT ranked.* FROM (
  SELECT ROW_NUMBER() OVER ( PARTITION BY itemid ORDER BY revision DESC, opid ) rank, * 
  FROM `items`) ranked
WHERE ranked.rank = 1 LIMIT ~A OFFSET ~A
"))

(defun sqlgen-get-latest-items (&key (offset 0)(limit 100))
  (values (format nil $get-latest-items-sql limit offset)
          nil))

;;; (sqlgen-get-latest-items :limit 25 :offset 1500)
