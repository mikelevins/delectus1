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
;;; the main items index
;;; ---------------------------------------------------------------------

(defun sqlgen-create-items-itemid-timestamp-index ()
  (values "CREATE INDEX idx_items_itemid_timestamp on `items` (`itemid`, `timestamp` DESC)"
          nil))


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

;;; (setf $dbpath (pathname (uiop:native-namestring "~/Desktop/testlist.delectus2")))
;;; (setf $origin (make-origin (delectus-node-identity) (getpid) $dbpath))
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
;;; (setf $origin (make-origin (delectus-node-identity) (getpid) $dbpath))
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
;;; (setf $origin (make-origin (delectus-node-identity) (getpid) $dbpath))
;;; (setf $column-values [(make-column-label) "Foo"])
;;; (sqlgen-insert-item 5 $origin (delectus-timestamp-now) 1 nil $column-values)

;;; ---------------------------------------------------------------------
;;; fetching ops
;;; ---------------------------------------------------------------------

(defun sqlgen-get-latest-listname-op ()
  (values "SELECT * FROM listnames ORDER BY timestamp DESC LIMIT 1"
          nil))

(defun sqlgen-get-latest-columns-op ()
  (values "SELECT * FROM columns ORDER BY timestamp DESC LIMIT 1"
          nil))

(defun sqlgen-check-latest-items-table-exists ()
  (values "SELECT * FROM sqlite_temp_master WHERE type='table' AND name='latest_items'"
          nil))

;; (defun sqlgen-create-latest-items-table ()
;;   (values
;;    (trim "
;; CREATE TEMPORARY TABLE latest_items AS
;; SELECT ranked.* FROM (
;;   SELECT ROW_NUMBER() OVER ( PARTITION BY itemid ORDER BY timestamp DESC) rank, * 
;;   FROM `items`) ranked
;; where ranked.rank=1
;; ")
;;    nil))

;;; a version that works without window functions (doesn't require SQLite 3.28.0)
(defun sqlgen-create-latest-items-table ()
  (values
   (trim "
create temporary table latest_items as
select itemsB.*
    from items itemsB
    where timestamp = 
        (select max(timestamp) from items itemsA where itemsB.itemid=itemsA.itemid)
")
   nil))



(defun sqlgen-get-latest-items (&key
                                  (order :asc)
                                  (offset 0)
                                  (limit *default-result-items-per-page*))
  (yield
   (select :*
     (from :latest_items)
     (order-by (case order
                 (:asc '(:asc :itemid))
                 (:desc '(:desc :itemid))
                 (else (error "Unrecognized order ~S" order))))
     (offset offset)
     (limit limit))))

(defun sqlgen-count-latest-items ()
  (yield
   (select ((:count :*))
     (from :latest_items))))


(defun sqlgen-get-latest-filtered-items (&key
                                           (column-labels nil)
                                           (filter-text nil)
                                           (offset 0)
                                           (limit nil))
  (let* ((column-selector
          (if (empty? column-labels)
              "*"
              (join-strings ", "
                            (mapcar (lambda (lbl) (format nil "~A" lbl))
                                    column-labels))))
         (like-clauses
          ;; have to have filter-text and column-labels in order to generate filtered results
          (if (or (empty? filter-text)
                  (empty? column-labels))
              ""
              (concatenate 'string "( "
                           (join-strings " OR "
                                         (mapcar (lambda (lbl)
                                                   (format nil "`~A` LIKE '%~A%'"
                                                           lbl filter-text))
                                                 column-labels))
                           " ) ")))
         (where-clause (if (empty? filter-text)
                           ""
                           (format nil " WHERE ~A" like-clauses)))
         (offset-clause (if (and limit offset)
                            (format nil "OFFSET ~D " offset)
                            ""))
         (limit-clause (if limit
                           (format nil "LIMIT ~D " limit)
                           "")))
    (values
     (trim (format nil "
SELECT ~A
FROM `latest_items` ~A ~A ~A
" column-selector where-clause limit-clause offset-clause))
     nil)))

;;; (sqlgen-get-latest-filtered-items :column-labels ["lbl1" "lbl2"] :filter-text "Foobie")
;;; (sqlgen-get-latest-filtered-items :column-labels ["lbl1" "lbl2"] :filter-text "")


(defun sqlgen-count-latest-filtered-items (&key
                                             (column-labels nil)
                                             (filter-text nil)
                                             (offset 0)
                                             (limit nil))
  (let* ((like-clauses
          ;; have to have filter-text and column-labels in order to generate filtered results
          (if (or (empty? filter-text)
                  (empty? column-labels))
              nil
              (concatenate 'string "( "
                           (join-strings " OR "
                                         (mapcar (lambda (lbl)
                                                   (format nil "`~A` LIKE '%~A%'"
                                                           lbl filter-text))
                                                 column-labels))
                           " ) ")))
         (where-clause (if (empty? column-labels)
                           ""
                           (format nil " WHERE ~A" like-clauses)))
         (offset-clause (if (and limit offset)
                            (format nil "OFFSET ~D " offset)
                            ""))
         (limit-clause (if limit
                           (format nil "LIMIT ~D " limit)
                           "")))
    (values
     (trim (format nil "
SELECT COUNT (*)
FROM `latest_items` ~A ~A ~A
" where-clause limit-clause offset-clause))
     nil)))

;;; (sqlgen-count-latest-filtered-items :column-labels ["lbl1" "lbl2"] :filter-text "Foobie")
;;; (sqlgen-count-latest-filtered-items :column-labels $lbls :filter-text "Springdale")


(defun sqlgen-get-specified-item (itemid origin)
  (yield
   (select :*
     (from :items)
     (where (:and (:= :itemid itemid)
                  (:= :origin origin)))
     (order-by (:desc :timestamp)))))

;;; (sqlgen-get-specified-item 0 (delectus-timestamp-now))
