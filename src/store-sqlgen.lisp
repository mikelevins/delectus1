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

;;; =====================================================================
;;; creating tables
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; 'delectus' table
;;; ---------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------
;;; 'listnames' table
;;; ---------------------------------------------------------------------

(defun sqlgen-create-listnames-table ()
  (yield
   (create-table :listnames
       ((origin :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)
        (name :type 'text)))))

;;; (sqlgen-create-listnames-table)

;;; ---------------------------------------------------------------------
;;; 'comments' table
;;; ---------------------------------------------------------------------

(defun sqlgen-create-comments-table ()
  (yield
   (create-table :comments
       ((origin :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)
        (comment :type 'text)))))

;;; (sqlgen-create-comments-table)


;;; ---------------------------------------------------------------------
;;; 'columns' table
;;; ---------------------------------------------------------------------

(defun sqlgen-create-columns-table ()
  (yield
   (create-table :columns
       ((origin :type 'blob)
        (revision :type 'integer)
        (timestamp :type 'integer)))))

;;; (sqlgen-create-columns-table)


;;; ---------------------------------------------------------------------
;;; 'items' table
;;; ---------------------------------------------------------------------

(defun sqlgen-create-items-table ()
  (yield
   (create-table :items
       ((origin :type 'blob)
        (revision :type 'integer)
        (itemid :type 'blob)
        (item_order :type 'real)
        (timestamp :type 'integer)
        (deleted :type 'integer)))))

;;; (sqlgen-create-items-table)

;;; ---------------------------------------------------------------------
;;; the main items index
;;; ---------------------------------------------------------------------

;; (defun sqlgen-create-items-itemid-timestamp-index ()
;;   (values "CREATE INDEX idx_items_itemid_timestamp on `items` (`itemid`, `timestamp` DESC)"
;;           nil))

(defun sqlgen-create-items-itemid-revision-timestamp-index ()
  (values "CREATE INDEX idx_main_items on `items` (`itemid`, `revision` DESC, `timestamp` DESC, `origin`)"
          nil))

;;; =====================================================================
;;; getting and setting times
;;; =====================================================================

(defun sqlgen-get-created-time ()
  (yield
   (select :created
     (from :delectus))))

(defun sqlgen-get-modified-time ()
  (yield
   (select :modified
     (from :delectus))))

(defun sqlgen-set-modified-time (timestamp)
  (yield
   (update :delectus
     (set= :modified timestamp))))

;;; =====================================================================
;;; getting next revision and order
;;; =====================================================================

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

;;; =====================================================================
;;; adding userdata columns
;;; =====================================================================

(defun sqlgen-add-columns-userdata-column (column-label)
  (yield
   (alter-table :columns
     (add-column (as-keyword column-label) :type 'text))))

(defun sqlgen-add-items-userdata-column (column-label)
  (yield
   (alter-table :items
     (add-column (as-keyword column-label) :type 'text))))

;;; =====================================================================
;;; inserting ops
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; 'listname' op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-listname-op (origin revision timestamp name-json)
  (yield
   (insert-into :listnames
     (set= :origin origin
           :revision revision
           :timestamp timestamp
           :name name-json))))

;;; ---------------------------------------------------------------------
;;; 'comment' op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-comment-op (origin revision timestamp comment-json)
  (yield
   (insert-into :comments
     (set= :origin origin
           :revision revision
           :timestamp timestamp
           :comment comment-json))))

;;; (sqlgen-insert-comment-op (makeid) 1 (delectus-timestamp-now) "Foo")

;;; ---------------------------------------------------------------------
;;; 'columns' op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-columns-op (origin revision timestamp columns-data)
  (let ((userdata-column-labels (get-plist-keys columns-data))
        (userdata-column-data (get-plist-values columns-data)))
    (values (format nil "INSERT INTO `columns` (origin, revision, timestamp, ~{~A~^, ~}) VALUES (?, ?, ?, ~{~A~^, ~})"
                    userdata-column-labels
                    (mapcar (constantly "?") userdata-column-labels))
            (append [origin revision timestamp] userdata-column-data))))

;;; (setf $cols [(make-default-column-description :name "Item")])
;;; (sqlgen-insert-columns-op (makeid) 1 (delectus-timestamp-now) (ensure-columns-data $cols))

;;; ---------------------------------------------------------------------
;;; 'item' op
;;; ---------------------------------------------------------------------

(defun sqlgen-insert-item-op (origin revision itemid item-order timestamp field-values-map)
  ;; field-values-map is a plist of [column-label value ...]
  (let ((userdata-column-labels (get-plist-keys field-values-map))
        (userdata-field-values (get-plist-values field-values-map)))
    (values (format nil "INSERT INTO `items` (origin, revision, itemid, item_order, timestamp, ~{~A~^, ~}) VALUES (?, ?, ?, ?, ?, ~{~A~^, ~})"
                    userdata-column-labels
                    (mapcar (constantly "?") userdata-column-labels))
            (append [origin revision itemid item-order timestamp] userdata-field-values))))

;;; (sqlgen-insert-item-op (makeid) 1 (makeid) (delectus-timestamp-now) [(make-column-label) "Foo"])


;;; =====================================================================
;;; getting the latest data
;;; =====================================================================

(defun sqlgen-check-latest-items-table-exists ()
  (values "SELECT * FROM sqlite_temp_master WHERE type='table' AND name='latest_items'"
          nil))

;; (defun sqlgen-create-latest-items-table ()
;;   (values
;;    (trim "
;; create temporary table latest_items as
;; select itemsB.*
;;     from items itemsB
;;     where timestamp = 
;;         (select max(timestamp) from items itemsA where itemsB.itemid=itemsA.itemid)
;; ")
;;    nil))

(defun sqlgen-create-latest-items-table ()
  (values
   (trim "
create temporary table latest_items as
SELECT latest.*
  FROM (SELECT *,
               ROW_NUMBER() OVER (PARTITION BY itemid ORDER BY revision DESC, timestamp DESC, origin) rank
        FROM items) latest
 WHERE latest.rank = 1 ORDER BY item_order ASC
")
   nil))



(defun sqlgen-count-latest-items ()
  (yield
   (select ((:count :*))
     (from :|`latest_items`|))))

;;; (sqlgen-count-latest-items)


(defun sqlgen-get-latest-items (&key
                                  (order :asc)
                                  (offset 0)
                                  (limit *default-result-items-per-page*))
  (yield
   (select :*
     (from :latest_items)
     (order-by (case order
                 (:asc '(:asc :item_order))
                 (:desc '(:desc :item_order))
                 (else (error "Unrecognized order ~S" order))))
     (offset offset)
     (limit limit))))
