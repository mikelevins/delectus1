;;;; ***********************************************************************
;;;;
;;;; Name:          store-ops.lisp
;;;; Project:       delectus 2
;;;; Purpose:       fetching and inserting ops
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; =====================================================================
;;; input validation
;;; =====================================================================

(defun db-ensure-origin (db thing)
  (cond
    ((null thing)
     (make-origin (process-identity)
                  (sqlite::database-path db)))
    ((origin? thing) thing)
    (t (error "Invalid origin in ~S; expected an origin or nil."
              thing))))

;;; (defparameter $testfile-path (path "~/Desktop/testfile.delectus2"))
;;; (with-open-database (db $testfile-path)(sqlite::database-path db))

(defun db-ensure-revision-number (db thing)
  (cond
    ((null thing)(db-get-next-revision db))
    ((and (integerp thing)
          (>= thing 0))
     thing)
    (t (error "Invalid revision number in ~S; expected a nonnegative integer or nil."
              thing))))

(defun db-ensure-item-order-number (db thing)
  (cond
    ((null thing)(db-get-next-item-order db))
    ((typep thing 'double-float) thing)
    (t (error "Invalid item-order number in ~S; expected a double-float or nil."
              thing))))

(defun db-ensure-timestamp (db thing)
  (cond
    ((null thing)(delectus-timestamp-now))
    ((and (integerp thing)
          (>= thing 0))
     thing)
    (t (error "Invalid timestamp in ~S; expected a nonnegative integer or nil."
              thing))))

(defun db-ensure-listname-string (db thing)
  (cond
    ((stringp thing) thing)
    (t (error "Invalid listname in ~S; expected a string."
              thing))))

(defun db-ensure-comment-string (db thing)
  (cond
    ((stringp thing) thing)
    (t (error "Invalid comment text in ~S; expected a string."
              thing))))

;;; converts a list of column-descriptions to a plist whose keys are
;;; column-label strings, and whose values are JSON column objects
(defun ensure-columns-data (thing)
  (cond
    ((listp thing) (loop for desc in thing
                      appending (list (column-description-label desc)
                                      (column-description-to-json desc))))
    (t (error "Invalid columns data in ~S; expected a list of column-descriptions."
              thing))))

;;; =====================================================================
;;; inserting ops
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; listname op
;;; ---------------------------------------------------------------------

(defmethod db-insert-listname-op ((db sqlite-handle)
                                  &key
                                    origin
                                    revision
                                    timestamp
                                    listname)
  (bind ((origin (db-ensure-origin db origin))
         (revision (db-ensure-revision-number db revision))
         (timestamp (db-ensure-timestamp db timestamp))
         (listname (db-ensure-listname-string db listname))
         (name-json (jonathan:to-json listname))
         (sql vals (sqlgen-insert-listname-op origin revision timestamp name-json)))
    (apply 'execute-non-query db sql vals)))

;;; ---------------------------------------------------------------------
;;; comment op
;;; ---------------------------------------------------------------------

(defmethod db-insert-comment-op ((db sqlite-handle)
                                 &key
                                   origin
                                   revision
                                   timestamp
                                   comment)
  (bind ((origin (db-ensure-origin db origin))
         (revision (db-ensure-revision-number db revision))
         (timestamp (db-ensure-timestamp db timestamp))
         (comment (db-ensure-comment-string db comment))
         (comment-json (jonathan:to-json comment))
         (sql vals (sqlgen-insert-comment-op origin revision timestamp comment-json)))
    (apply 'execute-non-query db sql vals)))

;;; ---------------------------------------------------------------------
;;; columns op
;;; ---------------------------------------------------------------------

(defmethod db-insert-columns-op ((db sqlite-handle)
                                 &key
                                   origin
                                   revision
                                   timestamp
                                   columns)
  (bind ((origin (db-ensure-origin db origin))
         (revision (db-ensure-revision-number db revision))
         (timestamp (db-ensure-timestamp db timestamp))
         (columns-data (ensure-columns-data columns))
         (sql vals (sqlgen-insert-columns-op origin revision timestamp columns-data)))
    (apply 'execute-non-query db sql vals)))

;;; ---------------------------------------------------------------------
;;; item op
;;; ---------------------------------------------------------------------

(defmethod db-insert-item-op ((db sqlite-handle)
                              &key
                                origin
                                revision
                                itemid
                                item-order
                                timestamp
                                field-values)
  (bind ((origin (db-ensure-origin db origin))
         (revision (db-ensure-revision-number db revision))
         (itemid (or itemid (makeid)))
         (item-order (db-ensure-item-order-number db item-order))
         (timestamp (db-ensure-timestamp db timestamp))
         (sql vals (sqlgen-insert-item-op origin revision itemid item-order timestamp field-values)))
    (apply 'execute-non-query db sql vals)))

;;; =====================================================================
;;; retrieving ops
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; listname op
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-listname-op ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-latest-listname-op)))
    (first (apply 'execute-to-list db sql vals))))


(defmethod get-latest-listname-op ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-latest-listname-op db)))

;;; ---------------------------------------------------------------------
;;; comment op
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-comment-op ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-latest-comment-op)))
    (first (apply 'execute-to-list db sql vals))))


(defmethod get-latest-comment-op ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-latest-comment-op db)))

;;; ---------------------------------------------------------------------
;;; columns op
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-columns-op ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-latest-columns-op)))
    (first (apply 'execute-to-list db sql vals))))

(defmethod get-latest-columns-op ((db-path pathname))
  (with-open-database (db db-path)
    (db-get-latest-columns-op db)))

;;; =====================================================================
;;; item ops
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; creating the temporary "latest_items" table
;;; ---------------------------------------------------------------------

(defmethod db-latest-items-table-exists? ((db sqlite-handle))
  (bind ((sql vals (sqlgen-check-latest-items-table-exists))
         (found-table (apply 'execute-to-list db sql vals)))
    (if found-table t nil)))

(defmethod db-create-latest-items-table ((db sqlite-handle))
  (bind ((sql vals (sqlgen-create-latest-items-table)))
    (apply 'execute-to-list db sql vals)))

;;; ---------------------------------------------------------------------
;;; counting the latest items
;;; ---------------------------------------------------------------------

(defmethod db-count-latest-items ((db sqlite-handle))
  (unless (db-latest-items-table-exists? db)
    (db-create-latest-items-table db))
  (bind ((sql vals (sqlgen-count-latest-items)))
    (apply 'execute-single db sql vals)))

(defmethod count-latest-items ((dbpath pathname))
  (assert (probe-file dbpath) () "No such file: ~S" dbpath)
  (with-open-database (db dbpath)
    (db-count-latest-items db)))

;;; ---------------------------------------------------------------------
;;; fetching the latest items
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-items ((db sqlite-handle)
                                &key
                                  (offset 0)
                                  (limit *default-result-items-per-page*))
  (unless (db-latest-items-table-exists? db)
    (db-create-latest-items-table db))
  (bind ((sql vals (sqlgen-get-latest-items :offset offset :limit limit)))
    (apply 'execute-to-list db sql vals)))

(defmethod get-latest-items ((dbpath pathname)
                             &key
                               (offset 0)
                               (limit *default-result-items-per-page*))
  (assert (probe-file dbpath) () "No such file: ~S" dbpath)
  (with-open-database (db dbpath)
    (db-get-latest-items db  :offset offset :limit limit)))

;;; counting filtered items
;;; ---------------------------------------------------------------------

(defmethod db-count-latest-filtered-items ((db sqlite-handle)
                                           &key
                                             (filter-text nil)
                                             (offset 0)
                                             (limit *default-result-items-per-page*))
  (unless (db-latest-items-table-exists? db)
    (db-create-latest-items-table db))
  (bind ((item-columns (db-get-latest-columns-op db))
         (item-userdata-columns (mapcar #'jonathan:parse
                                        (drop (length *columns-op-columns*)
                                              item-columns)))
         (column-labels (mapcar #'column-description-label
                                item-userdata-columns))
         (sql vals (sqlgen-count-latest-filtered-items :filter-text filter-text :column-labels column-labels
                                                       :offset offset :limit limit)))
    (apply 'execute-single db sql vals)))

(defmethod count-latest-filtered-items ((db-path pathname)
                                        &key
                                          (filter-text nil)
                                          (offset 0)
                                          (limit *default-result-items-per-page*))
  (with-open-database (db db-path)
    (db-count-latest-filtered-items db :filter-text filter-text :offset offset :limit limit)))

;;; (time (count-latest-filtered-items $movies-test-path :filter-text "East" :limit 1 :offset 2))
;;; (time (count-latest-filtered-items $zips-test-path :filter-text "Spring" :limit -1 :offset 0))
;;; (time (count-latest-filtered-items $wordtest100k-path :filter-text "" :limit -1 :offset 0))

;;; fetching filtered items
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-filtered-items ((db sqlite-handle)
                                         &key
                                           (filter-text nil)
                                           (offset 0)
                                           (limit *default-result-items-per-page*))
  (unless (db-latest-items-table-exists? db)
    (db-create-latest-items-table db))
  (bind ((item-columns (db-get-latest-columns-op db))
         (item-userdata-columns (mapcar #'jonathan:parse
                                        (drop (length *columns-op-columns*)
                                              item-columns)))
         (column-labels (mapcar #'column-description-label
                                item-userdata-columns))
         (sql vals (sqlgen-get-latest-filtered-items :filter-text filter-text :column-labels column-labels
                                                     :offset offset :limit limit)))
    (apply 'execute-to-list db sql vals)))

(defmethod get-latest-filtered-items ((db-path pathname)
                                      &key
                                        (filter-text nil)
                                        (offset 0)
                                        (limit *default-result-items-per-page*))
  (with-open-database (db db-path)
    (db-get-latest-filtered-items db :filter-text filter-text :offset offset :limit limit)))

;;; (time (get-latest-filtered-items $movies-test-path :filter-text "East" :limit 1 :offset 2))
;;; (length (time (get-latest-filtered-items $zips-test-path :filter-text "Spring" :limit 1000 :offset 0)))


;;; =====================================================================
;;; op accessors
;;; =====================================================================



;;; ---------------------------------------------------------------------
;;; tests
;;; ---------------------------------------------------------------------

;;; (setf $movies-test-path (path "~/Desktop/Movies.delectus2"))
;;; (setf $zips-test-path (path "~/Desktop/Zipcodes.delectus2"))
;;; (setf $wordtest100-path (path "~/Desktop/wordtest100.delectus2"))
;;; (setf $wordtest1k-path (path "~/Desktop/wordtest1k.delectus2"))
;;; (setf $wordtest10k-path (path "~/Desktop/wordtest10k.delectus2"))
;;; (setf $wordtest100k-path (path "~/Desktop/wordtest100k.delectus2"))

;;; (time (get-latest-listname-op $movies-test-path))
;;; (time (get-latest-listname-op $wordtest100k-path))

;;; (time (get-latest-comment-op $movies-test-path))
;;; (time (get-latest-comment-op $wordtest100k-path))

;;; (time (get-latest-columns-op $movies-test-path))
;;; (time (get-latest-columns-op $zips-test-path))
;;; (time (get-latest-columns-op $wordtest100k-path))

;;; (time (count-latest-items $movies-test-path))
;;; (time (count-latest-items $zips-test-path))
;;; (time (count-latest-items $wordtest100-path))
;;; (time (count-latest-items $wordtest1k-path))
;;; (time (count-latest-items $wordtest10k-path))
;;; (time (count-latest-items $wordtest100k-path))

;;; (time (get-latest-items $movies-test-path))
;;; (time (get-latest-items $zips-test-path))
;;; (time (get-latest-items $wordtest100-path))
;;; (time (get-latest-items $wordtest10k-path))
;;; (time (get-latest-items $wordtest100k-path))

