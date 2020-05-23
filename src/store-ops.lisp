;;;; ***********************************************************************
;;;;
;;;; Name:          store-ops.lisp
;;;; Project:       delectus 2
;;;; Purpose:       reading and writing ops
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; =====================================================================
;;; inserting ops
;;; =====================================================================

(defmethod db-insert-listname ((db sqlite-handle)
                               &key
                                 revision
                                 origin
                                 timestamp
                                 name)
  (bind ((revision (or revision (db-get-next-revision db)))
         (origin (or origin (error "Missing :ORIGIN argument")))
         (timestamp (or timestamp (delectus-timestamp-now)))
         (name (or name (error "Missing :NAME argument")))
         (sql vals (sqlgen-insert-listname revision origin timestamp name)))
    (apply 'execute-non-query db sql vals)
    (db-set-next-revision db (1+ revision))))


(defmethod db-insert-columns ((db sqlite-handle)
                              &key
                                revision
                                origin
                                timestamp
                                column-descriptions)
  (bind ((revision (or revision (db-get-next-revision db)))
         (origin (or origin (error "Missing :ORIGIN argument")))
         (timestamp (or timestamp (delectus-timestamp-now)))
         (sql vals (sqlgen-insert-columns revision origin timestamp column-descriptions)))
    (apply 'execute-non-query db sql vals)
    (db-set-next-revision db (1+ revision))))


(defmethod db-insert-item ((db sqlite-handle)
                           &key
                             revision
                             origin
                             timestamp
                             itemid
                             deleted
                             column-values)
  (bind ((revision (or revision (db-get-next-revision db)))
         (origin (or origin (error "Missing :ORIGIN argument")))
         (timestamp (or timestamp (delectus-timestamp-now)))
         (itemid (or itemid (db-get-next-itemid db)))
         (sql vals (sqlgen-insert-item revision origin timestamp itemid deleted column-values)))
    (apply 'execute-non-query db sql vals)
    (db-set-next-revision db (1+ revision))
    (db-set-next-itemid db (1+ itemid))))

;;; =====================================================================
;;; fetching ops
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; listname
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-listname-op ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-latest-listname-op)))
    (first (apply 'execute-to-list db sql vals))))

(defmethod get-latest-listname-op ((dbpath pathname))
  (with-open-database (db dbpath)
    (db-get-latest-listname-op db)))

;;; (setf $movies-test-path (path "~/Desktop/Movies-test.delectus2"))
;;; (get-latest-listname-op $movies-test-path)


;;; ---------------------------------------------------------------------
;;; columns
;;; ---------------------------------------------------------------------

(defmethod db-get-latest-columns-op ((db sqlite-handle))
  (bind ((sql vals (sqlgen-get-latest-columns-op)))
    (first (apply 'execute-to-list db sql vals))))

(defmethod get-latest-columns-op ((dbpath pathname))
  (with-open-database (db dbpath)
    (db-get-latest-columns-op db)))

;;; (setf $movies-test-path (path "~/Desktop/Movies-test.delectus2"))
;;; (get-latest-columns-op $movies-test-path)


;;; ---------------------------------------------------------------------
;;; items
;;; ---------------------------------------------------------------------

;;; the `latest-items` table
;;; ------------------------

(defmethod db-check-latest-items-table-exists ((db sqlite-handle))
  (bind ((sql vals (sqlgen-check-latest-items-table-exists))
         (found-table (apply 'execute-to-list db sql vals)))
    (if found-table t nil)))

;;; (setf $words-test-path (path "~/Desktop/wordtest100k.delectus2"))
;;; (with-open-database (db $words-test-path) (db-check-latest-items-table-exists db))

(defmethod db-create-latest-items-table ((db sqlite-handle))
  (bind ((sql vals (sqlgen-create-latest-items-table)))
    (apply 'execute-to-list db sql vals)))


;;; getting the latest items
;;; ------------------------

(defmethod db-get-latest-items ((db sqlite-handle)
                                &key
                                  (offset 0)
                                  (limit 100))
  (unless (db-check-latest-items-table-exists db)
    (db-create-latest-items-table db))
  (bind ((sql vals (sqlgen-get-latest-items :limit limit :offset offset))
         (items-with-rank (apply 'execute-to-list db sql vals)))
    ;; drop the ranks (which are always 1)
    (mapcar #'cdr items-with-rank)))

(defmethod get-latest-items ((db-path pathname)
                             &key
                               (offset 0)
                               (limit 100))
  (assert (probe-file db-path) () "No such file: ~S" db-path)
  (with-open-database (db db-path)
    (db-get-latest-items db :offset offset :limit limit)))

;;; (setf $movies-test-path (path "~/Desktop/Movies-test.delectus2"))
;;; (delete-file $movies-test-path)
;;; (time (get-latest-items (pathname $movies-test-path)))
;;; (time (get-latest-items (pathname $movies-test-path) :offset 1000 :limit 5))


;;; counting the latest items
;;; -------------------------

(defmethod db-count-latest-items ((db sqlite-handle)
                                  &key)
  (unless (db-check-latest-items-table-exists db)
    (db-create-latest-items-table db))
  (bind ((sql vals (sqlgen-count-latest-items)))
    (apply 'execute-single db sql vals)))

(defmethod count-latest-items ((db-path pathname))
  (assert (probe-file db-path) () "No such file: ~S" db-path)
  (with-open-database (db db-path)
    (db-count-latest-items db)))

;;; (setf $words-test-path (path "~/Desktop/words.delectus2"))
;;; (time (count-latest-items $words-test-path))


;;; getting the latest items, but filtered
;;; --------------------------------------
;;; - column-labels is a list of columns we want in the results
;;; - filter-text is text we want to match against the contents of those columns

(defmethod db-get-latest-filtered-items ((db sqlite-handle)
                                         &key
                                           (column-labels nil)
                                           (filter-text nil)
                                           (offset 0)
                                           (limit nil))
  (bind ((sql vals (sqlgen-get-latest-filtered-items :column-labels column-labels
                                                     :filter-text filter-text
                                                     :offset offset
                                                     :limit limit)))
    (unless (db-check-latest-items-table-exists db)
      (db-create-latest-items-table db))
    (apply 'execute-to-list db sql vals)))


(defmethod get-latest-filtered-items ((db-path pathname)
                                      &key
                                        (column-labels nil)
                                        (filter-text nil)
                                        (offset 0)
                                        (limit nil))
  (assert (probe-file db-path) () "No such file: ~S" db-path)
  (with-open-database (db db-path)
    (db-get-latest-filtered-items db
                                  :column-labels column-labels
                                  :filter-text filter-text
                                  :offset offset
                                  :limit limit)))

;;; (get-latest-filtered-items $zips-test-path :column-labels $lbls :filter-text "Springdale")


;;; counting the latest items, but filtered
;;; ---------------------------------------
;;; - column-labels is a list of columns we want in the results
;;; - filter-text is text we want to match against the contents of those columns

(defmethod db-count-latest-filtered-items ((db sqlite-handle)
                                           &key
                                             (column-labels nil)
                                             (filter-text nil)
                                             (offset 0)
                                             (limit nil))
  (unless (db-check-latest-items-table-exists db)
    (db-create-latest-items-table db))
  (bind ((sql vals (sqlgen-count-latest-filtered-items :column-labels column-labels
                                                       :filter-text filter-text
                                                       :offset offset
                                                       :limit limit)))
    (apply 'execute-single db sql vals)))

(defmethod count-latest-filtered-items ((db-path pathname)
                                        &key
                                          (column-labels nil)
                                          (filter-text nil)
                                          (offset 0)
                                          (limit nil))
  (assert (probe-file db-path) () "No such file: ~S" db-path)
  (with-open-database (db db-path)
    (db-count-latest-filtered-items db
                                    :column-labels column-labels
                                    :filter-text filter-text
                                    :offset offset
                                    :limit limit)))


;;; getting a specified item
;;; --------------------------------------

(defmethod db-get-specified-item ((db sqlite-handle)(itemid integer)(origin integer))
  (bind ((sql vals (sqlgen-get-specified-item itemid origin)))
    (unless (db-check-latest-items-table-exists db)
      (db-create-latest-items-table db))
    (apply 'execute-to-list db sql vals)))


(defmethod get-specified-item ((db-path pathname)(itemid integer)(origin integer))
  (assert (probe-file db-path) () "No such file: ~S" db-path)
  (with-open-database (db db-path)
    (db-get-specified-item db itemid origin)))

;;; (setf $zips-test-path (path "~/Desktop/Zipcodes.delectus2"))
;;; (get-latest-items (path "~/Desktop/Zipcodes.delectus2") :limit 5)
;;; (get-specified-item $zips-test-path :itemid 1)

;;; (setf $wordtest100k-path (path "/Users/mikel/Desktop/wordtest100k.delectus2"))
;;; (get-latest-items $wordtest100k-path :limit 5)
;;; (time (setf $it (get-specified-item $wordtest100k-path 11200)))
;;; (item-op-itemid (first $it))
;;; (item-op-revision (first $it))
;;; (item-op-origin (first $it))
;;; (delectus-timestamp->local-time (item-op-timestamp (first $it)))
;;; (item-op-deleted (first $it))
;;; (item-op-userdata (first $it))

;;; (time (setf $cols (get-latest-columns-op $wordtest100k-path)))
;;; (columns-op-userdata $cols)
