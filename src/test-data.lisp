;;;; ***********************************************************************
;;;;
;;;; Name:          test-data.lisp
;;;; Project:       delectus 2
;;;; Purpose:       building databases for testing
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this code builds a list file with a given number of items. It
;;; simulates item updates from three different devices in order to
;;; enable us to assess Delectus performance in the presence of merged
;;; op logs.

(defparameter $test-list-id "38fee3007f2411ea984e38c9864ebde0")
(defparameter $test-words-path "/usr/share/dict/words")

(defparameter $node1-revision -1)
(defparameter $node2-revision -1)
(defparameter $node3-revision -1)

(defun make-test-list (list-path &key
                                   (count 100000)
                                   (list-name "Words Test"))
  (create-delectus-file list-path :listname list-name :listid $test-list-id :create-default-userdata nil)
  (let* ((word-column-id (make-identity-string))
         (word-column-description (column-description
                                   :id word-column-id
                                   :name "Word"
                                   :order 10.0
                                   :title t
                                   :subtitle :false
                                   :deleted :false))
         (node-column-id (make-identity-string))
         (node-column-description (column-description
                                   :id node-column-id
                                   :name "Node"
                                   :order 20.0
                                   :title :false
                                   :subtitle :false
                                   :deleted :false))
         (rev-column-id (make-identity-string))
         (rev-column-description (column-description
                                  :id rev-column-id
                                  :name "Revision"
                                  :order 30.0
                                  :title :false
                                  :subtitle :false
                                  :deleted :false))
         (column-ids (list word-column-id node-column-id rev-column-id))
         (column-descriptions (list word-column-description node-column-description rev-column-description)))
    (with-open-database (db list-path)
      ;; create the columns
      (db-ensure-columns-exist db column-descriptions)
      ;; insert the listname op
      (db-insert-listname db :opid (makeid) :timestamp (delectus-now) :name list-name)
      ;; insert the columns op
      (db-insert-columns db :opid (makeid) :timestamp (delectus-now) :column-descriptions column-descriptions)
      (with-open-file (in $test-words-path :direction :input)
        (loop for i from 0 and
           word = (read-line in nil nil nil)
           then (read-line in nil nil nil)
           while (and word
                      (< i count))
           do (let ((itemid (makeid)))
                ;; half the time insert from node1 with a lowercase copy
                (when (any [t nil])
                  (db-insert-item db :opid (makeid) :timestamp (delectus-now) :itemid itemid
                                  :revision (incf $node1-revision)
                                  :column-values (alist->plist
                                                  (mapcar 'cons
                                                          column-ids
                                                          [(string-downcase word) "Node 1" $node1-revision]))))
                ;; half the time insert from node2 with an uppercase copy
                (when (any [t nil])
                  (db-insert-item db :opid (makeid) :timestamp (delectus-now) :itemid itemid
                                  :revision (incf $node2-revision)
                                  :column-values (alist->plist
                                                  (mapcar 'cons
                                                          column-ids
                                                          [(string-upcase word) "Node 2" $node2-revision]))))
                ;; half the time insert from node3 with a capitalized copy
                (when (any [t nil])
                  (db-insert-item db :opid (makeid) :timestamp (delectus-now) :itemid itemid
                                  :revision (incf $node3-revision)
                                  :column-values (alist->plist
                                                  (mapcar 'cons
                                                          column-ids
                                                          [(string-capitalize word) "Node 3" $node3-revision]))))))))))




;;; (setf $wordtest100-path "/Users/mikel/Desktop/wordtest100.delectus2")
;;; 0.3sec to build, 49k
;;; (time (make-test-list $wordtest100-path :count 100))
;;; (delete-file $wordtest100-path)
;;; (time (get-latest-items (pathname $wordtest100-path)))
;;; (time (get-latest-items (pathname $wordtest100-path) :offset 50))

;;; (setf $wordtest1000-path "/Users/mikel/Desktop/wordtest1000.delectus2")
;;; 3.8sec to build, 213k
;;; (time (make-test-list $wordtest1000-path :count 1000))
;;; (delete-file $wordtest1000-path)
;;; (time (get-latest-items (pathname $wordtest1000-path)))
;;; (time (get-latest-items (pathname $wordtest1000-path) :offset 500))

;;; (setf $wordtest10k-path "/Users/mikel/Desktop/wordtest10k.delectus2")
;;; 33sec to build, 1.9M
;;; (time (make-test-list $wordtest10k-path :count 10000))
;;; (delete-file $wordtest10k-path)
;;; (time (get-latest-items (pathname $wordtest10k-path)))
;;; (time (get-latest-items (pathname $wordtest10k-path) :offset 5000))

;;; (setf $wordtest100k-path "/Users/mikel/Desktop/wordtest100k.delectus2")
;;; (delete-file $wordtest100k-path)
;;; 12m24sec to build, 19.1M
;;; (time (make-test-list $wordtest100k-path :count 100000))
;;; (time (get-latest-items (pathname $wordtest100k-path)))
;;; (time (get-latest-items (pathname $wordtest100k-path) :offset 50000))
;;; (time (get-latest-items (pathname $wordtest100k-path) :offset 90000))




;; SQL to collect latest items
#|

Using this sql:

SELECT ranked.* FROM (
  SELECT ROW_NUMBER() OVER ( PARTITION BY itemid ORDER BY revision DESC, opid DESC ) rank, * 
  FROM `items`) ranked
WHERE ranked.rank = 1

yields a query time of about 3/4 of a second returning 100,000 items
from a table containing 200,000 with random duplications

...but only with the right index:

CREATE INDEX idx_item_revision_origin on items (item, revision DESC, origin DESC)


Better yet, a reasonable limit yields times well under a second:

SELECT a.* FROM (
  SELECT ROW_NUMBER() OVER ( PARTITION BY item ORDER BY revision DESC, origin DESC ) rank, * 
  FROM `items`) a  WHERE a.rank = 1 LIMIT 100 OFFSET 0
(59ms)

SELECT a.* FROM (
  SELECT ROW_NUMBER() OVER ( PARTITION BY item ORDER BY revision DESC, origin DESC ) rank, * 
  FROM `items`) a  WHERE a.rank = 1 LIMIT 100 OFFSET 70000
(689ms)

Even faster is this:
SELECT ROW_NUMBER() OVER ( PARTITION BY item ORDER BY revision DESC, origin DESC ) rank FROM `items`

...which uses a covering index and returns in 190ms. It may be possible to work out how to get it to return the info I need to fetch rows by rowid or something,to get even faster results.

This one:

SELECT ranked.rowid 
FROM (SELECT rowid,ROW_NUMBER() OVER ( PARTITION BY item ORDER BY revision DESC, origin DESC ) rank FROM `items`)  as ranked
WHERE ranked.rank=1

returns the rowids of the 100,000 latest versions of all items in under 250ms

This one:

SELECT * FROM
(SELECT ranked.rowid 
FROM (SELECT rowid,ROW_NUMBER() OVER ( PARTITION BY item ORDER BY revision DESC, origin DESC ) rank FROM `items`)  as ranked
WHERE ranked.rank=1) latest
INNER JOIN items ON items.rowid = latest.rowid LIMIT 100

does the job in about the same time as the window function. According to 
EXPLAIN QUERY PLAN, this one collects the rowids using a covering index, which is
takes about 250ms, but then searches the table for the rows (or course)
which takes around half a second

|#
