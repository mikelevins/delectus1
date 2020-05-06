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

(defparameter $process-id1 #(135 141 233 192 178 91 68 27 167 112 44 213 52 127 214 101))
(defparameter $process-id2 #(86 107 17 178 168 160 67 90 155 36 255 145 157 23 32 242))
(defparameter $process-id3 #(102 90 52 70 26 70 69 89 150 180 153 8 96 167 194 58))

(defun make-test-list (list-path &key
                                   (count 100000)
                                   (list-name "Words Test"))
  (create-delectus-file list-path :listname list-name :listid $test-list-id :create-default-userdata nil)
  (let* ((origin1 (make-origin $process-id1 (pathname list-path)))
         (origin1-string (identity->string origin1))
         (origin2 (make-origin $process-id2 (pathname list-path)))
         (origin2-string (identity->string origin2))
         (origin3 (make-origin $process-id3 (pathname list-path)))
         (origin3-string (identity->string origin3))
         (word-column-id (make-identity-string))
         (word-column-description (column-description
                                   :id word-column-id
                                   :name "Word"
                                   :order 10.0
                                   :title t
                                   :subtitle :false
                                   :deleted :false))
         (origin-column-id (make-identity-string))
         (origin-column-description (column-description
                                     :id origin-column-id
                                     :name "Origin"
                                     :order 20.0
                                     :title :false
                                     :subtitle :false
                                     :deleted :false))
         (column-ids (list word-column-id origin-column-id))
         (column-descriptions (list word-column-description origin-column-description)))
    (with-open-database (db list-path)
      ;; create the columns
      (db-ensure-columns-exist db column-descriptions)
      ;; insert the listname op
      (db-insert-listname db :origin origin1 :timestamp (now-utc) :name list-name)
      ;; insert the columns op
      (db-insert-columns db :origin origin1 :timestamp (now-utc) :column-descriptions column-descriptions)
      (with-open-file (in $test-words-path :direction :input)
        (loop for i from 0 and
           word = (read-line in nil nil nil)
           then (read-line in nil nil nil)
           while (and (< i count)
                      word)
           do (let ((item (db-get-next-item db))
                    ;; reuse the same revision number for all 3 inserts to simulate concurrent edits
                    (revision (db-get-next-revision db)))
                ;; always insert from origin1 with a lowercase copy of the word
                (db-insert-item db :origin origin1 :timestamp (now-utc) :item item :revision revision
                                :column-values (alist->plist
                                                (mapcar 'cons
                                                        column-ids
                                                        [(string-downcase word) origin1-string])))
                ;; half the time insert from origin2 with an uppercase copy
                (when (any [t nil])
                  (db-insert-item db :origin origin2 :timestamp (now-utc) :item item :revision revision
                                  :column-values (alist->plist
                                                  (mapcar 'cons
                                                          column-ids
                                                          [(string-upcase word) origin2-string]))))
                ;; half the time insert from origin3 with a capitalized copy
                (when (any [t nil])
                  (db-insert-item db :origin origin3 :timestamp (now-utc) :item item :revision revision
                                  :column-values (alist->plist
                                                  (mapcar 'cons
                                                          column-ids
                                                          [(string-capitalize word) origin3-string]))))))))))



;; SQL to collect latest items
#|

Using this sql:

SELECT a.* FROM (
  SELECT ROW_NUMBER() OVER ( PARTITION BY item ORDER BY revision DESC, origin DESC ) rank, * 
  FROM `items`) a 
WHERE a.rank = 1

yields a query time of 3/4 of a second returning 100,000 items from a
table containing 200,000 with random duplications

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

|#

;;; (Time (make-test-list "/Users/mikel/Desktop/wordtest100.delectus2" :count 100))
;;; (delete-file "/Users/mikel/Desktop/wordtest100.delectus2")

;;; (time (make-test-list "/Users/mikel/Desktop/wordtest1k.delectus2" :count 1000))
;;; (delete-file "/Users/mikel/Desktop/wordtest1k.delectus2")

;;; (time (make-test-list "/Users/mikel/Desktop/wordtest10k.delectus2" :count 10000))
;;; (delete-file "/Users/mikel/Desktop/wordtest10k.delectus2")

;;; 15m47s to build
;;; (time (make-test-list "/Users/mikel/Desktop/wordtest100k.delectus2" :count 100000))
;;; (delete-file "/Users/mikel/Desktop/wordtest100k.delectus2")
