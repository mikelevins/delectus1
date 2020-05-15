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

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------

(defmacro with-arbitrary-order (&rest forms)
  (let ((shuffled-forms (alexandria:shuffle forms)))
    `(progn ,@shuffled-forms)))

;;; ---------------------------------------------------------------------
;;; test parameters
;;; ---------------------------------------------------------------------

(defparameter $test-list-id "38fee3007f2411ea984e38c9864ebde0")
(defparameter $test-words-path "/usr/share/dict/words")

;;; three simulated nodes
(defparameter $node1-identity #(82 105 143 117 23 188 75 91 172 25 23 13 150 103 56 162))
(defparameter $node2-identity #(127 122 180 42 98 5 72 151 132 131 244 32 228 72 191 248))
(defparameter $node3-identity #(180 244 225 2 56 135 64 81 129 214 65 199 171 98 224 245))

;;; three arbitrary but different origins
(defparameter $node1-origin (make-origin $node1-identity 12345 (pathname "/tmp/testpath1")))
(defparameter $node2-origin (make-origin $node2-identity 23456 (pathname "/tmp/testpath2")))
(defparameter $node3-origin (make-origin $node3-identity 34567 (pathname "/tmp/testpath3")))

;;; revision counters for each simulated origin
(defparameter $node1-revision -1)
(defparameter $node2-revision -1)
(defparameter $node3-revision -1)

;;; itemid counters for each simulated origin
(defparameter $itemid -1)

(defun make-test-list (list-path &key
                                   (count 100000)
                                   (list-name "Words Test"))
  (create-delectus-file list-path :listname list-name :listid $test-list-id :create-default-userdata nil)
  (let* ((word-column-label (make-column-label))
         (word-column-description (column-description
                                   :label word-column-label
                                   :name "Word"
                                   :order 10.0
                                   :title t
                                   :subtitle :false
                                   :deleted :false))
         (node-column-label (make-column-label))
         (node-column-description (column-description
                                   :label node-column-label
                                   :name "Node"
                                   :order 20.0
                                   :title :false
                                   :subtitle :false
                                   :deleted :false))
         (rev-column-label (make-column-label))
         (rev-column-description (column-description
                                  :label rev-column-label
                                  :name "Revision"
                                  :order 30.0
                                  :title :false
                                  :subtitle :false
                                  :deleted :false))
         (column-labels (list word-column-label node-column-label rev-column-label))
         (column-descriptions (list word-column-description node-column-description rev-column-description)))
    (with-open-database (db list-path)
      ;; create the columns
      (db-ensure-columns-exist db column-descriptions)
      ;; insert the listname op
      (db-insert-listname db :origin $node1-origin :timestamp (delectus-timestamp-now) :name list-name)
      ;; insert the columns op
      (db-insert-columns db :origin $node1-origin :timestamp (delectus-timestamp-now)
                         :column-descriptions column-descriptions)
      (with-open-file (in $test-words-path :direction :input)
        (loop for i from 0 and
           word = (read-line in nil nil nil)
           then (read-line in nil nil nil)
           while (and word
                      (< i count))
           ;; use the same itemid for all three nodes so we can assess
           ;; sorting up updates to the same item
           do (let ((itemid (incf $itemid)))
                ;; simulate a sync after every 10 percent of writes or so
                (when (< (random 100) 10)
                  (let ((synced-rev (1+ (max $node1-revision $node2-revision $node3-revision))))
                    (setf $node1-revision synced-rev)
                    (setf $node2-revision synced-rev)
                    (setf $node3-revision synced-rev)))
                (with-arbitrary-order ;; randomize the order of updates
                    ;; half the time insert from node1 with a lowercase copy
                    (when (first (shuffle [t nil]))
                      (let ((rev (incf $node1-revision)))
                        (sleep 0.002) ; spread out timesstamps a little
                        (db-insert-item db :origin $node1-origin :timestamp (delectus-timestamp-now) :itemid itemid
                                        :revision rev
                                        :column-values (alist->plist
                                                        (mapcar 'cons
                                                                column-labels
                                                                [(string-downcase word) "Node 1" rev])))))
                  ;; half the time insert from node2 with an uppercase copy
                  (when (first (shuffle [t nil]))
                    (let ((rev (incf $node2-revision)))
                      (sleep 0.002) ; spread out timesstamps a little
                      (db-insert-item db :origin $node2-origin :timestamp (delectus-timestamp-now) :itemid itemid
                                      :revision rev
                                      :column-values (alist->plist
                                                      (mapcar 'cons
                                                              column-labels
                                                              [(string-upcase word) "Node 2" rev])))))
                  ;; half the time insert from node3 with a capitalized copy
                  (when (first (shuffle [t nil]))
                    (let ((rev (incf $node3-revision)))
                      (sleep 0.002) ; spread out timesstamps a little
                      (db-insert-item db :origin $node3-origin :timestamp (delectus-timestamp-now) :itemid itemid
                                      :revision rev
                                      :column-values (alist->plist
                                                      (mapcar 'cons
                                                              column-labels
                                                              [(string-capitalize word) "Node 3" rev]))))))))))))



#| SQL to order latest items
   first version seems to get the best result in a mixed set of synced items
   Uses timestamp, so will be wrong if clock skew is too great between devices

SELECT ranked.* FROM (
  SELECT ROW_NUMBER() OVER ( PARTITION BY itemid ORDER BY timestamp DESC) rank, * 
  FROM `items`) ranked
where ranked.rank=1

index that speeds it up by about 20%:

CREATE INDEX `ix_itemid_timestamp` on `items`(`itemid`, `timestamp` DESC)

|#


;;; (setf $wordtest100-path "/Users/mikel/Desktop/wordtest100.delectus2")
;;; 0.8sec
;;; (time (make-test-list $wordtest100-path :count 100))
;;; (delete-file $wordtest100-path)

;;; (setf $wordtest1000-path "/Users/mikel/Desktop/wordtest1000.delectus2")
;;; 8sec:
;;; (time (make-test-list $wordtest1000-path :count 1000))
;;; (delete-file $wordtest1000-path)

;;; (setf $wordtest10k-path "/Users/mikel/Desktop/wordtest10k.delectus2")
;;; 1m49sec:
;;; (time (make-test-list $wordtest10k-path :count 10000))
;;; (delete-file $wordtest10k-path)

;;; (setf $wordtest100k-path "/Users/mikel/Desktop/wordtest100k.delectus2")
;;; 14m58sec:
;;; (time (make-test-list $wordtest100k-path :count 100000))
;;; (delete-file $wordtest100k-path)




