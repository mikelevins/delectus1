;;;; ***********************************************************************
;;;;
;;;; Name:          test-data.lisp
;;;; Project:       delectus 2
;;;; Purpose:       test databases for testing
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)
(in-readtable :delectus)

(defparameter $test-list-id "I38fee3007f2411ea984e38c9864ebde0")

(defparameter $nodeid1 "Ia78874807f1511ea984e38c9864ebde0")
(defparameter $nodeid2 "Ib4a563807f1511ea984e38c9864ebde0")
(defparameter $nodeid3 "Ic129bc007f1511ea984e38c9864ebde0")

(defparameter $words-path "/usr/share/dict/words")

(defparameter $test-path "/Users/mikel/Desktop/wordtest.delectus2")

(defun make-test-list (&key (count 100000))
  (create-delectus-file $test-path "Words Test" $test-list-id)
  (with-open-file (in $words-path :direction :input)
    (with-open-database (db $test-path)
      (let* ((column-info (db-get-userdata-column-info db))
             (item-column-info (first column-info))
             (item-column-id (column-info-name item-column-info))
             (userdata-column-data (list (with +default-initial-column-attributes+ :|id| item-column-id))))
        (loop for i from 0 below count
              do (let* ((itemid (makeid))
                        (line (read-line in nil nil nil))
                        ;; reuse revs per-loop to make multiple entries concurrent
                        (rev (db-get-next-revision db)))
                   ;; always add an entry from node1
                   (db-assert-item db :opid (makeid) :origin $nodeid1 :revision rev
                                   :timestamp (now-timestamp) :column-data userdata-column-data
                                   :item itemid
                                   ;; node1 entries are always lowercase
                                   :column-values (list (string-downcase line)))
                   ;; if i is even, add an entry from node2
                   (when (evenp i)
                     (db-assert-item db :opid (makeid) :origin $nodeid2 :revision rev
                                     :timestamp (now-timestamp) :column-data userdata-column-data
                                     :item itemid
                                     ;; node2 entries are always uppercase
                                     :column-values (list (string-upcase line))))
                   ;; if i is a whole multiple of 4, add an entry from node3
                   (when (zerop (mod i 4))
                     (db-assert-item db :opid (makeid) :origin $nodeid3 :revision rev
                                     :timestamp (now-timestamp) :column-data userdata-column-data
                                     :item itemid
                                     ;; node3 entries are always Capitalized
                                     :column-values (list (string-capitalize line))))))
        $test-path))))

;;; 1000 items
;;; 455kB
;;; ~3sec to create
;;; (time (make-test-list :count 1000))
;;; ~0.02sec to get
;;; (time (progn (get-latest-items $test-path) 'done))

;;; 10000 items
;;; 4.4MB
;;; ~33sec to create
;;; (time (make-test-list :count 10000))
;;; ~0.14sec to get
;;; (time (progn (get-latest-items $test-path) 'done))

;;; 100000 items
;;; 45MB
;;; ~9m20sec to create
;;; (time (make-test-list :count 100000))
;;; ~1.5sec to get
;;; (time (progn (get-latest-items $test-path) 'done))

