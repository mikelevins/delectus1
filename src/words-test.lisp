;;;; ***********************************************************************
;;;;
;;;; Name:          words-test.lisp
;;;; Project:       delectus 2
;;;; Purpose:       simulating concurrent edits
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; test parameters
;;; ---------------------------------------------------------------------

(defparameter $test-list-id "3TMC7FBK693PJ3CI00DUQAG05S")
(defparameter $test-words-path "/usr/share/dict/words")

;;; three simulated processes
(defparameter $processid-1 #(82 105 143 117 23 188 75 91 172 25 23 13 150 103 56 162))
(defparameter $processid-2 #(127 122 180 42 98 5 72 151 132 131 244 32 228 72 191 248))
(defparameter $processid-3 #(180 244 225 2 56 135 64 81 129 214 65 199 171 98 224 245))

;;; three arbitrary but different origins
(defparameter $origin1 (make-origin $processid-1 (pathname "/tmp/testpath1")))
(defparameter $origin2 (make-origin $processid-2 (pathname "/tmp/testpath2")))
(defparameter $origin3 (make-origin $processid-3 (pathname "/tmp/testpath3")))


(defun make-test-list (list-path &key
                                   (count 1000)
                                   (listname "Words Test"))
  (create-delectus-file list-path :listname listname :listid $test-list-id :create-default-userdata nil)
  (let* ((word-column-label (make-column-label))
         (word-column-description (column-description
                                   :label word-column-label
                                   :name "Word"
                                   :column-order 100.0
                                   :title t
                                   :subtitle :false
                                   :deleted :false))
         (origin-column-label (make-column-label))
         (origin-column-description (column-description
                                     :label origin-column-label
                                     :name "Origin"
                                     :column-order 200.0
                                     :title :false
                                     :subtitle :false
                                     :deleted :false))
         (column-descriptions (list word-column-description origin-column-description)))
    (with-open-database (db list-path)
      ;; create the columns
      (db-ensure-columns-exist db column-descriptions)
      ;; insert the listname op
      (let ((rev (db-get-next-revision db "listnames")))
        (db-insert-listname-op db :origin $origin1 :revision rev
                               :timestamp (delectus-timestamp-now) :listname listname))
      ;; insert the columns op
      (let ((rev (db-get-next-revision db "columns")))
        (db-insert-columns-op db :origin $origin1 :revision rev
                              :timestamp (delectus-timestamp-now) :columns column-descriptions))
      (let ((item-order 0.0d0)
            (origins [$origin1 $origin2 $origin3])
            (origin-vals ["Origin 1" "Origin 2" "Origin 3"])
            (origin-indexes [0 1 2])
            (edit-chances [nil t nil t nil t nil t nil nil])
            (edits-cache nil))
        (with-open-file (in $test-words-path :direction :input)
          (loop for i from 0 and
             word = (read-line in nil nil nil)
             then (read-line in nil nil nil)
             while (and word (< i count))
             do ;; have one of the nodes write the word
               (progn
                 (alexandria:shuffle origin-indexes)
                 (alexandria:shuffle edit-chances)
                 (let* ((origin-index (first origin-indexes))
                        (origin (elt origins origin-index))
                        (origin-val (elt origin-vals origin-index))
                        (itemid (makeid))
                        (field-values [word-column-label word origin-column-label origin-val])
                        (op-args [:origin origin
                                          :revision (db-get-next-revision db itemid)
                                          :itemid itemid
                                          :item-order (incf item-order *item-order-interval*)
                                          :timestamp (delectus-timestamp-now)
                                          :field-values field-values]))
                   (apply #'db-insert-item-op db op-args)
                   ;; push half of the ops onto the edits cache so we can update them at random moments
                   (when (evenp i)
                     (push op-args edits-cache)))
                 ;; about 40% of the time, pick one of the items in the edits cache and post it as a
                 ;; new edit of an old item
                 (when (first edit-chances)
                   (alexandria:shuffle origin-indexes)
                   (let* ((op-args (any edits-cache))
                          (old-field-vals (getf op-args :field-values))
                          (origin-index (first origin-indexes))
                          (origin (elt origins origin-index))
                          (itemid (getf op-args :itemid))
                          (rev (db-get-next-revision db itemid))
                          (field-values [(first old-field-vals)
                                         (concatenate 'string (second old-field-vals) " " (format nil "~A" rev))
                                         (third old-field-vals)
                                         (elt origin-vals origin-index)]))
                     (db-insert-item-op db
                                        :origin origin
                                        :revision rev
                                        :itemid itemid
                                        :item-order (getf op-args :item-order)
                                        :timestamp (delectus-timestamp-now)
                                        :field-values field-values))))))))))

;;; (setf $wordtest100-path (path "~/Desktop/wordtest100.delectus2"))
;;; 0.8sec:
;;; (time (make-test-list $wordtest100-path :count 100))
;;; (time (count-latest-items $wordtest100-path))
;;; (time (get-latest-items $wordtest100-path))
;;; (delete-file $wordtest100-path)

;;; (setf $wordtest1k-path (path "~/Desktop/wordtest1k.delectus2"))
;;; 1.7sec:
;;; (time (make-test-list $wordtest1k-path :count 1000))
;;; (time (count-latest-items $wordtest1k-path))
;;; (time (get-latest-items $wordtest1k-path))
;;; (delete-file $wordtest1k-path)

;;; (setf $wordtest10k-path (path "~/Desktop/wordtest10k.delectus2"))
;;; 12sec:
;;; (time (make-test-list $wordtest10k-path :count 10000))
;;; (time (count-latest-items $wordtest10k-path))
;;; (time (get-latest-items $wordtest10k-path :offset 9000))
;;; (delete-file $wordtest10k-path)

;;; (setf $wordtest100k-path (path "~/Desktop/wordtest100k.delectus2"))
;;; 2m45sec:
;;; (time (make-test-list $wordtest100k-path :count 100000))
;;; (time (count-latest-items $wordtest100k-path))
;;; (time (get-latest-items $wordtest100k-path :offset 98000))
;;; (delete-file $wordtest100k-path)
