;;;; ***********************************************************************
;;;;
;;;; Name:          data-csv.lisp
;;;; Project:       delectus 2
;;;; Purpose:       reading and writing csv files
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;; importing csv data into a Delectus list
;;; ---------------------------------------------------------------------

(defun %val->JSON-string (val)
  (if (equal val "#f") ; correct a misfeature of Delectus 1 CSV export
      (jonathan:to-json :false)
      (jonathan:to-json val)))

;;; convert a row of values to the corresponding JSON strings
(defun canonicalize (value-list &key (sharpf-is-nil t))
  (mapcar #'%val->JSON-string
          value-list))

;;; (setf $movies-csv-path (path "~/Workshop/src/delectus/test-data/Movies.csv"))
;;; (setf $csv-vals (with-open-file (in $movies-csv-path)(fare-csv:read-csv-line in)(fare-csv:read-csv-line in)))
;;; (canonicalize $csv-vals)
;;; (mapcar #'jonathan:parse (canonicalize $csv-vals))

(defun import-csv (csv-path list-path list-name
                   &key
                     (comment "")
                     (first-row-is-headers t)
                     (sharpf-is-nil t)
                     (listid nil))
  ;; first, make sure the source file exists; if not, we throw an error
  (assert (file-pathname-p csv-path) () "file does not exist: ~S" csv-path)
  (assert (stringp list-name) () "Expected a text string for LIST-NAME, not: ~S" list-name)
  (assert (not (probe-file list-path)) () "file exists: ~S" list-path)
  (let ((first-line-vals (with-open-file (in csv-path :direction :input)
                           (fare-csv:read-csv-line in))))
    (when first-line-vals
      ;; create the output list
      (create-delectus-file list-path
                            :listname list-name
                            :listid (or listid (make-identity-string))
                            :create-default-userdata nil)
      (with-open-database (db list-path)
        (with-transaction db
          ;; first create columns for the csv data
          (let* ((origin (make-origin (process-identity)
                                      (pathname list-path)))
                 (column-labels (mapcar (lambda (val)
                                          (declare (ignore val))
                                          (make-column-label))
                                        first-line-vals))
                 (column-names (if first-row-is-headers
                                   first-line-vals
                                   column-labels))
                 (column-orders (loop for i from 1 upto (length column-labels)
                                   collect (* i *column-order-interval*)))
                 (column-descriptions (mapcar (lambda (lbl cname corder)
                                                (column-description
                                                 :label lbl
                                                 :name cname
                                                 :column-order corder
                                                 :title :false
                                                 :subtitle :false
                                                 :deleted :false))
                                              column-labels column-names column-orders)))
            ;; create the columns
            (db-ensure-columns-exist db column-descriptions)
            ;; insert the listname op
            (db-insert-listname-op db :origin origin :revision (db-get-next-revision db "listnames")
                                   :timestamp (delectus-timestamp-now) :listname list-name)
            ;; insert the comment op
            (db-insert-comment-op db :origin origin :revision (db-get-next-revision db "comments")
                                  :timestamp (delectus-timestamp-now) :comment comment)
            ;; insert the columns op
            (db-insert-columns-op db :origin origin :revision (db-get-next-revision db "columns")
                                   :timestamp (delectus-timestamp-now) :columns column-descriptions)
            ;; read and insert the csv rows
            (with-open-file (in csv-path)
              ;; discard the first line if we used it for headers
              (when first-row-is-headers
                (fare-csv:read-csv-line in))
              (let ((item-order 0.0))
                (loop for
                   row = (canonicalize (fare-csv:read-csv-line in) :sharpf-is-nil sharpf-is-nil)
                   then (canonicalize (fare-csv:read-csv-line in) :sharpf-is-nil sharpf-is-nil)
                   while row
                   do (let* ((field-values (alist->plist (mapcar 'cons column-labels row))))
                        (db-insert-item-op db :origin origin
                                           :revision (db-get-next-revision db "columns")
                                           :itemid (makeid)
                                           :item-order (incf item-order *item-order-interval*)
                                           :timestamp (delectus-timestamp-now)
                                           :field-values field-values)))))))))))



;;; (setf $movies-csv-path (path "~/Workshop/src/delectus/test-data/Movies.csv"))
;;; (setf $movies-test-path (path "~/Desktop/Movies.delectus2"))
;;; (delete-file $movies-test-path)
;;; 0.174sec:
;;; (time (import-csv $movies-csv-path $movies-test-path "Movies"))
;;; (time (get-latest-items (pathname $movies-test-path)))
;;; (time (get-latest-items (pathname $movies-test-path) :offset 1000))

;;; (setf $zips-csv-path (path "~/Workshop/src/delectus/test-data/zipcode.csv"))
;;; (setf $zips-test-path (path "~/Desktop/Zipcodes.delectus2"))
;;; (delete-file $zips-test-path)
;;; 5.1sec:
;;; (time (import-csv $zips-csv-path $zips-test-path "Zipcodes"))
;;; (time (get-latest-items (pathname $zips-test-path)))
;;; (time (get-latest-items (pathname $zips-test-path) :offset 30000 :limit 10))


