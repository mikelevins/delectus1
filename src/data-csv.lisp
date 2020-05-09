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

;;; convert for a misfeature of Delectus 1 CSV export:

(defun %sharpf->nil (thing)
  (if (equal thing "#f")
      nil
      thing))

(defun canonicalize (value-list &key (sharpf-is-nil t))
  (if sharpf-is-nil
      (mapcar #'%sharpf->nil value-list)
      value-list))

(defun import-csv (csv-path list-path list-name
                   &key
                     (first-row-is-headers t)
                     (sharpf-is-nil t)
                     (list-id nil))
  (assert (file-pathname-p csv-path) () "file does not exist: ~S" csv-path)
  (assert (stringp list-name) () "Expected a text string for LIST-NAME, not: ~S" list-name)
  (assert (not (probe-file list-path)) () "file exists: ~S" list-path)
  (let ((first-line-vals (with-open-file (in csv-path :direction :input)
                           (fare-csv:read-csv-line in))))
    ;; first, make sure the source file exists; if not, we throw an error
    (when first-line-vals
      ;; create the output list
      (create-delectus-file list-path :listname list-name
                            :listid (or list-id (make-identity-string))
                            :create-default-userdata nil)
      (with-open-database (db list-path)
        (with-transaction db
          ;; first create columns for the csv data
          (let* ((column-ids (loop for val in first-line-vals collect (make-identity-string)))
                 (column-names (if first-row-is-headers
                                   first-line-vals
                                   column-ids))
                 (column-orders (loop for i from 1 upto (length column-ids)
                                   collect (* i 10.0)))
                 (column-descriptions (mapcar (lambda (cid cname corder)
                                                (column-description
                                                 :id cid
                                                 :name cname
                                                 :order corder
                                                 :title :false
                                                 :subtitle :false
                                                 :deleted :false))
                                              column-ids column-names column-orders)))
            ;; create the columns
            (db-ensure-columns-exist db column-descriptions)
            ;; insert the listname op
            (db-insert-listname db :opid (makeid) :timestamp (delectus-timestamp-now) :name list-name)
            ;; insert the columns op
            (db-insert-columns db :opid (makeid) :timestamp (delectus-timestamp-now)
                               :column-descriptions column-descriptions)
            ;; read and insert the csv rows
            (with-open-file (in csv-path)
              ;; discard the first line if we used it for headers
              (when first-row-is-headers
                (fare-csv:read-csv-line in))
              (loop for
                 row = (canonicalize (fare-csv:read-csv-line in) :sharpf-is-nil sharpf-is-nil)
                 then (canonicalize (fare-csv:read-csv-line in) :sharpf-is-nil sharpf-is-nil)
                 while row
                 do (let* ((column-values (alist->plist (mapcar 'cons column-ids row))))
                      (db-insert-item db :opid (makeid) :timestamp (delectus-timestamp-now)
                                      :column-values column-values))))))))))

;;; (setf $movies-csv-path "/Users/mikel/Workshop/src/delectus/test-data/Movies.csv")
;;; (setf $movies-test-path "/Users/mikel/Desktop/Movies-test.delectus2")
;;; (delete-file $movies-test-path)
;;; (time (import-csv $movies-csv-path $movies-test-path "Movies"))
;;; (time (get-latest-items (pathname $movies-test-path)))
;;; (time (get-latest-items (pathname $movies-test-path) :offset 1000))

;;; (setf $zips-csv-path "/Users/mikel/Workshop/src/delectus/test-data/zipcode.csv")
;;; (setf $zips-test-path "/Users/mikel/Desktop/Zipcodes.delectus2")
;;; (delete-file $zips-test-path)
;;; (time (import-csv $zips-csv-path $zips-test-path "Zipcodes"))
;;; (time (get-latest-items (pathname $zips-test-path)))
;;; (time (get-latest-items (pathname $zips-test-path) :offset 30000))

