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

(defun import-csv (csv-path list-path list-name
                   &key
                     (first-row-is-headers t)
                     (sharpf-is-nil t)
                     (list-id nil)
                     (origin nil))
  (assert (file-pathname-p csv-path) () "file does not exist: ~S" csv-path)
  (assert (stringp list-name) () "Expected a text string for LIST-NAME, not: ~S" list-name)
  (assert (not (probe-file list-path)) () "file exists: ~S" list-path)
  (let ((origin (or origin (make-origin (process-identity) (pathname list-path))))
        (first-line-vals (with-open-file (in csv-path :direction :input)
                           (fare-csv:read-csv-line in))))
    ;; first, make sure the source file exists; if not, we throw an error
    (when first-line-vals
      ;; create the output list
      (create-delectus-file list-path :listname list-name :listid (make-identity-string) :create-default-userdata nil)
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
            (db-insert-listname db :origin origin :timestamp (now-utc) :name list-name)
            ;; insert the columns op
            (db-insert-columns db :origin origin :timestamp (now-utc) :column-descriptions column-descriptions)
            ;; read and insert the csv rows
            ))))))

;;; (setf $csvdelectus-path "/Users/mikel/Desktop/csvtest.delectus2")
;;; (setf $bogus-path "/no/file/here.csv")
;;; (setf $wrong-path "/Users/mikel/.emacs")
;;; (import-csv $bogus-path $csvdelectus-path "Bogus")
;;; (import-csv $wrong-path $csvdelectus-path "Wrong format")

;;; (setf $movies-csv-path "/Users/mikel/Workshop/src/delectus/test-data/Movies.csv")
;;; (setf $movies-test-path "/Users/mikel/Desktop/Movies-test.delectus2")
;;; (import-csv $movies-csv-path $movies-test-path "Movies")
;;; (delete-file $movies-test-path)
