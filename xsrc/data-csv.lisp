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
                     (listid nil))
  ;; first, make sure the source file exists; if not, we throw an error
  (assert (file-pathname-p csv-path) () "file does not exist: ~S" csv-path)
  ;; next, make sure we have a name for the list
  (assert (stringp list-name) () "Expected a text string for LIST-NAME, not: ~S" list-name)
  ;; now, check to see if there's a file in the way of the output
  (assert (not (probe-file list-path)) () "file exists: ~S" list-path)
  ;; finally, read the csv and write the data to the list file
  (let ((first-line-vals (with-open-file (in csv-path :direction :input)
                           (fare-csv:read-csv-line in))))
    (when first-line-vals
      ;; create the output list
      (create-delectus-file list-path
                            :listname list-name
                            :listid (or listid (make-identity-string))
                            ;; don't insert the default data; we have real data to insert
                            :create-default-userdata nil)
      (with-open-database (db list-path)
        (with-transaction db
          ))))))))))



;;; (setf $movies-csv-path (path "~/Workshop/src/delectus/test-data/Movies.csv"))
;;; (setf $movies-test-path (path "~/Desktop/Movies.delectus2"))
;;; (delete-file $movies-test-path)
;;; (time (import-csv $movies-csv-path $movies-test-path "Movies"))
;;; (time (get-latest-items (pathname $movies-test-path)))
;;; (time (get-latest-items (pathname $movies-test-path) :offset 1000))

;;; (setf $zips-csv-path (path "~/Workshop/src/delectus/test-data/zipcode.csv"))
;;; (setf $zips-test-path (path "~/Desktop/Zipcodes.delectus2"))
;;; (delete-file $zips-test-path)
;;; (time (import-csv $zips-csv-path $zips-test-path "Zipcodes"))
;;; (time (get-latest-items (pathname $zips-test-path)))
;;; (time (get-latest-items (pathname $zips-test-path) :offset 30000 :limit 10))

;;; (setf $words-csv-path "/usr/share/dict/words")
;;; (setf $words-test-path (path "~/Desktop/words.delectus2"))
;;; (delete-file $words-test-path)
;;; 1m, 10.1MB
;;; (time (import-csv $words-csv-path $words-test-path "Words" :first-row-is-headers nil))
;;; (time (get-latest-items (pathname $words-test-path) :offset 235000 :limit 10))

