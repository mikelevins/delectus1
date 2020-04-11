;;;; ***********************************************************************
;;;;
;;;; Name:          csv.lisp
;;;; Project:       delectus 2
;;;; Purpose:       I/O operations on csv files
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

(defun read-csv (path)
  (fare-csv:with-rfc4180-csv-syntax ()
    (let* ((fare-csv:*separator* #\,))
      (fare-csv:read-csv-file path))))

;;; (defparameter $csvpath "/Users/mikel/Workshop/src/delectus/test-data/zipcode.csv")
;;; (time (defparameter $data (read-csv $csvpath)))
;;; (length $data)
;;; (elt $data 0)
;;; (elt $data 43191)

;;; ---------------------------------------------------------------------
;;; importing csv data into a Delectus list
;;; ---------------------------------------------------------------------

(defun import-csv (csv-path list-path list-name
                   &key
                     (first-row-is-headers t)
                     (list-id nil)
                     (origin *origin*))
  (assert (file-pathname-p csv-path) () "file does not exist: ~S" csv-path)
  (assert (stringp list-name) () "Expected a text string for LIST-NAME, not: ~S" list-name)
  (assert (not (probe-file list-path)) () "file exists: ~S" list-path)
  (let ((list-id (or list-id (makeid)))
        (csv-data (read-csv csv-path)))
    (if (null csv-data)
        (error "No data in file: ~S" csv-path)
        (let* ((column-ids (loop for i from 0 below (length (first csv-data))
                              collect (makeid)))
               (column-names (if first-row-is-headers
                                 (first csv-data)
                                 column-ids))
               (item-data (if first-row-is-headers
                              (rest csv-data)
                              csv-data)))
          ;; create the list file
          (create-delectus-file list-path list-name list-id :create-default-userdata nil)
          ;; populate it from the csv data:
          ;; first, give them each an ID
          (let* ((coldata-with-ids (loop for cid in column-ids
                                      collect (fset:with +default-initial-column-attributes+
                                                         :|id| cid)))
                 ;; next, give each on a name
                 (coldata-with-names (loop
                                        for cdata in coldata-with-ids
                                        and nm in column-names
                                        collect (fset:with cdata
                                                           :|name| nm)))
                 ;; next, give each one a distinct order
                 (coldata-with-orders (loop
                                         for i from 1 upto (length coldata-with-names)
                                         and cdata in coldata-with-names
                                         collect (fset:with cdata :|order| (* 10.0 i))))
                 ;; next, ensure that exactly one is marked as the title column
                 (coldata-with-title-flags (cons (fset:with (first coldata-with-orders)
                                                            :|title| t)
                                                 (loop for cdata in (rest coldata-with-orders)
                                                    collect (fset:with cdata :|title| :false)))))
            (with-open-database (db list-path)
              (with-transaction db
                ;; - first, add the userdata columns
                (let ((opid (makeid))
                      (columns-rev (db-get-next-revision db))
                      (ts (now-timestamp)))
                  (db-assert-columns db :opid opid :origin origin :revision columns-rev :timestamp ts
                                     :column-data coldata-with-title-flags))
                ;; - next, insert the items
                (loop for item-values in item-data
                   do (let ((opid (makeid))
                            (item-rev (db-get-next-revision db))
                            (ts (now-timestamp))
                            (itemid (makeid)))
                        (db-assert-item db :opid opid :origin origin :revision item-rev :timestamp ts
                                        :item itemid :deleted nil
                                        :column-data coldata-with-title-flags
                                        :column-values item-values))))))))))

;;; (defparameter $csvpath "/Users/mikel/Workshop/src/delectus/test-data/zipcode.csv")
;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; this takes a minute and a half
;;; and yields a Delectus db 8.5 MB in size
;;; (time (import-csv $csvpath $zippath "Zipcodes"))

