;;;; ***********************************************************************
;;;;
;;;; Name:          legacy.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       reading Delectus 1.x files
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; lecter converts Delectus 1.x files to text files containing
;;; (non-deleted) Delectus data in s-expression format. We then read
;;; these sexp files and convert the data to Delectus 2.0.
;;;
;;; The data layout in sexp files is like this:
;;;
;;; :COLUMNS (
;;; "Column Label"
;;; ... repeated for the number of columns
;;; )
;;; :ROWS
;;; (value1 value2 ...)
;;; ... 
;;;
;;; The legacy code reads a file in this format and converts the data
;;; to Delectus 2 data suitable for storage in a SQLite file.

(defmethod sexp-val->delectus-val (val) val)

(defmethod sexp-val->delectus-val ((val symbol)) 
  (symbol-name val))

;;; convert-delectus-sexp-file ((path pathname) &optional (outpath nil))
;;; ---------------------------------------------------------------------
(defmethod convert-delectus-sexp-file ((path pathname) &optional (outpath nil))
  (let* ((outpath (or outpath
                      (make-pathname :directory (pathname-directory path)
                                     :name (pathname-name path)
                                     :type "delectus2"))))
    (with-open-file (in path :direction :input)
      (let ((sentinel nil))
        ;; 1. check the file-format sentinels
        ;; remove the :DELECTUS sentinel
        (setf sentinel (read in))
        (assert (eql :DELECTUS sentinel)() "File format error: Expected :DELECTUS but found ~S" sentinel)
        ;; remove the :SEXP sentinel
        (setf sentinel (read in))
        (assert (eql :SEXP sentinel)() "File format error: Expected :SEXP but found ~S" sentinel)
        ;; remove the :COLUMNS sentinel
        (setf sentinel (read in))
        (assert (eql :COLUMNS sentinel)() "File format error: Expected :COLUMNS but found ~S" sentinel)
        ;;
        ;; 2. read columns
        (let ((columns (read in)))
          ;; remove the :ROWS sentinel
          (setf sentinel (read in))
          (assert (eql :ROWS sentinel)() "File format error: Expected :ROWS but found ~S" sentinel)
          ;; 3. Read and convert rows
          (let* ((column-labels columns))
            (create-delectus-file outpath column-labels)
            (with-open-database (db outpath)
              (with-transaction db
                ;; read and insert rows
                (loop for row = (read in nil nil nil) then (read in nil nil nil)
                      while row
                      do (let* ((fields (mapcar #'sexp-val->delectus-val row))
                                (insert-sql (format nil "insert into contents (~{~s~^, ~}) values (~{~s~^, ~})" column-labels fields)))
                           (execute-non-query db insert-sql)))))))))
    outpath))

(defmethod convert-delectus-sexp-file ((path string) &optional (outpath nil))
  (convert-delectus-sexp-file (pathname path) outpath))

;;; (convert-delectus-sexp-file "/Users/mikel/Desktop/junior-movies.sexp")
;;; (convert-delectus-sexp-file "/Users/mikel/Desktop/Movies.sexp")

;;; convert-delectus-csv-file ((path pathname) &optional (outpath nil))
;;; ---------------------------------------------------------------------
;;; TODO: decide how to handle edge cases in case we want to import arbitrary CSV:
;;;  1. how do we know whether to treat the first row as column labels?
(defmethod convert-delectus-csv-file ((path pathname) &optional (outpath nil))
  (let* ((outpath (or outpath
                      (make-pathname :directory (pathname-directory path)
                                     :name (pathname-name path)
                                     :type "delectus2"))))
    (with-open-file (in path :direction :input)
      (with-rfc4180-csv-syntax ()
        (let ((column-labels (read-csv-line in)))
          (create-delectus-file outpath column-labels)
          (with-open-database (db outpath)
            (with-transaction db
              ;; read and insert rows
              (loop for row = (read-csv-line in) then (read-csv-line in)
                    while row
                    do (let* ((lbls column-labels) ; add the "deleted" column
                              (fields row) ; add a false for the "deleted" column
                              (insert-sql (format nil "insert into contents (~{~s~^, ~}) values (~{~s~^, ~})" lbls fields)))
                         (execute-non-query db insert-sql))))))))))

(defmethod convert-delectus-csv-file ((path string) &optional (outpath nil))
  (convert-delectus-csv-file (pathname path) outpath))

;;; shell: lecter --csv test-data/junior-movies.delectus > ~/Desktop/junior-movies.csv
;;; (time (convert-delectus-csv-file "/Users/mikel/Desktop/junior-movies.csv" "/Users/mikel/Desktop/junior-movies.delectus2"))
;;; shell: lecter --csv test-data/Movies.delectus > ~/Desktop/Movies.csv
;;; (time (convert-delectus-csv-file "/Users/mikel/Desktop/Movies.csv" "/Users/mikel/Desktop/Movies.delectus2"))
