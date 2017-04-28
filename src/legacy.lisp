;;;; ***********************************************************************
;;;;
;;;; Name:          legacy.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       reading Delectus 1.x files
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.legacy)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; The legacy.lisp code works together with a command-line utility
;;; called lecter, written in Gambit Scheme and compiled separately.
;;; lecter converts Delectus 1.x files to text files containing
;;; Delectus data in s-expression or csv format. We then read these
;;; files and convert the data to Delectus 2.0.
;;;
;;; The data layout in csv files is:
;;; header <newline> [row]*
;;; where header is:
;;; label[,label]*
;;; and row is
;;; field[,field]*
;;;
;;; The data layout in sexp files is like this:
;;;
;;; :COLUMNS ([label]*)
;;; :ROWS
;;; ([value]*)
;;; ... 
;;;
;;; The legacy code reads a file in this format and converts the data
;;; to Delectus 2 data suitable for storage in a SQLite file. It is an
;;; error if the number of fields in any row differs from the number
;;; of labels.

;;; sexp-val->delectus-val (val)
;;; ---------------------------------------------------------------------
;;; PRIVATE GENERIC FUNCTION
;;; converts VAL to a value that can be stored in a Delectus file,
;;; returning the converted value.

(defmethod sexp-val->delectus-val (val) val)

(defmethod sexp-val->delectus-val ((val symbol)) 
  (symbol-name val))

;;; convert-delectus-sexp-file (path &optional (outpath nil)
;;; ---------------------------------------------------------------------
;;; EXPORTED GENERIC FUNCTION
;;; creates a new Delectus file containing the converted contents of
;;; the file at PATH. The file at PATH must be in lecter's SEXP export
;;; format.

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

;;; convert-delectus-csv-file (path &optional (outpath nil))
;;; ---------------------------------------------------------------------
;;; EXPORTED GENERIC FUNCTION
;;; creates a new Delectus file containing the converted contents of
;;; the file at PATH. The file at PATH must be in lecter's CSV export
;;; format.

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
                    do (let* ((lbls column-labels)
                              (fields row)
                              (insert-sql (format nil "insert into contents (~{~s~^, ~}) values (~{~s~^, ~})" lbls fields)))
                         (execute-non-query db insert-sql))))))))))

(defmethod convert-delectus-csv-file ((path string) &optional (outpath nil))
  (convert-delectus-csv-file (pathname path) outpath))


;;; TEST CODE
;;; ---------------------------------------------------------------------

;;; shell: lecter --csv test-data/junior-movies.delectus > ~/Desktop/junior-movies.csv
;;; (time (convert-delectus-csv-file "/Users/mikel/Desktop/junior-movies.csv" "/Users/mikel/Desktop/junior-movies.delectus2"))

;;; shell: lecter --csv test-data/Movies.delectus > ~/Desktop/Movies.csv
;;; (time (convert-delectus-csv-file "/Users/mikel/Desktop/Movies.csv" "/Users/mikel/Desktop/Movies.delectus2"))
;;; (time (convert-delectus-sexp-file "/Users/mikel/Desktop/Movies.sexp" "/Users/mikel/Desktop/Movies.delectus2"))

;;; (time (convert-delectus-csv-file "/Users/mikel/Desktop/zipcode_20k.csv" "/Users/mikel/Desktop/zipcode.delectus2"))
;;; (time (convert-delectus-csv-file "/Users/mikel/Workshop/src/delectus/test-data/zip_codes_states.csv" "/Users/mikel/Desktop/zip_codes_states.delectus2"))
