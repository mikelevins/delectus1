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
;;; Delectus data in s-expression format. We then read these sexp files
;;; and convert the data to Delectus 2.0.
;;;
;;; The data layout in sexp files is like this:
;;;
;;; :COLUMNS (
;;; ("Column Label" [DELETED])
;;; ... repeated for the number of columns
;;; )
;;; :ROWS
;;; ([T|NIL] value1 value2 ...)
;;; ... 
;;;
;;; The symbol DELETED appears in a column's cons if the column is marked deleted
;;; The first element of a row is T is the row is marked deleted, NIL otherwise
;;;
;;; The legacy code reads a file in this format and converts the data
;;; to Delectus 2 data suitable for storage in a SQLite file.

;;; convert-delectus-sexp-file ((path pathname) &optional (outpath nil))
;;; ---------------------------------------------------------------------
(defmethod convert-delectus-sexp-file ((path pathname) &optional (outpath nil))
  (let* ((in-basename (pathname-name path))
         (outpath (or outpath
                      (make-pathname :directory (pathname-directory path)
                                     :name (pathname-name path)
                                     :type "delectus2"))))
    (with-open-file (in path :direction :input)
      (let ((column-sentinel nil))
        ;; remove the :COLUMNS sentinel value
        (setf column-sentinel (read in))
        (assert (eql :COLUMNS column-sentinel)() "File format error: Expected :COLUMNS but found ~S"
          column-sentinel)
        ;; columns format:
        ;; (("Label" [:DELETED]) ...)
        (let ((columns (read in))
              ;; remove the :ROWS sentinel value
              (rows-sentinel (read in)))
          (assert (eql :ROWS rows-sentinel)() "File format error: Expected :ROWS but found ~S"
            rows-sentinel)
          (flet ((deleted-column? (col)(eql :deleted (second col))))
            ;; remove the "deleted" label; Delectus always creates that one automatically
            (let* ((column-labels (remove "deleted" (mapcar #'first columns) :test #'equal))
                   (deleted-labels (mapcar #'first (remove-if-not #'deleted-column? columns))))
              (create-delectus-file outpath)
              (with-open-database (db outpath)
                (with-transaction db
                  (dolist (lbl column-labels)
                    (execute-non-query db (format nil "ALTER TABLE \"contents\" ADD COLUMN ~S" lbl))
                    (execute-non-query db "insert into column_order (column_name) values (?)" lbl))
                  (dolist (lbl deleted-labels)
                    (execute-non-query db "insert into deleted_columns (column_name) values (?)" lbl))
                  ;; read and insert rows
                  (loop for row = (read in nil nil) then (read in nil nil)
                        while row
                        do (let* ((fields (mapcar (lambda (field)
                                                    (cond ((equal T field) "1")
                                                          ((equal NIL field) "0")
                                                          (t field)))
                                                  row))
                                  (insert-sql (format nil "insert into contents values (~{~s~^, ~})" fields)))
                             (execute-non-query db insert-sql))))))))))
    outpath))

(defmethod convert-delectus-sexp-file ((path string) &optional (outpath nil))
  (convert-delectus-sexp-file (pathname path) outpath))

;;; (convert-delectus-sexp-file "/Users/mikel/Desktop/junior-movies.sexp")
;;; (convert-delectus-sexp-file "/Users/mikel/Desktop/Movies.sexp")
