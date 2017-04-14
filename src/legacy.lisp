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
;;; :LABEL <column label string> :DELETED [T | NIL]
;;; ... repeated for the number of columns
;;; )
;;; :ROWS
;;; (:DELETED [T | NIL] :VALUE <value string or NIL> ... repeated for the number of values in the row)
;;; ... repeated for the number of rows
;;;
;;; The legacy code reads a file in this format and converts the data
;;; to Delectus 2 data suitable for storage in a SQLite file.

(defmethod read-delectus-sexp-file0 ((path pathname))
  "Read a .sexp file (a Delectus 1.x file in s-expression format)"
  (let* ((filetype (pathname-type path)))
    (assert (equal "sexp" filetype)()
            "Expected a text file of type \".sexp\" but found ~S"
            (format nil "~A.~A" (pathname-name path)(pathname-type path)))
    (with-open-file (in path)
      (let ((column-sentinel nil)
            (columns nil)
            (rows nil)
            (row-sentinel nil))
        ;; read columns
        ;; read rows
        ;; TODO: if the file is really big then it might not
        ;;       be a good idea to read all the rows at once
        ;;       rewrite the reader to read a row at a time
        ;;       in an SQLite transaction, adding each row to
        ;;       the store as we read it
        (setf column-sentinel (read in))
        (assert (eql :COLUMNS column-sentinel)() "Expected :COLUMNS but found ~S" column-sentinel)
        (setf columns (read in))
        (setf row-sentinel (read in))
        (assert (eql :ROWS row-sentinel)() "Expected :ROWS but found ~S" row-sentinel)
        (setf rows (loop for row = (read in nil) then (read in nil) while row collect row))
        (list :columns columns :rows rows)))))

(defmethod read-delectus-sexp-file ((path string))
  "Read a .sexp file (a Delectus 1.x file in s-expression format)"
  (read-delectus-sexp-file (pathname path)))

;;; (defparameter $data (read-delectus-sexp-file "/Users/mikel/Desktop/raw-delectus-data.sexp"))

(defmethod convert-delectus-sexp-file ((path pathname) &optional (outpath nil))
  (let* ((in-basename (pathname-name path))
         (outpath (or outpath
                      (make-pathname :directory (pathname-directory path)
                                     :name (pathname-name path)
                                     :type "delectus2"))))
    (with-open-file (in path :direction :input)
      (let ((column-sentinel nil))
        (setf column-sentinel (read in))
        (assert (eql :COLUMNS column-sentinel)() "File format error: Expected :COLUMNS but found ~S"
                column-sentinel)
        ;; columns format:
        ;; (:LABEL "Title" :DELETED NIL :LABEL "Star" :DELETED NIL :LABEL "Costar" :DELETED NIL ...)
        (let ((columns (read in))
              (rows-sentinel (read in)))
          (assert (eql :ROWS rows-sentinel)() "File format error: Expected :ROWS but found ~S"
                  rows-sentinel)
          (let* ((stripped (loop for x in (cdr columns) by #'cddr collect x))
                 (pairs (loop for tail on stripped by #'cddr
                           collect (cons (car tail)(cadr tail))))
                 (deleted-pairs (remove-if-not #'(lambda (pair)(cdr pair))
                                               pairs))
                 (column-labels (cons "deleted" (mapcar #'car pairs)))
                 (deleted-labels (mapcar #'car deleted-pairs))
                 (create-contents-sql (format nil "create table contents (~{~a~^, ~})"
                                              column-labels)))
            (with-open-database (db outpath)
              (with-transaction db
                ;; table: delectus - identifies format version
                (execute-non-query db "create table delectus (format_version integer)")
                (execute-non-query db "insert into delectus (format_version) values (?)" +delectus-format-version+)
                ;; table: contents - stores document data
                (execute-non-query db create-contents-sql)
                ;; table: column_order - stores the user-defined column order
                (execute-non-query db "create table column_order (column_name string)")
                (dolist (lbl column-labels)
                  (execute-non-query db "insert into column_order (column_name) values (?)" lbl))
                ;; table: deleted_columns - stores labels of columns that are present but marked deleted
                (execute-non-query db "create table deleted_columns (column_name string)")
                (dolist (lbl deleted-labels)
                  (execute-non-query db "insert into deleted_columns (column_name) values (?)" lbl))
                ;; read and insert rows
                (loop for row = (read in nil nil) then (read in nil nil)
                   while row
                   do (let* ((fields (mapcar #'(lambda (f)
                                                 (cond ((not f) "0")
                                                       ((eql f t) "1")
                                                       (t f)))
                                             (loop for x in (cdr row) by #'cddr collect x)))
                             (insert-sql (format nil "insert into contents values (~{~s~^, ~})" fields)))
                        (execute-non-query db insert-sql)))))))))))

(defmethod convert-delectus-sexp-file ((path string) &optional (outpath nil))
  (convert-delectus-sexp-file (pathname path) outpath))
