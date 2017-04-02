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

(defmethod read-delectus-sexp-file ((path pathname))
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
