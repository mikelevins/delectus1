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



(defmethod read-delectus-sexp-file ((path pathname))
  "Read a .sexp file (a Delectus 1.x file in s-expression format)"
  (let* ((filetype (pathname-type path)))
    (assert (equal "sexp" filetype)()
      "Expected a text file of type \".sexp\" but found ~S"
      (format nil "~A.~A" (pathname-name path)(pathname-type path)))
    (with-open-file (in path)
      (let* ((cl:*package* (find-package :delectus)))
        (read in)))))

(defmethod read-delectus-sexp-file ((path string))
  "Read a .sexp file (a Delectus 1.x file in s-expression format)"
  (read-delectus-sexp-file (pathname path)))

;;; (defparameter $data (read-delectus-sexp-file "/Users/mikel/Desktop/raw-delectus-data.sexp"))
