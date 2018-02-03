;;;; ***********************************************************************
;;;;
;;;; Name:          utils-sqlite.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       utilities for operating on sqlite files
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defmethod ensure-valid-sqlite-file ((path pathname))
  (let ((path (probe-file path)))
    (and path
         (uiop/pathname:file-pathname-p path)
         (with-open-database (db path)
           (execute-non-query db "pragma schema_version"))
         path)))

(defmethod ensure-valid-sqlite-file ((path string))
  (ensure-valid-sqlite-file (pathname path)))

;;; should return the pathname because it's a valid sqlite file:
;;; (ensure-valid-sqlite-file "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2")
;;; should return NIL because it isn't:
;;; (ensure-valid-sqlite-file "/Users/mikel/.emacs")
;;; should return NIL because it doesn't exist:
;;; (ensure-valid-sqlite-file "/Users/brobdingnag/.emacs")
