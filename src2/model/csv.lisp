;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          csv.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       conversion to and from csv format
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defparameter $csv-read-buffer-size 128)

(defmethod read-csv ((path pathname))
  (let ((rows nil))
    (csv-parser:do-csv-file ((fields num-fields) path)
      (push (make-delectus fields) rows))
    (make-delectus (reverse rows))))

(defmethod read-csv ((path string))
  (read-csv (pathname path)))


