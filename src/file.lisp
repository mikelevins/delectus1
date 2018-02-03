;;;; ***********************************************************************
;;;;
;;;; Name:          file.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       managing database files
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;;---------------------------------------------------------------------
;;; CLASS sqlite-file
;;;---------------------------------------------------------------------

(defclass sqlite-file ()
  ((path :accessor path :initform nil :initarg :path)))

(defmethod sqlite-file-valid? ((file sqlite-file))
  (ensure-valid-sqlite-file (path file)))
