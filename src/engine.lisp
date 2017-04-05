;;;; ***********************************************************************
;;;;
;;;; Name:          engine.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the Delectus 2 storage engine
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; store
;;; ---------------------------------------------------------------------
;;; a class that represents data storage

;;; store
;;; ---------------------------------------------------------------------
(defclass store ()
  (;; a reference to the database in which data are stored
   (db-handle :reader db-handle :initform nil :initarg :db-handle)
   ;; the pathname of the backing file, or ":memory:" if it's an in-memory db
   (db-path :reader db-path :initform nil :initarg :db-path)))

(defmethod initialize-instance :after ((store store) &rest initargs &key &allow-other-keys)
  (if (db-path store)
      ;; create the database file
      ()
    ;; create an in-memory database
    ()))