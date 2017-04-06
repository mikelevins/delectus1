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
   (db-path :reader db-path :initform nil :initarg :db-path)))

;;; TODO: currently it squeals if the file exists already
;;;       what we want to do is peek at the file to see if it's
;;;       a delectus2 file; if it is, just go ahead and return the
;;;       store; if it isn't, then squeal
(defmethod initialize-instance :after ((store store) &rest initargs &key &allow-other-keys)
  (assert (db-path store)() "Creating a store requires that you supply a pathname for a store file")
  (with-open-database (db (db-path store))
    (with-transaction db
      (execute-non-query db "create table delectus (format_version integer)")
      (execute-non-query db "insert into delectus (format_version) values (?)" +delectus-format-version+)
      (execute-non-query db "create table contents (rowid integer primary key) without rowid"))))
