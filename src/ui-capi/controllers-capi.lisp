;;;; ***********************************************************************
;;;;
;;;; Name:          controllers-capi.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       controllers for Lispworks CAPI views
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.desktop)


;;; ---------------------------------------------------------------------
;;; CLASS sqlite-database-controller
;;; ---------------------------------------------------------------------
;;; a controller used to inspect the contents of SQLite files

(defclass sqlite-database-controller ()
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath) ; the database file to inspect
   (window :accessor window :initform nil) ; the window to manage; see initialize-instance :after
   ))

(defmethod initialize-instance :after ((controller sqlite-database-controller) &rest initargs &key &allow-other-keys)
  (setf (window controller)
        (make-instance 'sqlite-window :controller controller)))

;;; (defparameter $moviesdb "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2")
;;; (defparameter $controller (make-instance 'sqlite-database-controller :dbpath $moviesdb))
;;; (contain (window $controller))
