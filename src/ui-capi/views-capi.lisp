;;;; ***********************************************************************
;;;;
;;;; Name:          views-capi.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Lispworks CAPI views
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.desktop)

;;; ---------------------------------------------------------------------
;;; INTERFACE sqlite-browser
;;; ---------------------------------------------------------------------
;;; the sqlite-browser is a utility for browsing the contents of
;;; arbitrary SQLite files. It is not the Delectus2 user interface;
;;; it's a tool for inspecting SQLite files.

(define-interface sqlite-browser ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (table-name :accessor table-name :initform nil :initarg :table-name)
   (rows-per-page :accessor rows-per-page :initform 10 :initarg :rows-per-page)
   (current-page :accessor current-page :initform 0 :initarg :current-page))

  ;; -- panes ---------------------------------------------
  (:panes
   (tables-pane list-panel :reader tables-pane
                :alternating-background t
                :items (compute-sqlite-tables interface)))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(tables-pane)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 600 :height 400
    :title "SQLite Browser"))

(defmethod initialize-instance :after ((browser sqlite-browser) &rest initargs &key &allow-other-keys)
  ;; the browser starts with nothing selected and waits for the user to make a selection
  (setf (choice-selection (tables-pane browser))
        nil))

(defmethod compute-sqlite-tables ((interface sqlite-browser))
  (let ((dbpath (dbpath interface)))
    (if dbpath
        (delectus.data::sqlite-list-tables dbpath)
      nil)))

;;; (defparameter $dbpath "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2")
;;; (defparameter $win (contain (make-instance 'sqlite-browser :dbpath $dbpath)))
