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
;;; INTERFACE sqlite-table-list
;;; ---------------------------------------------------------------------

(define-interface sqlite-table-list ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath))

  ;; -- panes ---------------------------------------------
  (:panes
   (contents-pane list-panel :reader contents-pane
                  :alternating-background t
                  :items (compute-sqlite-tables interface)))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(contents-pane)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 400 :height 400))

(defmethod  compute-sqlite-tables ((interface sqlite-table-list))
  (let ((path (dbpath interface)))
    (if path
        (delectus.data::sqlite-list-tables path)
        nil)))

;;; (defparameter $dbpath "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2")
;;; (defparameter $win (contain (make-instance 'sqlite-table-list :dbpath $dbpath)))

;;; ---------------------------------------------------------------------
;;; INTERFACE sqlite-column-list
;;; ---------------------------------------------------------------------

(define-interface sqlite-column-list ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (table-name :accessor table-name :initform nil :initarg :table-name))

  ;; -- panes ---------------------------------------------
  (:panes
   (contents-pane list-panel :reader contents-pane
                  :alternating-background t
                  :items (compute-sqlite-column-names interface)))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(contents-pane)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 400 :height 400))

(defmethod compute-sqlite-column-names ((interface sqlite-column-list))
  (let ((path (dbpath interface))
        (table-name (table-name interface)))
    (if path
        (if table-name
            (delectus.data::sqlite-list-table-column-names path table-name)
            nil)
        nil)))

;;; (defparameter $dbpath "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus2")
;;; (defparameter $win (contain (make-instance 'sqlite-column-list :dbpath $dbpath :table-name "contents")))
