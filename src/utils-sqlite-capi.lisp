;;;; ***********************************************************************
;;;;
;;;; Name:          utils.sqlite-capi.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       visualization UI for sqlite files
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus.capi)

(defun tables-pane-selection-callback (item-data item interface)
  (display-message "item-data: ~S, item: ~S, interface: ~S"
                   item-data item interface))

(define-interface table-list ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (responder :accessor responder :initform nil :initarg :responder))

  ;; -- panes ---------------------------------------------
  (:panes
   (tables-pane list-panel :reader tables-pane
                :alternating-background t
                :items (compute-tables interface)
                :interaction :single-selection
                :callback-type :full
                :selection-callback 'tables-pane-selection-callback))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(tables-pane)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :window-styles '(:textured-background)
    :width 600 :height 600))

(defmethod compute-tables ((pane table-list))
  (delectus.sqlite::list-tables (dbpath pane)))

;;; (contain (make-instance 'table-list :dbpath delectus.sqlite::$moviesdb))
