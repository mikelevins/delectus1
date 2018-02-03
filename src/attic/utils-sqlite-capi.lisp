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

(define-interface sqlite-window ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath))

  ;; -- panes ---------------------------------------------
  (:panes
   (tables-pane list-panel :reader tables-pane)
   (columns-pane list-panel :reader columns-pane)
   (contents-pane multi-column-list-panel :reader contents-pane
                  :columns '((:title "Fruits" 
                              :adjust :right 
                              :width (character 15))
                             (:title "Vegetables" 
                              :adjust :left 
                              :visible-min-width (character 30)))
                  :alternating-background t
                  :auto-reset-column-widths nil))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (contents-layout column-layout '(columns-pane contents-pane))
   (main-layout row-layout '(tables-pane contents-layout)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :window-styles '(:textured-background)
    :width 800 :height 600))

;;; (contain (make-instance 'sqlite-window))
