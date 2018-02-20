;;;; ***********************************************************************
;;;;
;;;; Name:          views-capi.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Lispworks CAPI views
;;;; Author:        mikel evins
;;;; Copyright:     2018 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; CLASS sqlite-window
;;; ---------------------------------------------------------------------
;;; a window used to inspect the contents of SQLite files

;;; callbacks
(defun handle-sqlite-table-selection (table-name sqlite-window)
  (let* ((controller (controller sqlite-window))
         (dbpath (dbpath controller))
         (column-names (mapcar #'second (sqlite-list-table-columns (dbpath (controller sqlite-window)) table-name)))
         (columns-pane (columns-pane sqlite-window)))
    (when column-names
      (setf (collection-items columns-pane)
            column-names))))

;;; interface definition
(define-interface sqlite-window ()
  ;; -- slots ---------------------------------------------
  ((controller :accessor controller :initform nil :initarg :controller))

  ;; -- panes ---------------------------------------------
  (:panes
   (tables-pane list-panel :reader tables-pane
                :selection-callback 'handle-sqlite-table-selection
                :callback-type :item-interface)
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
   (browser-layout column-layout '(tables-pane :divider columns-pane))
   (main-layout row-layout '(browser-layout :divider contents-pane)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :title "SQLite Utilities"
    :window-styles '(:textured-background)
    :width 800 :height 600))

(defmethod initialize-instance :after ((win sqlite-window) &rest initargs &key &allow-other-keys)
  (when (controller win)
    (when (dbpath (controller win))
        (let ((table-names (sqlite-list-tables (dbpath (controller win))))
              (tables-pane (tables-pane win)))
          (when table-names
            (setf (collection-items tables-pane)
                  table-names))))))

