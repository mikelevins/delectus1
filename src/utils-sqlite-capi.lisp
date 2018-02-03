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

;;; ---------------------------------------------------------------------
;;; sqlite-controller
;;; ---------------------------------------------------------------------
;;; coordinates SQLite models with views that display and edit them

(defclass sqlite-controller ()
  ())

(defmethod handle-table-name-selected ((responder sqlite-controller) table-name table-list)
  (display-message "selected table: ~S" table-name))

;;; ---------------------------------------------------------------------
;;; table-list
;;; ---------------------------------------------------------------------

;;; FUNCTION tables-pane-selection-callback (item-data item interface)
;;; ---------------------------------------------------------------------
;;; signals the controller of the INTERFACE (if any) when a selection
;;; takes place

(defun tables-pane-selection-callback (item-data item table-list)
  (let ((responder (responder table-list)))
    (when responder
      (handle-table-name-selected responder item-data table-list))))

;;; CLASS table-list
;;; ---------------------------------------------------------------------
;;; displays a list of tables in a SQLite file

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
                :selected-item nil
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
