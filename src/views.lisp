;;;; ***********************************************************************
;;;;
;;;; Name:          views.lisp
;;;; Project:       delectus 2
;;;; Purpose:       UI viwes of delectus daata
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

(define-interface list-items-pane ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (rows-per-page :accessor rows-per-page :initform 20 :initarg :rows-per-page)
   (current-page :accessor current-page :initform 0 :initarg :current-page))

  ;; -- panes ---------------------------------------------
  (:panes
   (items-pane multi-column-list-panel :reader items-pane
              :alternating-background t
              :items nil
              :columns '((:title "Item"))
              :callback-type :item-interface
              :selection-callback 'handle-item-selection))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(items-pane)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 600 :height 400
    :title "Delectus"))

(defmethod initialize-instance :after ((pane list-items-pane) &rest initargs &key &allow-other-keys)
  (let* ((metadata-column-info (delectus::get-metadata-column-info (dbpath pane)))
         (metadata-column-names (mapcar #'delectus::column-info-name metadata-column-info))
         (userdata-column-info (delectus::get-userdata-column-info (dbpath pane)))
         (latest-column-data (delectus::get-latest-columns (dbpath pane)))
         (latest-userdata (mapcar #'delectus::from-json
                                  (delectus::op-userdata latest-column-data)))
         (userdata-column-names (mapcar (lambda (ud)(fset:@ ud :|name|)) latest-userdata))
         (column-info (delectus::get-column-info (dbpath pane)))
         (column-names (append metadata-column-names userdata-column-names))
         (column-specs (mapcar (lambda (cname) `(:title ,cname :default-width 96))
                               column-names))
         (list-name-op (delectus::get-latest-listname (dbpath pane)))
         (listname (or (delectus::op-name list-name-op)
                       "Untitled list")))
    (setf (interface-title pane) listname)
    (modify-multi-column-list-panel-columns (items-pane pane)
                                            :columns column-specs)))

(defun handle-item-selection (item interface)
  (format t "~%Selected item ~S from interface ~S"
          item interface))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (setf $win (contain (make-instance 'list-items-pane :dbpath $zippath)))
