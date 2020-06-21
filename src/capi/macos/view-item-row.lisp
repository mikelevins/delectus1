;;;; ***********************************************************************
;;;;
;;;; Name:          view-item-row.lisp
;;;; Project:       delectus 2
;;;; Purpose:       a row presentation of item values
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; item-row
;;; ---------------------------------------------------------------------

(define-interface item-row ()
  ;; -- slots ---------------------------------------------
  ((item-data :accessor item-data :initform nil))

  ;; -- panes ---------------------------------------------
  (:panes)
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout row-layout '() :reader main-layout))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout))

(defmethod initialize-instance :after ((row item-row) &rest initargs 
                                       &key
                                         (item-data nil)
                                         &allow-other-keys)
  (let* ((vals (mapcar #'value->presentation (delectus::item-op-userdata item-data)))
         (value-panes (mapcar #'make-item-value-pane vals)))
    (setf (layout-description (main-layout row))
          value-panes)))

;;; (setf $movies (delectus::path "~/Desktop/Movies.delectus2"))
;;; (setf $items (delectus::get-latest-items $movies))
;;; (setf $it (elt $items 0))

;;; (setf $win (contain (make-instance 'item-row :item-data $it)))

