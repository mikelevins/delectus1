;;;; ***********************************************************************
;;;;
;;;; Name:          view-item-card.lisp
;;;; Project:       delectus 2
;;;; Purpose:       a card-style presentation of a single list item
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; item-card
;;; ---------------------------------------------------------------------

(define-interface item-card ()
  ;; -- slots ---------------------------------------------
  ((columns-data :accessor columns-data :initform nil)
   (item-data :accessor item-data :initform nil))

  ;; -- panes ---------------------------------------------
  (:panes)
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout grid-layout '()
                :columns 2
                :reader main-layout))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 800 :height 600))

(defmethod value->presentation (value)(format nil "~A" value))
(defmethod value->presentation ((value null)) "")

(defun make-item-label-pane (label)
  (make-instance 'title-pane
                 :text label
                 :font
                 (gp:make-font-description
                  :family "Helvetica" 
                  :size 14
                  :weight :bold                         
                  :slant :roman)))

(defun make-item-value-pane (value)
  (make-instance 'title-pane
                 :text value
                 :font
                 (gp:make-font-description
                  :family "Helvetica" 
                  :size 14
                  :weight :normal                         
                  :slant :roman)))

(defmethod initialize-instance :after ((card item-card) &rest initargs 
                                       &key
                                         (columns-data nil)
                                         (item-data nil)
                                         &allow-other-keys)
  (setf (columns-data card)(delectus::columns-op-userdata columns-data))
  (setf (item-data card)(delectus::item-op-userdata item-data))
  (setf (layout-description (main-layout card))
        (make-item-card-description (columns-data card)
                                    (item-data card))))

(defun make-item-card-description (columns-data item-data)
  (let* ((lbls (mapcar #'delectus::column-description-name columns-data))
         (label-panes (mapcar #'make-item-label-pane lbls))
         (vals (mapcar #'value->presentation item-data))
         (value-panes (mapcar #'make-item-value-pane vals)))
    (delectus::interleave label-panes value-panes)))

;;; (setf $movies (delectus::path "~/Desktop/Movies.delectus2"))
;;; (setf $items (delectus::get-latest-items $movies))
;;; (setf $cols (delectus::get-latest-columns-op $movies))
;;; (setf $it (elt $items 0))

;;; (setf $win (contain (make-instance 'item-card :columns-data $cols :item-data $it)))


