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
;;; item-card-field
;;; ---------------------------------------------------------------------

(define-interface item-card-field ()
  ;; -- slots ---------------------------------------------
  ((field-label :accessor field-label :initform nil :initarg :field-label)
   (field-value :accessor field-value :initform nil :initarg :field-value))

  ;; -- panes ---------------------------------------------
  (:panes
   (label-pane title-pane
               :font
               (gp:make-font-description
                :family "Helvetica" 
                :size 14
                :weight :bold                         
                :slant :roman)
               :reader label-pane)
   (value-pane title-pane
               :font
               (gp:make-font-description
                :family "Helvetica" 
                :size 14
                :weight :medium                         
                :slant :roman)
               :reader value-pane))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout row-layout '(label-pane value-pane)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout))

(defmethod initialize-instance :after ((field item-card-field) &rest initargs 
                                       &key &allow-other-keys)
  (when (field-label field)
    (setf (title-pane-text (label-pane field))
          (field-label field)))
  (when (field-value field)
    (setf (title-pane-text (value-pane field))
          (format nil "~A" (field-value field)))))

;;; (setf $win (contain (make-instance 'item-card-field :field-label "Title" :field-value "Home Alone")))

(defun item-card-field (label value)
  (make-instance 'item-card-field :field-label label :field-value value))


;;; ---------------------------------------------------------------------
;;; item-card
;;; ---------------------------------------------------------------------

(define-interface item-card ()
  ;; -- slots ---------------------------------------------
  ((columns-data :accessor columns-data :initform nil :initarg :columns-data)
   (item-data :accessor item-data :initform nil :initarg :item-data))

  ;; -- panes ---------------------------------------------
  (:panes)
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '()
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 800 :height 600))


(defmethod initialize-instance :after ((card item-card) &rest initargs 
                                       &key &allow-other-keys)
  (when (columns-data card)
    (let* ((metadata-column-labels (mapcar #'symbol-name delectus::*item-op-columns*))
           (userdata-columns-data (mapcar #'jonathan:parse
                                          (delectus::drop (length delectus::*columns-op-columns*)
                                                          (columns-data card))))
           (userdata-column-labels (mapcar #'delectus::column-description-label
                                           userdata-columns-data))
           (userdata-column-names (mapcar #'delectus::column-description-name
                                          userdata-columns-data))
           (userdata-field-values (mapcar (lambda (val)(format nil "~A" (jonathan:parse val)))
                                          (delectus::drop (length delectus::*item-op-columns*)
                                                          (item-data card))))
           (field-layouts (mapcar #'item-card-field userdata-column-names userdata-field-values)))
      (setf (layout-description (main-layout card))
            field-layouts))))

;;; (setf $movies (delectus::path "~/Desktop/Movies.delectus2"))
;;; (setf $items (delectus::get-latest-items $movies))
;;; (setf $cols (delectus::get-latest-columns-op $movies))
;;; (setf $it (elt $items 0))

;;; (setf $win (contain (make-instance 'item-card :columns-data $cols :item-data $it)))
