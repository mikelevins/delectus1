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

(defun item-card-field-layout (label value)
  (make-instance 'row-layout
                 :description [(make-instance 'title-pane :text label)
                               (make-instance 'title-pane :text value)]))

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
           ;; BUG: csv import constructed the field values as literals, not as JSON strings
           ;;      need to fix import code to store values as JSON
           ;;      temporary workaround: do not parse the values
           (userdata-field-values (mapcar (lambda (val)(format nil "~A" val))
                                          (delectus::drop (length delectus::*item-op-columns*)
                                                          (item-data card))))
           (field-layouts (mapcar #'item-card-field-layout userdata-column-names userdata-field-values)))
      (setf (layout-description (main-layout card))
            field-layouts))))

;;; (setf $movies (delectus::path "~/Desktop/Movies.delectus2"))
;;; (setf $items (delectus::get-latest-items $movies))
;;; (setf $cols (delectus::get-latest-columns-op $movies))
;;; (setf $it (elt $items 0))

;;; (setf $win (contain (make-instance 'item-card :columns-data $cols :item-data $it)))
