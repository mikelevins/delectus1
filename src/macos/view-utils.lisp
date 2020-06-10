;;;; ***********************************************************************
;;;;
;;;; Name:          view-utils.lisp
;;;; Project:       delectus 2
;;;; Purpose:       general view helpers
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; printing and presenting values
(defmethod value->presentation (value)(format nil "~A" value))
(defmethod value->presentation ((value null)) "")

;;; panes for item labels and values

(defun make-item-label-pane (label)
  (make-instance 'title-pane
                 :text label
                 :visible-border t
                 :font
                 (gp:make-font-description
                  :family "Helvetica" 
                  :size 14
                  :weight :bold                         
                  :slant :roman)))

(defun make-item-value-pane (value)
  (make-instance 'title-pane
                 :text value
                 :visible-border t
                 :font
                 (gp:make-font-description
                  :family "Helvetica" 
                  :size 14
                  :weight :normal                         
                  :slant :roman)))

