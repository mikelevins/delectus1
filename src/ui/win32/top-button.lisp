;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          top-button.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       cocoa-specific toolbar buttons
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defparameter $button-target-map {})

(defclass top-button (push-button)()
  (:default-initargs :title-position :bottom :title-adjust :center))

(defmethod initialize-instance :after ((btn top-button) &rest initargs 
                                       &key &allow-other-keys)
  )

;;; (contain (make-instance 'top-button))
;;; (contain (make-instance 'top-button :image $add-button-image :title "Add"))

