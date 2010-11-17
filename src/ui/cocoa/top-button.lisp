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

(defparameter $foo nil)

(defun init-top-button (capi-button objc-button)
  (let ((btn (invoke objc-button "init")))
    (invoke btn "setButtonType:" $NSMomentaryChangeButton)
    (invoke btn "setBordered:" nil)
    (invoke btn "setImagePosition:" $NSImageAbove)
    (when (target capi-button)
      (invoke btn "setTarget:" (objc-object-pointer (target capi-button))))
    (when (action capi-button)
      (invoke btn "setAction:" (coerce-to-selector (action capi-button))))
    (invoke btn "setTitle:" (label capi-button))    
    (when (image capi-button)
      (invoke btn "setImage:" (nsimage (image capi-button))))
    (when (altimage capi-button)
      (invoke btn "setAlternateImage:" (nsimage (altimage capi-button))))
    btn))

(defclass top-button (cocoa-view-pane)
  ((target :reader target :initarg :target :initform nil) 
   (action :reader action :initarg :action :initform nil)
   (label :reader label :initarg :label :initform nil)
   (image :reader image :initarg :image :initform nil)
   (altimage :reader altimage :initarg :altimage :initform nil))
  (:default-initargs :view-class "NSButton" :background :transparent
    :init-function #'init-top-button))

;;; (contain (make-instance 'top-button))
;;; (contain (make-instance 'top-button :image (resource "trashempty48.png")))

