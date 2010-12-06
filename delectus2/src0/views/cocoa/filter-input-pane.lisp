;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          filter-input-pane.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       text entry for filtering list contents
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defparameter $NSTextFieldRoundedBezel 1)

(defun init-filter-view (capi-pane objc-view)
  (let ((objc-view (invoke objc-view "init")))
    (invoke objc-view "setBezeled:" t)
    (invoke objc-view "setBezelStyle:" $NSTextFieldRoundedBezel)
    objc-view))

(defclass filter-input-pane (cocoa-view-pane)
  ()
  (:default-initargs :view-class "NSSearchField"
    :init-function #'init-filter-view))

