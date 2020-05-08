;;;; ***********************************************************************
;;;;
;;;; Name:          macos-view-utils.lisp
;;;; Project:       delectus 2
;;;; Purpose:       helpers for working with macos views
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; macos utils
;;; ---------------------------------------------------------------------

(defmethod set-mac-button-style ((button push-button)(style integer))
  (let* ((rep (capi-cocoa-library::representation-main-view (capi::find-representation-for-pane button))))
    (objc:invoke rep "setBezelStyle:" style)))

(defmethod set-mac-button-image ((button push-button)(image-name string))
  (let* ((rep (capi-cocoa-library::representation-main-view (capi::find-representation-for-pane button))))
    (objc:invoke rep "setImage:" (objc:invoke "NSImage" "imageNamed:" image-name))))
