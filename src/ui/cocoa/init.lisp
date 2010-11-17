;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          init.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       cocoa-specific init
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defun init-platform-libraries ()
  (objc:ensure-objc-initialized
   :modules
   '("/System/Library/Frameworks/Foundation.framework/Versions/C/Foundation"
     "/System/Library/Frameworks/Cocoa.framework/Versions/A/Cocoa")))

(defun init-platform-interface ()
  (set-application-interface (ui (app))))
