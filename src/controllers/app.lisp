;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          app.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       application object
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; application
;;; ---------------------------------------------------------------------

(defclass application ()
  ((resources :accessor resources :initform {:images {}})
   (documents :accessor documents :initform nil)
   (active-interface :accessor active-interface :initform nil)
   (untitled-index :accessor %untitled-index :initform 0)
   (ui :reader ui :initform (make-instance 'delectus-ui)))
  (:metaclass singleton-class))

(defun app ()(make-instance 'application))

(defmethod untitled-index ()
  (incf (%untitled-index (app)))
  (%untitled-index (app)))

(defmethod next-untitled-name ()
  (format nil "Untitled ~A" (untitled-index)))

;;; ---------------------------------------------------------------------
;;; application main
;;; ---------------------------------------------------------------------

(defun delectus ()
  #+cocoa (objc:ensure-objc-initialized
           :modules '("/System/Library/Frameworks/Foundation.framework/Versions/C/Foundation"
                      "/System/Library/Frameworks/Cocoa.framework/Versions/A/Cocoa"))
  (set-application-interface (ui (app)))
  (convert-to-screen nil))

;;; (app)