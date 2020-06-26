;;;; ***********************************************************************
;;;;
;;;; Name:          app-main.lisp
;;;; Project:       delectus 2
;;;; Purpose:       the main macOS application
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; main entry point
;;; ---------------------------------------------------------------------

(in-package #:cl-user)

(defun delectus-cocoa-application ()
  (let ((application (make-instance 'ui::delectus2-application)))
    
    ;; debugging output on launch
    (format t "~%Delivered app? ~A~%"
            (if (cl-user::delivered-application-p) "Yes." "No."))
    (format t "~%Bundle path: ~S~%" delectus::*delectus-root-pathname*)
    (format t "~%SQLite3 loaded from: ~S~%" (CFFI:FOREIGN-LIBRARY-PATHNAME 'sqlite-ffi::sqlite3-lib))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with no windows initially.
    (capi:convert-to-screen nil)))
