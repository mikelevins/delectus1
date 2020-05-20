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

(defun cocoa-application-interface-item-title (self prefix)
  (concatenate 'string prefix " " (capi:interface-title self)))

(defun cocoa-application-interface-multiple-about ()
  (capi:display-message-on-screen (capi:convert-to-screen nil)
                                  "Delectus2 alpha build"))

(defun cocoa-application-interface-multiple-preferences ()
  )

(defun cocoa-application-interface-multiple-message (self message &rest args)
  (declare (ignore self))
  )

(defun handle-open-file ()
  (let ((path (capi:prompt-for-file "Open a Delectus list..." :filter "*.delectus2")))
    (when path
      (capi:contain (make-instance 'delectus-ui::items-sheet :dbpath path)))))

(capi:define-interface delectus2-application (capi:cocoa-default-application-interface)
  ()
  (:menus
   (application-menu
    (capi:interface-title capi:interface)
    ((:component
      (((cocoa-application-interface-item-title capi:interface "About")
        :callback 'cocoa-application-interface-multiple-about
        :callback-type :none)))
     (:component
      (("Preferences..."
        :callback 'cocoa-application-interface-multiple-preferences
        :callback-type :none)))
     (:component
      ()
      ;; This is a special named component where the CAPI will
      ;; attach the standard Services menu.
      :name :application-services)
     (:component
      (((cocoa-application-interface-item-title capi:interface "Hide")
        :accelerator "accelerator-h"
        :callback-data :hidden)
       ("Hide Others"
        :accelerator "accelerator-meta-h"
        :callback-data :others-hidden)
       ("Show All"
        :callback-data :all-normal))
      :callback #'(setf capi:top-level-interface-display-state)
      :callback-type :data-interface)
     (:component
      (((cocoa-application-interface-item-title capi:interface "Quit")
        :accelerator "accelerator-q"
        :callback 'capi:destroy
        :callback-type :interface)))))
   (file-menu
    "File"
    (("Open"
      :callback 'handle-open-file
      :callback-type :none))))
  (:menu-bar application-menu file-menu)
  (:default-initargs
      :title "Delectus2"
    :application-menu 'application-menu
    :message-callback 'cocoa-application-interface-multiple-message))

(in-package #:cl-user)

(defun delectus-cocoa-application ()
  (let ((application (make-instance 'ui::delectus2-application)))
    ;; debugging output on launch
    (format t "~%Delectus delivered? ~S~%" (and (member :delectus2 cl:*features*) t))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with no windows initially.
    (capi:convert-to-screen nil)))
