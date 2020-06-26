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
;;; toplevel app UI
;;; ---------------------------------------------------------------------

(defun delectus-interface-item-title (self prefix)
  (concatenate 'string prefix " " (capi:interface-title self)))

(defun delectus-interface-about ()
  (capi:display-message-on-screen (capi:convert-to-screen nil)
                                  (format nil "Delectus2 alpha build~%version ~A~%built ~A"
                                          delectus::+delectus-version+
                                          (local-time:to-rfc1123-timestring (local-time:now)))))

(defun delectus-interface-preferences ()
  )

(defun delectus-interface-multiple-message (self message &rest args)
  (declare (ignore self))
  )

(defun handle-open-file (interface)
  (let ((path (capi:prompt-for-file "Open a Delectus list..." :filter "*.delectus2")))
    (when path
      (capi:display (make-instance 'delectus-ui::items-sheet :dbpath path)))))

(defun delectus-interface-message (self message &rest args)
  (declare (ignore self))
  (case message
    (:open-file
     (let ((filename (car args)))
       (when (equal (pathname-type filename) "delectus2")
         (capi:display (make-instance 'delectus-ui::items-sheet :dbpath filename)))))))


(capi:define-interface delectus2-application (capi:cocoa-default-application-interface)
  ()
  (:menus
   (application-menu
    (capi:interface-title capi:interface)
    ((:component
      (((delectus-interface-item-title capi:interface "About")
        :callback 'delectus-interface-about
        :callback-type :none)))
     (:component
      (("Preferences..."
        :callback 'delectus-interface-preferences
        :callback-type :none)))
     (:component
      ()
      ;; This is a special named component where the CAPI will
      ;; attach the standard Services menu.
      :name :application-services)
     (:component
      (((delectus-interface-item-title capi:interface "Hide")
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
      (((delectus-interface-item-title capi:interface "Quit")
        :accelerator "accelerator-q"
        :callback 'capi:destroy
        :callback-type :interface)))))
   (file-menu
    "File"
    (("Open"
      :accelerator "accelerator-o"
      :callback 'handle-open-file
      :callback-type :interface))))
  (:menu-bar application-menu file-menu)
  (:default-initargs
      :title "Delectus2"
    :application-menu 'application-menu
    :message-callback 'delectus-interface-message))

;;;(capi:set-application-interface (make-instance 'ui::delectus2-application))
