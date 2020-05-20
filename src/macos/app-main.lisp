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

(defun delectus-interface-multiple-about ()
  (capi:display-message-on-screen (capi:convert-to-screen nil)
                                  "Delectus2 alpha build"))

(defun delectus-interface-multiple-preferences ()
  )

(defun delectus-interface-multiple-message (self message &rest args)
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
      (((delectus-interface-item-title capi:interface "About")
        :callback 'delectus-interface-multiple-about
        :callback-type :none)))
     (:component
      (("Preferences..."
        :callback 'delectus-interface-multiple-preferences
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
      :callback 'handle-open-file
      :callback-type :none))))
  (:menu-bar application-menu file-menu)
  (:default-initargs
      :title "Delectus2"
    :application-menu 'application-menu
    :message-callback 'delectus-interface-multiple-message))

;;; ---------------------------------------------------------------------
;;; main entry point
;;; ---------------------------------------------------------------------

(in-package #:cl-user)

(defun delectus-cocoa-application ()
  (let ((application (make-instance 'ui::delectus2-application)))
    ;; initialize app parameters
    (if (hcl:delivered-image-p)
        ;; delivered app: the delectus root path is the app bundle
        (delectus:bind ((image-path (lw:lisp-image-name))
                        (flag image-directory filename maybe-filename
                              (uiop:split-unix-namestring-directory-components image-path))
                        ;; two directories up from the image is the bundle path
                        (bundle-directory (subseq image-directory 0 (- (length image-directory) 2))))
          (setf delectus:*delectus-root-pathname*
                (make-pathname :directory (cons flag bundle-directory)))
          ;; find and set up the correct SQLite library
          )
        ;; development time: the delectus root path is the project root
        (setf delectus::*delectus-root-pathname*
              (asdf:system-relative-pathname :delectus "")))
    ;; debugging output on launch
    (format t "~%Bundle path: ~S~%" delectus::*delectus-root-pathname*)
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with no windows initially.
    (capi:convert-to-screen nil)))
