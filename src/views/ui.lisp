;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ui.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       CAPI UI definitions
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(define-interface delectus-ui (#+cocoa cocoa-default-application-interface)
  ;; slots
  ()
  ;; menus
  (:menus
   (application-menu "Delectus"
                     ((:component (("About Delectus" :callback 'delectus-about :callback-type :none)))
                      (:component (("Preferences..." :callback 'delectus-preferences :callback-type :none)))
                      (:component ()
                       ;; This is a special named component where the CAPI will
                       ;; attach the standard Services menu.
                       :name :application-services)
                      (:component (("Hide Delectus" :accelerator "accelerator-h" :callback-data :hidden)
                                   ("Hide Others" :accelerator "accelerator-meta-h" :callback-data :others-hidden)
                                   ("Show All" :callback-data :all-normal))
                                  :callback #'(setf capi:top-level-interface-display-state)
                                  :callback-type :data-interface)
                      (:component (("Quit Delectus" :accelerator "accelerator-q"
                                                    :callback 'capi:destroy :callback-type :interface)))))
   (file-menu "File" () :items-function 'file-menu-items)
   (edit-menu "Edit" () :items-function 'edit-menu-items)
   (windows-menu "Window" () :items-function 'windows-menu-items)
   (help-menu "Help" () :items-function 'help-menu-items))
  ;; menubar
  (:menu-bar application-menu file-menu edit-menu windows-menu help-menu)
  ;; defaults
  (:default-initargs :title "Delectus" 
    :application-menu 'application-menu))
