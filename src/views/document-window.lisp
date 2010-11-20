;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document-window.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       CAPI window definition
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(define-interface document-window ()
  ;; slots
  ((document :reader document :initarg :document :initform nil))
  ;; panes
  (:panes
   ;; top row
   (add-row-button push-button :text "Add Row")
   (delete-row-button push-button :text "Del Row")
   (add-column-button push-button :text "Add Col")
   (delete-column-button push-button :text "Del Col")
   ;; main row
   (row-pane multi-column-list-panel :columns '((:title "A" :width (character 12))))
   ;; bottom row
   (trash-button push-button :text "Trash")
   (filter-field text-input-pane))
  ;; layouts
  (:layouts
   ;; main
   (main-layout column-layout '(top-row table-row bottom-row))
   ;; rows
   (top-row row-layout '(row-cluster nil column-cluster) :external-min-height 60 :external-max-height 60)
   (table-row row-layout '(row-pane))
   (bottom-row row-layout '(trash-cluster nil filter-field) :external-min-height 56 :external-max-height 56)
   ;; control clusters
   (row-cluster row-layout '(add-row-button delete-row-button) :adjust :center 
                :external-min-width 196 :external-max-width 196)
   (column-cluster row-layout '(add-column-button delete-column-button) :adjust :center
                   :external-min-width 196 :external-max-width 196)
   (trash-cluster row-layout '(trash-button) :adjust :center
                  :external-min-width 84 :external-max-width 84))
  ;; defaults
  (:default-initargs :title "Delectus" :width 700 :height 400 :initial-focus 'filter-field
                     :window-styles '(:internal-borderless :textured-background)
                     :activate-callback (lambda (intf activep)
                                          (when activep
                                            (unless (eql (active-interface (app)) intf)
                                              (setf (active-interface (app)) intf))))
                     :destroy-callback (lambda (intf)
                                         (when (eql (active-interface (app)) intf)
                                           (setf (active-interface (app)) nil)))))

;;; (contain (make-instance 'document-window))