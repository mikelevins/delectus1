;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus-window.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       common window UI
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(define-interface delectus-window ()
  ;; slots
  ((document :reader document :initarg :document :initform nil))
  ;; panes
  (:panes
   ;; top row
   (add-row-button top-button :target (document interface) :label "Add Row"
                   :action "addRow:" :image (resource "add.png") :altimage (resource "addhl.png"))
   (delete-row-button top-button :target (document interface) :label "Del Row"
                   :action "deleteRow:" :image (resource "del.png") :altimage (resource "delhl.png"))
   (add-column-button top-button :target (document interface) :label "Add Col"
                      :action "addColumn:" :image (resource "add.png") :altimage (resource "addhl.png"))
   (delete-column-button top-button :target (document interface) :label "Del Col"
                   :action "deleteColumn:" :image (resource "del.png") :altimage (resource "delhl.png"))
   ;; main row
   ;;(row-pane model-pane :reader model-pane)
   ;; bottom row
   ;;(trash-button trash-button)
   ;;(filter-field filter-pane)
   )
  ;; layouts
  (:layouts
   ;; main
   (main-layout column-layout '(top-row table-row bottom-row))
   ;; rows
   (top-row row-layout '(row-cluster nil column-cluster) :external-min-height 60 :external-max-height 60)
   (table-row row-layout '(#+(or)row-pane))
   (bottom-row row-layout '(trash-cluster nil #+(or)filter-field) :external-min-height 56 :external-max-height 56)
   ;; control clusters
   (row-cluster row-layout '(add-row-button delete-row-button) :adjust :center 
                :external-min-width 196 :external-max-width 196)
   (column-cluster row-layout '(add-column-button delete-column-button) :adjust :center
                   :external-min-width 196 :external-max-width 196)
   (trash-cluster row-layout '(#+(or)trash-button) :adjust :center
                  :external-min-width 84 :external-max-width 84))
  ;; defaults
  (:default-initargs :title "Delectus" :width 700 :height 400 #|:initial-focus 'filter-field|#
                     :window-styles '(:internal-borderless :textured-background)
                     :create-callback (lambda (intf)
                                        (let* ((doc (document intf)))
                                          (when doc
                                            (setup-columns (row-pane intf) doc))))
                     :activate-callback (lambda (intf activep)
                                          (when activep
                                            (unless (eql (active-interface (app)) intf)
                                              (activate-interface intf))))
                     :destroy-callback (lambda (intf)
                                         (when (eql (active-interface (app)) intf)
                                           (setf (%active-interface (app)) nil)))))

;;;(contain (make-instance 'delectus-window))