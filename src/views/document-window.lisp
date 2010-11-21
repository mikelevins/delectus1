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

(defparameter $toolbar-image-width 16)
(defparameter $toolbar-image-height 16)

(define-interface document-window ()
  ;; slots
  ((document :reader document :initarg :document :initform nil))
  ;; panes
  (:panes
   ;; top row
   (row-cluster toolbar :title "Row" :title-position :right
                :items (list (make-instance 'toolbar-button :text "Add Row" :image (image :add-button))
                             (make-instance 'toolbar-button :text "Del Row" :image (image :del-button)))
                :image-width $toolbar-image-width 
                :image-height $toolbar-image-height)
   (column-cluster toolbar :title "Column" :title-position :left
                   :items (list (make-instance 'toolbar-button :text "Add Col" :image (image :add-button))
                                (make-instance 'toolbar-button :text "Del Col" :image (image :del-button)))
                   :image-width $toolbar-image-width 
                   :image-height $toolbar-image-height)
   ;; main row
   (row-pane multi-column-list-panel :columns '((:title "A" :width (character 12))))
   ;; bottom row
   (trash-button push-button :text "Trash")
   (filter-field text-input-pane :title "Filter" :title-position :left))
  ;; layouts
  (:layouts
   ;; main
   (main-layout column-layout '(top-row table-row bottom-row))
   ;; rows
   (top-row row-layout '(row-cluster nil column-cluster))
   (table-row row-layout '(row-pane))
   (bottom-row row-layout '(trash-cluster nil filter-field) :external-min-height 56 :external-max-height 56)
   ;; control clusters
   ;;(row-cluster row-layout '(add-row-button delete-row-button) :adjust :center 
   ;;             :external-min-width 196 :external-max-width 196)
   ;;(column-cluster row-layout '(add-column-button delete-column-button) :adjust :center
   ;;                :external-min-width 196 :external-max-width 196)
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