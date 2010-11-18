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
   (add-row-button top-button :title "Add Row" :image $add-button-image)
   (delete-row-button top-button :title "Del Row" :image $delete-button-image :background :transparent
                   :title-position :bottom :title-adjust :center)
   (add-column-button top-button :title "Add Col"  :image $add-button-image :background :transparent
                   :title-position :bottom :title-adjust :center)
   (delete-column-button top-button :title "Del Col" :image $delete-button-image :background :transparent
                   :title-position :bottom :title-adjust :center)
   ;; main row
   ;;(row-pane model-pane :reader model-pane)
   ;; bottom row
   (trash-button top-button :image $trash-button-empty-image)
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
   (add-row-container simple-pinboard-layout '(add-row-button) :background :transparent)
   (delete-row-container simple-pinboard-layout '(delete-row-button) :background :transparent)
   (add-column-container simple-pinboard-layout '(add-column-button) :background :transparent)
   (delete-column-container simple-pinboard-layout '(delete-column-button) :background :transparent)
   (row-cluster row-layout '(add-row-container delete-row-container) :adjust :center 
                :external-min-width 196 :external-max-width 196)
   (column-cluster row-layout '(nil add-column-container delete-column-container) :adjust :center
                   :external-min-width 196 :external-max-width 196)
   (trash-cluster row-layout '(trash-button) :adjust :center
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