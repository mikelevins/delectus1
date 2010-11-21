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
(defparameter $trash-image-width 32)
(defparameter $trash-image-height 32)

(define-interface document-window ()
  ;; slots
  ((document :reader document :initarg :document :initform nil))
  ;; panes
  (:panes
   ;; top row
   (row-cluster toolbar :title "Row" :title-position :right
                :items (list (make-instance 'toolbar-button :image (image :add-button))
                             (make-instance 'toolbar-button :image (image :del-button)))
                :image-width $toolbar-image-width 
                :image-height $toolbar-image-height)
   (column-cluster toolbar :title "Column" :title-position :left
                   :items (list (make-instance 'toolbar-button :image (image :add-button))
                                (make-instance 'toolbar-button :image (image :del-button)))
                   :image-width $toolbar-image-width 
                   :image-height $toolbar-image-height)
   ;; bottom row
   (trash-cluster toolbar :title "Show deleted items" :title-position :right
                  :image-width $trash-image-width 
                  :image-height $trash-image-height
                  :items (list (make-instance 'toolbar-button :image (image :trashempty-button))))
   (filter-field text-input-pane :title "Filter" :title-position :left))
  ;; layouts
  (:layouts
   ;; main
   (main-layout column-layout '(top-row table-rows bottom-row))
   ;; rows
   (top-row row-layout '(row-cluster nil column-cluster))
   (table-rows row-layout '() :reader table-rows)
   (bottom-row row-layout '(trash-cluster nil filter-field) :external-min-height 56 :external-max-height 56))
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

;;; Against all reasonable expectation, this actually works and produces reasonable performance!
;;; (setq $doc(make-instance 'document :presentation (make-instance 'presentation :model $zips)))
;;; (setq $mlist (make-instance 'multi-column-list-panel :columns (mapcar (lambda (col) (list :title (label col) :adjust :left :width '(character 16))) (as 'list (columns $zips))) :column-function (lambda (row)(as 'list (elements row))) :items (as 'list (rows $zips)) :item-print-function #'val))
;;; (capi:execute-with-interface (window $doc)(lambda ()(setf (layout-description (table-rows (window $doc))) (list $mlist))))