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
   (row-cluster toolbar :title "Row" :title-position :right :flatp t
                :items (list (make-instance 'toolbar-button :image (image :add-button))
                             (make-instance 'toolbar-button :image (image :del-button)))
                :image-width $toolbar-image-width 
                :image-height $toolbar-image-height)
   (column-cluster toolbar :title "Column" :title-position :left :flatp t
                   :items (list (make-instance 'toolbar-button :image (image :add-button))
                                (make-instance 'toolbar-button :image (image :del-button)))
                   :image-width $toolbar-image-width 
                   :image-height $toolbar-image-height)
   ;; bottom row
   (trash-cluster toolbar :title "Show deleted items" :title-position :right :flatp t
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

(defmethod update-contents ((win document-window))
  (let* ((doc (document win))
         (pres (presentation doc))
         (rows (table-rows win))
         (row-list (make-instance 'multi-column-list-panel
                                 :columns (mapcar (lambda (col)
                                                    (list :title (label col)
                                                          :adjust :left
                                                          :width '(character 16)))
                                                  (as 'list (columns pres)))
                                 :column-function (lambda (row)(as 'list (elements row)))
                                 :items (as 'list (rows pres))
                                 :item-print-function #'val)))
    (execute-with-interface win
                            (lambda ()
                              (setf (layout-description rows)
                                    (list row-list))))))

