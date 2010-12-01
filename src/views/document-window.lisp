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

(defun handle-add-row-button (data intf)
  (let ((doc (document intf)))
    (add-row! doc)))

(defun handle-delete-row-button (data intf)
  (display-message "~S, ~S" data intf))

(defun handle-add-column-button (data intf)
  (let ((doc (document intf)))
    (multiple-value-bind (label okp)
        (prompt-for-string "Enter a label for the new column")
      (when okp
        (if (find-column (presentation doc) label)
            (display-message "The column '~A' already exists" label)
            (add-column! doc label))))))

(defun handle-delete-column-button (data intf)
  (display-message "~S, ~S" data intf))

(defun handle-trash-button (data intf)
  (display-message "~S, ~S" data intf))

(define-interface document-window ()
  ;; slots
  ((document :reader document :initarg :document :initform nil))
  ;; panes
  (:panes
   ;; top row
   (row-cluster toolbar :title "Row" :title-position :right :flatp t
                :items (list (make-instance 'toolbar-button :image (image :add-button)
                                            :callback #'handle-add-row-button)
                             (make-instance 'toolbar-button :image (image :del-button)
                                            :callback #'handle-delete-row-button))
                :image-width $toolbar-image-width 
                :image-height $toolbar-image-height)
   (column-cluster toolbar :title "Column" :title-position :left :flatp t
                   :items (list (make-instance 'toolbar-button :image (image :add-button)
                                               :callback #'handle-add-column-button)
                                (make-instance 'toolbar-button :image (image :del-button)
                                               :callback #'handle-delete-column-button))
                   :image-width $toolbar-image-width 
                   :image-height $toolbar-image-height)
   ;; bottom row
   (trash-cluster toolbar :title "Show deleted items" :title-position :right :flatp t
                  :image-width $trash-image-width 
                  :image-height $trash-image-height
                  :items (list (make-instance 'toolbar-button :image (image :trashempty-button)
                                              :callback #'handle-trash-button)))
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
                                         (remove-document! (app)(document intf))
                                         (when (eql (active-interface (app)) intf)
                                           (setf (active-interface (app)) nil)))))

(defun column-description (col)
  (list :title col
        :adjust :left
        :width '(character 16)))

#+cocoa
(defun setup-nstableview (pane)
  (let ((objc-view (slot-value (slot-value pane 'capi-internals::representation)
                               'capi-cocoa-library::main-view)))
    (objc:invoke objc-view "setAllowsColumnReordering:" t)
    (objc:invoke objc-view "setUsesAlternatingRowBackgroundColors:" t)))

(defun setup-rows (win)
  (let* ((doc (document win))
         (pres (presentation doc))
         (rows (table-rows win))
         (row-list (make-instance 'multi-column-list-panel
                                  :columns (mapcar #'column-description (columns pres))
                                  :column-function #'identity
                                  :items (rows pres)
                                  :item-print-function #'identity)))
    (setf (layout-description rows)(list row-list))
    #+cocoa (setup-nstableview row-list)))

(defmethod update-contents ((win document-window))
  (execute-with-interface win #'setup-rows win))

