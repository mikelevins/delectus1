;;;; ***********************************************************************
;;;;
;;;; Name:          view-capi.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Delectus Desktop UI
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; Cocoa system icons from Delectus 1:
;;; add row button: NSListViewTemplate
;;; add column button: NSColumnViewTemplate

;;; ---------------------------------------------------------------------
;;; view classes
;;; ---------------------------------------------------------------------

;;; TODO: change the sort implementation
;;; currently it uses the built-in CAPI sort feature but that will
;;; sort only what's actually in the pane.  In order to get the best
;;; results with dbs that are too large to display all at once I
;;; should instead have a click in the header re-fetch the contents
;;; with an ORDER BY clause in the SQL.  That way the results will be
;;; sorted on the entire column even when only a fraction of the rows
;;; are displayed.
(define-interface document-window ()
  ;; -- slots ---------------------------------------------
  ((document :accessor document :initform nil :initarg :document)
   (item-count-limit :accessor item-count-limit :initform 256 :initarg :item-count-limit)
   (item-start-index :accessor item-start-index :initform 0 :initarg :item-start-index))

  ;; -- panes ---------------------------------------------
  (:panes
   (contents-pane multi-column-list-panel :reader contents-pane
                  :alternating-background t
                  :columns (compute-column-descriptions interface)
                  :items (compute-visible-rows interface))
   (count-pane title-pane :reader count-pane)
   (add-row-button push-button :reader add-row-button :title "Add a row" :title-position :right :text "+")
   (add-column-button push-button :reader add-column-button :title "Add a column" :text "+")
   (filter-input text-input-pane :reader filter-input
                 :title "Filter"
                 :visible-min-width 196
                 :text-change-callback 'handle-changed-filter-text)
   (previous-page-button push-button :reader previous-page-button :text "<")
   (next-page-button push-button :reader next-page-button :text ">"))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (header-layout row-layout '(nil add-column-button)
                  :adjust :bottom
                  :visible-min-height 40
                  :visible-max-height 40
                  :reader header-layout :border 4)
   (pager-layout row-layout '(previous-page-button count-pane next-page-button)
                 :reader pager-layout
                 :adjust :center)
   (footer-layout row-layout '(add-row-button nil filter-input nil pager-layout)
                  :adjust :bottom
                  :visible-min-height 36
                  :visible-max-height 36
                  :reader paging-layout :border 4)
   (main-layout column-layout '(header-layout contents-pane footer-layout)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :window-styles '(:textured-background)
   :width 800 :height 600))

(defmethod initialize-instance :after ((window document-window) &rest initargs &key &allow-other-keys)
  (setf (title-pane-text (count-pane window))
        (compute-item-count-text window)))

(defmethod filter-text ((window document-window))
  (text-input-pane-text (filter-input window)))

(defmethod compute-column-descriptions ((window document-window))
  (let* ((column-labels (visible-column-labels (document window))))
    (mapcar (lambda (lbl) `(:title ,lbl :default-width 96))
            column-labels)))

(defmethod compute-visible-rows ((window document-window))
  (visible-rows (document window)
                :column-labels (visible-column-labels (document window))
                :count-limit (item-count-limit window)
                :start-index (item-start-index window)
                :filter-text (filter-text window)))

(defun element-getter (n)
  #'(lambda (it)(elt it n)))

(defmethod compute-item-count-text ((window document-window))
  (format nil "Items ~A-~A (of ~A)"
          (1+ (item-start-index window)) 
          (+ (item-start-index window)
             (length (compute-visible-rows window)))
          (store-count-all-rows (store (document window)))))

(defun handle-changed-filter-text (text filter-input window caret-position)
  (declare (ignore text filter-input caret-position))
  (setf (collection-items (contents-pane window))
        (compute-visible-rows window)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (defparameter $doc (make-instance 'document :store $store))
;;; (defparameter $ui (contain (make-instance 'document-window :document $doc)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/zipcode.delectus2"))
;;; (defparameter $doc (make-instance 'document :store $store))
;;; (defparameter $ui (contain (make-instance 'document-window :document $doc)))


