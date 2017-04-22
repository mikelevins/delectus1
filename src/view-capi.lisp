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
   (item-count :accessor item-count :initform 0) ; computed from select results
   (item-count-limit :accessor item-count-limit :initform 256 :initarg :item-count-limit)
   (item-start-index :accessor item-start-index :initform 0 :initarg :item-start-index))

  ;; -- panes ---------------------------------------------
  (:panes
   (contents-pane multi-column-list-panel :reader contents-pane
                  :alternating-background t
                  :columns (compute-column-descriptions document)
                  :items (compute-visible-rows document :count-limit item-count-limit :start-index item-start-index))
   (count-pane title-pane :reader count-pane)
   (add-row-button push-button :reader add-row-button :title "Add a row" :text "+")
   (add-column-button push-button :reader add-column-button :title "Add a column" :text "+")
   (filter-input text-input-pane :reader filter-input))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (header-layout row-layout '(nil add-column-button)
                  :visible-min-height 48
                  :visible-max-height 48
                  :reader header-layout :border 4)
   (footer-layout row-layout '(add-row-button nil count-pane filter-input nil)
                  :visible-min-height 48
                  :visible-max-height 48
                  :reader footer-layout :border 4)
   (main-layout column-layout '(header-layout contents-pane footer-layout)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :window-styles '(:textured-background)
   :width 800 :height 600))

(defmethod initialize-instance :after ((window document-window) &rest initargs &key &allow-other-keys)
  (setf (item-count window)
        (length (collection-items (contents-pane window))))
  (setf (title-pane-text (count-pane window))
        (compute-item-count-text window)))

;;; dummy method
(defmethod compute-column-descriptions ((document null))
  `((:title "" :default-width 96)))

(defmethod compute-column-descriptions ((document document))
  (let* ((column-labels (visible-column-labels document)))
    (mapcar (lambda (lbl) `(:title ,lbl :default-width 96))
            column-labels)))

(defmethod compute-visible-rows ((document null) &key (count-limit nil)(start-index 0)) 
  (declare (ignore count-limit start-index))
  nil)

(defmethod compute-visible-rows ((document document) &key (count-limit nil)(start-index 0))
  (visible-rows document
                :column-labels (visible-column-labels document)
                :count-limit count-limit
                :start-index start-index))

(defun element-getter (n)
  #'(lambda (it)(elt it n)))

(defmethod compute-item-count-text ((window document-window))
  (let ((store-row-count (store-nondeleted-row-count (store (document window)))))
    (format nil "Items ~A to ~A of ~A items"
            (item-start-index window) 
            (1- (+ (item-start-index window)
                   (item-count window)))
            store-row-count)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (defparameter $doc (make-instance 'document :store $store))
;;; (defparameter $ui (contain (make-instance 'document-window :document $doc :item-start-index 17)))
