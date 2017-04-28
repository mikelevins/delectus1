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

;;; ---------------------------------------------------------------------
;;; TODO
;;; ---------------------------------------------------------------------
;;; - make the column-widths stay where the user adjusts them to even
;;;   when sorting by different columns
;;; - figure out a way to sort numerically in columns that are mainly
;;;   numbers

;;; ---------------------------------------------------------------------
;;; Notes
;;; ---------------------------------------------------------------------

;;; Cocoa system icons from Delectus 1:
;;; add row button: NSListViewTemplate
;;; add column button: NSColumnViewTemplate

;;; ---------------------------------------------------------------------
;;; view classes
;;; ---------------------------------------------------------------------

(define-interface document-window ()
  ;; -- slots ---------------------------------------------
  ((document :accessor document :initform nil :initarg :document)
   (sort-column :accessor sort-column :initform nil :initarg :sort-column)
   (sort-order :accessor sort-order :initform nil :initarg :sort-order) ; NIL | :ascending | :descending
   (item-count-limit :accessor item-count-limit :initform 256 :initarg :item-count-limit)
   (item-start-index :accessor item-start-index :initform 0 :initarg :item-start-index))

  ;; -- panes ---------------------------------------------
  (:panes
   (contents-pane multi-column-list-panel :reader contents-pane
                  :header-args '(:selection-callback handle-column-selection :callback-type :item-interface)
                  :alternating-background t
                  :columns (compute-column-descriptions interface)
                  :items (compute-visible-rows interface)
                  :auto-reset-column-widths nil)
   (count-pane title-pane :reader count-pane)
   (add-row-button push-button :reader add-row-button :title "Add a row" :title-position :right :text "+")
   (add-column-button push-button :reader add-column-button :title "Add a column" :text "+")
   (filter-input text-input-pane :reader filter-input
                 :title "Filter"
                 :visible-min-width 196
                 :text-change-callback 'handle-changed-filter-text)
   (previous-page-button push-button :reader previous-page-button :text "<"
                         :callback-type :interface
                         :callback 'handle-go-previous)
   (next-page-button push-button :reader next-page-button :text ">"
                     :callback-type :interface
                     :callback 'handle-go-next))
  
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
  (unless (sort-column window)
    (setf (sort-column window)
          (first (visible-column-labels (document window)))))
  (unless (sort-order window)
    (setf (sort-order window) :ascending))
  (update-collection-rows window)
  (update-pager-text window))

(defmethod update-collection-rows ((window document-window))
  (setf (collection-items (contents-pane window))
        (compute-visible-rows window)))

(defmethod update-pager-text ((window document-window))
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
                :filter-text (filter-text window)
                :sort-column (sort-column window)
                :sort-order (sort-order window)))

(defmethod compute-item-count-text ((window document-window))
  (format nil "Items ~A-~A (of ~A)"
          (1+ (item-start-index window))
          (+ (item-start-index window)
             (length (collection-items (contents-pane window))))
          (store-count-rows (store (document window))
                            :filter-text (filter-text window))))

(defun handle-changed-filter-text (text filter-input window caret-position)
  (declare (ignore text filter-input caret-position))
  (setf (item-start-index window) 0)
  (update-collection-rows window)
  (update-pager-text window))

(defun handle-go-previous (window)
  (let* ((old-start-index (item-start-index window))
         (delta (item-count-limit window))
         (computed-new-start-index (- old-start-index delta))
         (new-start-index (max 0 computed-new-start-index)))
    (setf (item-start-index window)
          new-start-index)
    (update-collection-rows window)
    (update-pager-text window)))

(defun handle-go-next (window)
  (let* ((old-start-index (item-start-index window))
         (delta (item-count-limit window))
         (computed-new-start-index (+ old-start-index delta))
         (selected-row-count (store-count-rows (store (document window))
                                               :filter-text (filter-text window)))
         (new-start-index (if (>= computed-new-start-index selected-row-count)
                              old-start-index
                            computed-new-start-index)))
    (setf (item-start-index window)
          new-start-index)
    (update-collection-rows window)
    (update-pager-text window)))

(defun handle-column-selection (window selected-column)
  (let* ((old-sort-column (sort-column window))
         (old-sort-order (sort-order window))
         (new-sort-column selected-column))
    (if (equalp new-sort-column old-sort-column)
        (if (equal :ascending old-sort-order)
            (setf (sort-order window) :descending)
          (setf (sort-order window) :ascending))
      (setf (sort-column window) new-sort-column
            (sort-order window) :ascending))
    (setf (item-start-index window) 0)
    (update-collection-rows window)
    (update-pager-text window)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/Movies.delectus2"))
;;; (defparameter $doc (make-instance 'document :store $store))
;;; (defparameter $ui (contain (make-instance 'document-window :document $doc)))

;;; (defparameter $store (make-instance 'store :data-path "/Users/mikel/Desktop/zip_codes_states.delectus2"))
;;; (defparameter $doc (make-instance 'document :store $store))
;;; (defparameter $ui (contain (make-instance 'document-window :document $doc)))
