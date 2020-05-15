;;;; ***********************************************************************
;;;;
;;;; Name:          views-items-sheet.lisp
;;;; Project:       delectus 2
;;;; Purpose:       UI: a spreadsheet-like view of list items
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; parameters
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; items-sheet
;;; ---------------------------------------------------------------------

(define-interface items-sheet ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (total-items :accessor total-items :initform 0 :initarg :total-items)
   (items-per-page :accessor items-per-page :initform *default-result-items-per-page* :initarg :items-per-page)
   (current-page :accessor current-page :initform 0 :initarg :current-page))

  ;; -- panes ---------------------------------------------
  (:panes
   (items-pane multi-column-list-panel :reader items-pane
               :alternating-background t
               :item-print-function (lambda (it)(if (null it) "" it))
               :items nil
               :columns '((:title "Item"))
               :callback-type :item-interface
               :selection-callback 'handle-item-selection
               :header-args `(:font ,(gp:make-font-description :size 14 :slant :italic)))
   (filter-pane text-input-pane :search-field "Filter" :reader filter-pane
                ;; :change-callback 'update-items-sheet-for-changed-filter
                )
   (previous-button push-button :reader previous-button :text ""
                    :external-min-width 28 :external-max-width 28
                    :external-min-height 32 :external-max-height 32
                    ;; :callback 'handle-previous-button-click
                    )
   (next-button push-button :reader next-button :text ""
                :external-min-width 28 :external-max-width 28
                :external-min-height 32 :external-max-height 32
                ;; :callback 'handle-next-button-click
                )
   (item-count-pane title-pane :reader item-count-pane)
   (current-page-label title-pane :reader current-page-label :text "Page ")
   (current-page-pane text-input-pane :reader current-page-pane
                      ;; :callback 'handle-current-page-edit
                      ;; :callback-type :interface-data
                      :max-characters 9
                      :external-min-width 48 :external-max-width 48)
   (page-range-pane title-pane :reader page-range-pane))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (pager-layout row-layout '(previous-button current-page-label current-page-pane
                              page-range-pane next-button) :adjust :center)
   (controls-layout row-layout '(item-count-pane nil filter-pane nil pager-layout)
                    :ratios '(3 3 18 3 6)
                    :adjust :center)
   (main-layout column-layout '(items-pane controls-layout)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :width 800 :height 600
   :title "Delectus"))

;;; ---------------------------------------------------------------------
;;; initialize-instance
;;; ---------------------------------------------------------------------

(defmethod initialize-instance :after ((pane items-sheet) &rest initargs 
                                       &key &allow-other-keys)
  (setf (total-items pane)
        (count-latest-items (pathname (dbpath pane))))
  (update-list-display pane))

;;; ---------------------------------------------------------------------
;;; handlers and helpers
;;; ---------------------------------------------------------------------

;;; fix up the widget styles
(defmethod capi:interface-display :before ((pane items-sheet))
  (set-mac-button-style (previous-button pane)
                        *delectus-application-button-style*)
  (set-mac-button-image (previous-button pane)
                        +NSImageNameGoLeftTemplate+)
  (set-mac-button-style (next-button pane)
                        *delectus-application-button-style*)
  (set-mac-button-image (next-button pane)
                        +NSImageNameGoRightTemplate+))

(defmethod total-pages ((pane items-sheet))
  (ceiling (total-items pane)
           (items-per-page pane)))

(defmethod update-list-display ((pane items-sheet) &rest initargs 
                                &key  &allow-other-keys)
  (format t "~%Called update-list-display from ~S..." pane))

(defun handle-item-selection (item interface)
  (format t "~%Selected item ~S from interface ~S"
          item interface))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $zippath))))

;;; this may crash because it's not in apply-in-pane-process:
;;; (time (inc-list-page $win))

;;; (defparameter $moviespath "/Users/mikel/Desktop/Movies.delectus2")
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $moviespath))))

;;; (setf $screen (convert-to-screen))
;;; (describe $screen)

;;; opening test data
;;; (defparameter $words1k-path "/Users/mikel/Desktop/wordtest1k.delectus2")
;;; ~0.07sec to open, paging is instant
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $words1k-path))))

;;; (defparameter $words10k-path "/Users/mikel/Desktop/wordtest10k.delectus2")
;;; ~0.14sec to open, paging is instant
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $words10k-path))))

;;; (defparameter $words100k-path "/Users/mikel/Desktop/wordtest100k.delectus2")
;;; ~1.0sec to open
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $words100k-path))))
;;; ~0.5 sec to page


