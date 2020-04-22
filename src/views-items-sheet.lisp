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
(in-readtable :delectus)

;;; ---------------------------------------------------------------------
;;; items-sheet
;;; ---------------------------------------------------------------------

(define-interface items-sheet ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (total-items :accessor total-items :initform 0 :initarg :total-items)
   (items-per-page :accessor items-per-page :initform 50 :initarg :items-per-page)
   (current-page :accessor current-page :initform 0 :initarg :current-page))

  ;; -- panes ---------------------------------------------
  (:panes
   (items-pane multi-column-list-panel :reader items-pane
               :alternating-background t
               :item-print-function (lambda (it)
                                      (if (null it) "" it))
               :items nil
               :columns '((:title "Item"))
               :callback-type :item-interface
               :selection-callback 'handle-item-selection)
   (filter-pane text-input-pane :search-field "Filter"
                :change-callback 'update-items-sheet-for-changed-filter)
   (previous-button push-button :reader previous-button :text ""
                    :external-min-width 28 :external-max-width 28
                    :external-min-height 32 :external-max-height 32
                    :callback #'handle-previous-button-click)
   (next-button push-button :reader next-button :text ""
                :external-min-width 28 :external-max-width 28
                :external-min-height 32 :external-max-height 32
                :callback #'handle-next-button-click)
   (item-count-pane title-pane :reader item-count-pane)
   (item-range-pane title-pane :reader item-range-pane))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (pager-layout row-layout '(previous-button item-range-pane next-button) :adjust :center)
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
        (delectus::count-latest-items (dbpath pane)))
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
                                &key (filter-text nil) &allow-other-keys)
  (with-open-database (db (dbpath pane))
    (let* ((list-name-op (get-latest-listname (dbpath pane)))
           (listname (or (op-name list-name-op) "Untitled list"))
           (latest-column-data (get-latest-columns (dbpath pane)))
           (latest-column-userdata (mapcar #'from-json (op-userdata latest-column-data)))
           (column-names (mapcar (lambda (ud)(fset:@ ud :|name|)) latest-column-userdata))
           (column-ids (mapcar (lambda (ud)(fset:@ ud :|id|)) latest-column-userdata))
           (column-widths (mapcar #'(lambda (x)(+ 4 x))
                                  (delectus::get-userdata-column-widths (dbpath pane))))
           (column-specs (mapcar (lambda (name width) `(:title ,name :default-width (:character ,width)))
                                 column-names column-widths))
           (itemdata (delectus::get-latest-items-userdata (dbpath pane)
                                                          :column-ids column-ids
                                                          :like filter-text
                                                          :offset (* (items-per-page pane)
                                                                     (current-page pane))
                                                          :limit (items-per-page pane))))
      (setf (interface-title pane) listname)
      (modify-multi-column-list-panel-columns (items-pane pane) :columns column-specs)
      (setf (title-pane-text (item-count-pane pane)) 
            (format nil " ~:D pages (~:D items)"
                    (total-pages pane) (total-items pane)))
      (setf (title-pane-text (item-range-pane pane)) 
            (format nil "Page ~D"
                    (1+ (current-page pane))))
      (setf (collection-items (items-pane pane))
            itemdata))))

(defun update-items-sheet-for-changed-filter (text filter-pane sheet-pane caret-position)
  (update-list-display sheet-pane :filter-text text))

(defun dec-list-page (items-sheet)
  (let ((next-page (1- (current-page items-sheet))))
    (when (>= next-page 0)
      (decf (current-page items-sheet))))
  (update-list-display items-sheet))

(defun inc-list-page (items-sheet)
  (let* ((itemcount (total-items items-sheet))
         (next-start-index (* (items-per-page items-sheet)
                              (1+ (current-page items-sheet)))))
    (when (< next-start-index itemcount)
      (incf (current-page items-sheet))))
  (update-list-display items-sheet))

(defun handle-previous-button-click (data interface)
  (dec-list-page interface))

(defun handle-next-button-click (data interface)
  (inc-list-page interface))

(defun handle-item-selection (item interface)
  (format t "~%Selected item ~S from interface ~S"
          item interface))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $zippath))))

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
;;; (time (inc-list-page $win))
