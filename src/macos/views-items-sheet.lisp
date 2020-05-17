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
                :change-callback 'update-items-sheet-for-changed-filter)
   (previous-button push-button :reader previous-button :text ""
                    :external-min-width 28 :external-max-width 28
                    :external-min-height 32 :external-max-height 32
                    :callback 'handle-previous-button-click)
   (next-button push-button :reader next-button :text ""
                :external-min-width 28 :external-max-width 28
                :external-min-height 32 :external-max-height 32
                :callback 'handle-next-button-click)
   (item-count-pane title-pane :reader item-count-pane)
   (current-page-label title-pane :reader current-page-label :text "Page ")
   (current-page-pane text-input-pane :reader current-page-pane
                      :callback 'handle-current-page-edit
                      :callback-type :interface-data
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
  (with-open-database (db (dbpath pane))
    (let* ((listname (listname-op-name (db-get-latest-listname-op db)))
           (column-data (columns-op-userdata (db-get-latest-columns-op db)))
           (column-labels (mapcar (lambda (col)(getf col :|label| nil))
                                  column-data))
           (column-names (mapcar (lambda (col)(getf col :|name| nil))
                                 column-data))
           
           (filter-text (text-input-pane-text (filter-pane pane)))
           (itemdata (if (empty? filter-text)
                         (mapcar #'item-op-userdata
                                 (db-get-latest-items db
                                                      :offset (* (items-per-page pane)
                                                                 (current-page pane))
                                                      :limit (items-per-page pane)))
                         (db-get-latest-filtered-items db
                                                       :column-labels column-labels
                                                       :filter-text filter-text
                                                       :offset (* (items-per-page pane)
                                                                  (current-page pane))
                                                       :limit (items-per-page pane))))
           (itemcount (if (empty? filter-text)
                          (db-count-latest-items db)
                          (db-count-latest-filtered-items db
                                                          :column-labels column-labels
                                                          :filter-text filter-text
                                                          :offset 0
                                                          :limit nil)))
           (column-widths (mapcar (lambda (txt)(+ 4 (length txt)))
                                  (first itemdata)))
           (column-specs (mapcar (lambda (name width) `(:title ,name :default-width (:character ,width)))
                                 column-names column-widths)))
      (setf (interface-title pane) listname)
      (setf (total-items pane) itemcount)
      (modify-multi-column-list-panel-columns (items-pane pane) :columns column-specs)
      (setf (title-pane-text (item-count-pane pane)) 
            (format nil " ~:D items" (total-items pane)))
      (setf (text-input-pane-text (current-page-pane pane)) 
            (format nil "~A" (1+ (current-page pane))))
      (setf (title-pane-text (page-range-pane pane)) 
            (format nil " of ~D"
                    (total-pages pane)))
      (setf (collection-items (items-pane pane))
            itemdata)
      nil)))


(defun update-items-sheet-for-changed-filter (text filter-pane sheet-pane caret-position)
  (declare (ignore filter-pane caret-position))
  (setf (current-page sheet-pane) 0)
  (update-list-display sheet-pane :filter-text text))


(defun handle-item-selection (item interface)
  (format t "~%Selected item ~S from interface ~S"
          item interface))

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
  (declare (ignore data))
  (dec-list-page interface))

(defun handle-next-button-click (data interface)
  (declare (ignore data))
  (inc-list-page interface))

(defun handle-current-page-edit (items-sheet proposed-page-text)
  (let ((proposed-page (parse-integer proposed-page-text :junk-allowed t))
        (old-page (current-page items-sheet)))
    (if proposed-page
        (if (<= 0 (1- proposed-page)(1- (total-pages items-sheet)))
            (progn (setf (current-page items-sheet)
                         (1- proposed-page))
                   (update-list-display items-sheet))
            (setf (text-input-pane-text (current-page-pane items-sheet)) 
                  (format nil "~A" (1+ old-page))))
        (setf (text-input-pane-text (current-page-pane items-sheet)) 
              (format nil "~A" (1+ old-page))))))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $zippath))))

;;; (defparameter $moviespath "/Users/mikel/Desktop/Movies.delectus2")
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $moviespath))))

;;; (setf $screen (convert-to-screen))
;;; (describe $screen)

;;; opening test data
;;; (setf $wordtest100-path (path "~/Desktop/wordtest100.delectus2"))
;;; ~1sec
;;; (time (make-test-list $wordtest100-path :count 100))
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $wordtest100-path))))
;;; (delete-file $wordtest100-path)

;;; opening test data
;;; (setf $wordtest1k-path (path "~/Desktop/wordtest1k.delectus2"))
;;; ~8sec:
;;; (time (make-test-list $wordtest1k-path :count 1000))
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $wordtest1k-path))))
;;; (delete-file $wordtest1k-path)

;;; opening test data
;;; (setf $wordtest10k-path (path "~/Desktop/wordtest10k.delectus2"))
;;; ~7m2sec:
;;; (time (make-test-list $wordtest10k-path :count 10000))
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $wordtest10k-path))))
;;; (delete-file $wordtest10k-path)

;;; opening test data
;;; (setf $wordtest100k-path (path "~/Desktop/wordtest100k.delectus2"))
;;; ~16m4sec:
;;; (time (make-test-list $wordtest100k-path :count 100000))
;;; (time (setf $win (contain (make-instance 'items-sheet :dbpath $wordtest100k-path))))
;;; (delete-file $wordtest100k-path)
