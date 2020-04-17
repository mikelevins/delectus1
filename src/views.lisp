;;;; ***********************************************************************
;;;;
;;;; Name:          views.lisp
;;;; Project:       delectus 2
;;;; Purpose:       UI viwes of delectus daata
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; list-items-pane
;;; ---------------------------------------------------------------------

(define-interface list-items-pane ()
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
   (previous-button push-button :reader previous-button :text "<"
                    :callback #'handle-previous-button-click)
   (next-button push-button :reader next-button :text ">"
                :callback #'handle-next-button-click)
   (item-range-pane title-pane :reader item-range-pane)
   (item-count-pane title-pane :reader item-count-pane))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (pager-layout row-layout '(nil previous-button item-range-pane next-button item-count-pane nil) :adjust :center)
   (main-layout column-layout '(items-pane pager-layout)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :width 600 :height 400
   :title "Delectus"))

(defmethod initialize-instance :after ((pane list-items-pane) &rest initargs 
                                       &key (show-metadata nil) &allow-other-keys)
  (setf (total-items pane)
        (delectus::count-latest-items (dbpath pane)))
  (update-list-display pane :show-metadata show-metadata))

(defmethod total-pages ((pane list-items-pane))
  (ceiling (total-items pane)
           (items-per-page pane)))

(defmethod update-list-display ((pane list-items-pane) &rest initargs 
                                &key (show-metadata nil) &allow-other-keys)
  (let* ((metadata-column-count (length delectus::+metadata-column-labels+))
         (column-info (delectus::get-column-info (dbpath pane)))
         (metadata-column-info (subseq column-info 0 metadata-column-count))
         (metadata-column-names (mapcar #'delectus::column-info-name metadata-column-info))
         (userdata-column-info (subseq column-info metadata-column-count))
         (latest-column-data (delectus::get-latest-columns (dbpath pane)))
         (latest-userdata (mapcar #'delectus::from-json (delectus::op-userdata latest-column-data)))
         (userdata-column-names (mapcar (lambda (ud)(fset:@ ud :|name|)) latest-userdata))
         (column-names (if show-metadata
                           (append metadata-column-names userdata-column-names)
                         userdata-column-names))
         (column-specs (mapcar (lambda (cname) `(:title ,cname :default-width 96))
                               column-names))
         (list-name-op (delectus::get-latest-listname (dbpath pane)))
         (listname (or (delectus::op-name list-name-op) "Untitled list"))
         (latest-items (delectus::get-latest-items (dbpath pane) 
                                                   :offset (* (items-per-page pane)
                                                              (current-page pane))
                                                   :limit (items-per-page pane)))
         (itemdata (if show-metadata
                       latest-items
                     (mapcar #'delectus::op-userdata latest-items))))
    (setf (interface-title pane) listname)
    (modify-multi-column-list-panel-columns (items-pane pane) :columns column-specs)
    (setf (title-pane-text (item-range-pane pane)) 
            (format nil "Page ~D" (1+ (current-page pane))))
    (setf (title-pane-text (item-count-pane pane)) 
          (format nil " of ~D" (total-pages pane)))
    (setf (collection-items (items-pane pane))
           itemdata)))

(defun dec-list-page (list-items-pane)
  (let ((next-page (1- (current-page list-items-pane))))
    (when (>= next-page 0)
      (decf (current-page list-items-pane))))
  (update-list-display list-items-pane :show-metadata nil))

(defun inc-list-page (list-items-pane)
  (let* ((itemcount (total-items list-items-pane))
         (next-start-index (* (items-per-page list-items-pane)
                              (1+ (current-page list-items-pane)))))
    (when (< next-start-index itemcount)
      (incf (current-page list-items-pane))))
  (update-list-display list-items-pane :show-metadata nil))

(defun handle-previous-button-click (data interface)
  (dec-list-page interface))

(defun handle-next-button-click (data interface)
  (inc-list-page interface))

(defun handle-item-selection (item interface)
  (format t "~%Selected item ~S from interface ~S"
          item interface))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (time (setf $win (contain (make-instance 'list-items-pane :dbpath $zippath))))

;;; (defparameter $moviespath "/Users/mikel/Desktop/Movies.delectus2")
;;; (time (setf $win (contain (make-instance 'list-items-pane :dbpath $moviespath))))

;;; (setf $screen (convert-to-screen))
;;; (describe $screen)

;;; opening test data
;;; (defparameter $words1k-path "/Users/mikel/Desktop/wordtest1k.delectus2")
;;; ~0.07sec to open, paging is instant
;;; (time (setf $win (contain (make-instance 'list-items-pane :dbpath $words1k-path))))

;;; (defparameter $words10k-path "/Users/mikel/Desktop/wordtest10k.delectus2")
;;; ~0.14sec to open, paging is instant
;;; (time (setf $win (contain (make-instance 'list-items-pane :dbpath $words10k-path))))

;;; (defparameter $words100k-path "/Users/mikel/Desktop/wordtest100k.delectus2")
;;; ~0.8sec to open, paging is instant
;;; (time (setf $win (contain (make-instance 'list-items-pane :dbpath $words100k-path))))
;;; ~0.4 sec to page
;;; (time (inc-list-page $win))


;;; ---------------------------------------------------------------------
;;; list-item-card
;;; ---------------------------------------------------------------------

(define-interface list-item-card ()
  ;; -- slots ---------------------------------------------
  ((columns-data :accessor columns-data :initform nil :initarg :columns-data)
   (item-data :accessor item-data :initform nil :initarg :item-data))

  ;; -- panes ---------------------------------------------
  (:panes)
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '()
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 400 :height 300))

(defmethod initialize-instance :after ((card list-item-card) &rest initargs 
                                       &key &allow-other-keys)
  (let* ((column-value-pairs (mapcar #'cons
                                     (columns-data card)
                                     (item-data card)))
         (sorted-pairs (sort column-value-pairs
                             (lambda (left right)
                               (< (getf (car left) :|order|)
                                  (getf (car right) :|order|)))))
         (entry-views (mapcar (lambda (it)
                                (let* ((coldata (car it))
                                       (colname (getf coldata :|name|))
                                       (colvalue (cdr it))
                                       (label (make-instance 'title-pane :text colname))
                                       (valpane (make-instance 'title-pane :text (format nil "~A" colvalue)))
                                       (entrypane (make-instance 'row-layout :description (list label valpane))))
                                  entrypane))
                              sorted-pairs)))
    (setf (layout-description (main-layout card))
          entry-views)))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (time (progn (setf $columns (delectus::get-latest-userdata-columns-data $zippath)) 'done))
;;; (time (progn (setf $items (mapcar #'delectus::op-userdata (delectus::get-latest-items $zippath))) 'done))
;;; (length $items)
;;; (setf $win (contain (make-instance 'list-item-card :columns-data $columns :item-data (elt $items 0))))
