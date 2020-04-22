;;;; ***********************************************************************
;;;;
;;;; Name:          views-card-list.lisp
;;;; Project:       delectus 2
;;;; Purpose:       UI: a view of items as a list of cards
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)
(in-readtable :delectus)


;;; ---------------------------------------------------------------------
;;; item-card
;;; ---------------------------------------------------------------------

(define-interface item-card ()
  ;; -- slots ---------------------------------------------
  ((columns-data :accessor columns-data :initform nil :initarg :columns-data)
   (item-data :accessor item-data :initform nil :initarg :item-data))

  ;; -- panes ---------------------------------------------
  (:panes)
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '()
                :reader main-layout
                :border 8
                :gap 8))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 400 :height 300))

(defmethod initialize-instance :after ((card item-card) &rest initargs 
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
                                       (entrypane (make-instance 'title-pane
                                                                 :title-position :left
                                                                 :title (format nil "~A" colname)
                                                                 :title-font (gp:make-font-description 
                                                                              :size 12
                                                                              :slant :italic)
                                                                 :text (format nil "~A" colvalue)
                                                                 :font (gp:make-font-description 
                                                                        :size 16
                                                                        :slant :roman))))
                                  entrypane))
                              sorted-pairs)))
    (setf (layout-description (main-layout card))
          entry-views)))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (time (progn (setf $columns (delectus::get-latest-userdata-columns-data $zippath)) 'done))
;;; (time (progn (setf $items (mapcar #'delectus::op-userdata (delectus::get-latest-items $zippath))) 'done))
;;; (length $items)
;;; (setf $win (contain (make-instance 'item-card :columns-data $columns :item-data (elt $items 0))))

;;; (defparameter $moviespath "/Users/mikel/Desktop/Movies.delectus2")
;;; (time (progn (setf $columns (delectus::get-latest-userdata-columns-data $moviespath)) 'done))
;;; (time (progn (setf $items (delectus::get-latest-items-userdata $moviespath)) 'done))
;;; (length $items)
;;; (setf $win (contain (make-instance 'item-card :columns-data $columns :item-data (elt $items 0))))


;;; ---------------------------------------------------------------------
;;; card-list
;;; ---------------------------------------------------------------------

