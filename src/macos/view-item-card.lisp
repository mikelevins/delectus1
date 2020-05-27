;;;; ***********************************************************************
;;;;
;;;; Name:          view-item-card.lisp
;;;; Project:       delectus 2
;;;; Purpose:       UI: a single-item pane
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; parameters
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; item-pane
;;; ---------------------------------------------------------------------

(define-interface item-card ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (columns-data :accessor columns-data :initform nil :initarg :columns-data)
   (item-data :accessor item-data :initform nil :initarg :item-data))

  ;; -- panes ---------------------------------------------
  (:panes)
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '()
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :width 800 :height 600))

;;; ---------------------------------------------------------------------
;;; initialize-instance
;;; ---------------------------------------------------------------------

(defmethod initialize-instance :after ((pane item-card) &rest initargs 
                                       &key &allow-other-keys)
  (let ((column-names ))
    ))

;;; ---------------------------------------------------------------------
;;; handlers and helpers
;;; ---------------------------------------------------------------------

;;; (setf $zips-test-path (path "~/Desktop/Zipcodes.delectus2"))
;;; (setf $coldata (get-latest-columns-op $zips-test-path))
;;; (setf $item (get-specified-item $zips-test-path 1))

;;; (columns-op-userdata $coldata)

;;; (item-op-itemid (first $item))
;;; (item-op-revision (first $item))
;;; (item-op-origin (first $item))
;;; (delectus-timestamp->local-time (item-op-timestamp (first $item)))
;;; (item-op-deleted (first $item))
;;; (item-op-userdata (first $item))
