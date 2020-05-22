;;;; ***********************************************************************
;;;;
;;;; Name:          view-item-pane.lisp
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

(define-interface item-pane ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (item-data :accessor item-data :initform nil :initarg :item-data))

  ;; -- panes ---------------------------------------------
  (:panes
   )
  
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

(defmethod initialize-instance :after ((pane item-pane) &rest initargs 
                                       &key &allow-other-keys)
  )

;;; ---------------------------------------------------------------------
;;; handlers and helpers
;;; ---------------------------------------------------------------------


;;; (setf $zips-test-path (path "~/Desktop/Zipcodes.delectus2"))
;;; (time (count-latest-items (pathname $zips-test-path)))
;;; (time (get-latest-items (pathname $zips-test-path) :offset 43190 :limit 1))
;;; (time (setf $win (contain (make-instance 'item-pane))))
