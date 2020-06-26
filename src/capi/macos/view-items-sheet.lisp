;;;; ***********************************************************************
;;;;
;;;; Name:          view-item-row.lisp
;;;; Project:       delectus 2
;;;; Purpose:       a row presentation of item values
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; item-row
;;; ---------------------------------------------------------------------

(define-interface items-sheet ()
  ;; -- slots ---------------------------------------------
  ()

  ;; -- panes ---------------------------------------------
  (:panes
   (items-pane multi-column-list-panel))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '() :reader main-layout))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout))

(defmethod initialize-instance :after ((row items-sheet) &rest initargs 
                                       &key
                                         &allow-other-keys)
  )

;;; (setf $movies (delectus::path "~/Desktop/Movies.delectus2"))
;;; (setf $items (delectus::get-latest-items $movies))
;;; (setf $it (elt $items 0))

;;; (setf $win (contain (make-instance 'items-sheet)))

