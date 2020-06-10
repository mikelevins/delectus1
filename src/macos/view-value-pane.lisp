;;;; ***********************************************************************
;;;;
;;;; Name:          view-value-pane.lisp
;;;; Project:       delectus 2
;;;; Purpose:       a simple-pane for containing labels and editors
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; item-pane
;;; ---------------------------------------------------------------------

(define-interface value-pane ()
  ;; -- slots ---------------------------------------------
  ()

  ;; -- panes ---------------------------------------------
  (:panes)
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout simple-layout '() :reader main-layout))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout :visible-border t))

(defmethod initialize-instance :after ((pane value-pane) &rest initargs 
                                       &key
                                         (data-pane nil)
                                         &allow-other-keys)
  (setf (layout-description (main-layout pane))
        (list data-pane)))

;;; (setf $movies (delectus::path "~/Desktop/Movies.delectus2"))
;;; (setf $items (delectus::get-latest-items $movies))
;;; (setf $it (elt $items 0))

;;; (setf $win (contain (make-instance 'value-pane :data-pane (make-instance 'title-pane :text "Foo"))))

;;; (setf $win (contain (make-instance 'simple-pinboard-layout :visible-border t :description (list (make-instance 'title-pane :text "Foo")))))


;;; (setf $win (contain (make-instance 'title-pane :text "Foobar Baz..." :title-position :frame :title "Foo" :visible-border t)))
