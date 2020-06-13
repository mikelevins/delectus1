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
  ((value :accessor value :initform nil :initarg :value))

  ;; -- panes ---------------------------------------------
  (:panes)
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (display-layout simple-pinboard-layout () :reader display-layout
                   :child (make-instance 'item-pinboard-object
                                         :text (value interface)
                                         :graphics-args
                                         (list :font
                                               (gp:find-best-font (convert-to-screen)
                                                                  (gp:make-font-description
                                                                   :size 14
                                                                   :slant :roman)))))
   (edit-layout simple-pinboard-layout () :reader edit-layout
                :child (make-instance 'item-pinboard-object
                                      :text (value interface)))
   (main-layout switchable-layout '(display-layout edit-layout) :reader main-layout))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout :visible-border t))

;;; (setf $movies (delectus::path "~/Desktop/Movies.delectus2"))
;;; (setf $items (delectus::get-latest-items $movies))
;;; (setf $it (elt $items 0))

;;; (setf $win (contain (make-instance 'value-pane :value "Foo")))


;;; :font (gp:make-font-description :size 14 :slant :roman)
