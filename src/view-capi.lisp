;;;; ***********************************************************************
;;;;
;;;; Name:          view-capi.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Delectus Desktop UI
;;;; Author:        mikel evins
;;;; Copyright:     2017 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; view classes
;;; ---------------------------------------------------------------------

(define-interface delectus-ui ()
  ;; -- slots ---------------------------------------------
  ((document :accessor document :initform nil :initarg :document))

  ;; -- panes ---------------------------------------------
  (:panes
   (contents-pane multi-column-list-panel :reader contents-pane
                  :alternating-background t
                  :columns (compute-column-descriptions document)
                  :items (compute-visible-rows document))
   (count-pane title-pane :reader count-pane :text (format nil "some items")))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(contents-pane count-pane)
                :reader main-layout :border 4))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :width 800 :height 600))

;;; dummy method
(defmethod compute-column-descriptions ((document null))
  `((:title "" :default-width 96)))

(defmethod compute-column-descriptions ((document document))
  (let* ((column-labels (visible-column-labels document)))
    (mapcar (lambda (lbl) `(:title ,lbl :default-width 96))
            column-labels)))



(defmethod compute-visible-rows ((document null)) nil)

(defmethod compute-visible-rows ((document document))
  (visible-rows document
                (visible-column-labels document)))


