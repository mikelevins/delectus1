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
  ((document-path :accessor document-path :initform nil :initarg :document-path))

  ;; -- panes ---------------------------------------------
  (:panes
   (contents-pane multi-column-list-panel :reader contents-pane
                  :alternating-background t
                  :columns (compute-column-descriptions interface)
                  :items `(("Home Sweet Home" "Joe" "blah blah")
                           ("Another Title" "A. Nother Author" "various gibberish")
                           ("Duke" "L. Lington" "she doob e")))
   (count-pane title-pane :reader count-pane :text (format nil "some items")))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(contents-pane count-pane)
                :reader main-layout :border 4))

  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
   :width 800 :height 600))

(defmethod compute-column-descriptions ((ui delectus-ui))
  `((:title "Title" :default-width 96)(:title "Byline" :default-width 96)(:title "Credits" :default-width 96)))
