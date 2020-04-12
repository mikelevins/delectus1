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

(define-interface list-items-pane ()
  ;; -- slots ---------------------------------------------
  ((dbpath :accessor dbpath :initform nil :initarg :dbpath)
   (rows-per-page :accessor rows-per-page :initform 20 :initarg :rows-per-page)
   (current-page :accessor current-page :initform 0 :initarg :current-page))

  ;; -- panes ---------------------------------------------
  (:panes
   (rows-pane multi-column-list-panel :reader rows-pane
              :alternating-background t
              :items nil
              :columns '((:title "Item"))
              :callback-type :item-interface
              :selection-callback 'handle-item-selection))
  
  ;; -- layouts ---------------------------------------------
  (:layouts
   (main-layout column-layout '(rows-pane)
                :reader main-layout :border 4))
  
  ;; -- defaults ---------------------------------------------
  (:default-initargs :layout 'main-layout
    :width 600 :height 400
    :title "Delectus"))

(defmethod initialize-instance :after ((browser list-items-pane) &rest initargs &key &allow-other-keys)
  (let* ((list-name-op (delectus::get-latest-listname (dbpath browser)))
         (listname ))
    (format t "~%~s~%" list-name-op)
    ))

(defun handle-item-selection (item interface)
  (format t "~%Selected item ~S from interface ~S"
          item interface))

;;; (defparameter $zippath "/Users/mikel/Desktop/zipcodes.delectus2")
;;; (setf $win (contain (make-instance 'list-items-pane :dbpath $zippath)))
