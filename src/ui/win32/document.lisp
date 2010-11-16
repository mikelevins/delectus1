;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          views.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       Windows-specific document code
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;;  document
;;; ---------------------------------------------------------------------


(defclass document ()
  ((name :reader name :initarg :name :initform nil)
   (changed? :accessor changed? :initform t)
   (pathname :accessor pathname :initarg :pathname :initform nil)
   (window :reader window :initarg :window :initform nil)
   (presentation :reader presentation :initarg :presentation :initform (make-instance 'presentation))))

(defmethod initialize-instance :after ((doc document) &rest initargs &key (name "Untitled") (presentation nil)
                                       &allow-other-keys)
  (let ((pres (or presentation (make-instance 'presentation))))
    (setf (slot-value doc 'name)  name)
    (setf (slot-value doc 'window)(make-instance 'delectus-window :document doc))))

(defmethod notify-redisplay-document ((doc document))
  )

(defmethod notify-document-changed! ((doc document))
  )

