;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       document object
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; document
;;; ---------------------------------------------------------------------

(defclass document ()
  ((title :accessor title :initarg :title :initform (next-untitled-name))
   (pathname :accessor pathname :initarg :pathname :initform nil)
   (presentation :accessor presentation :initarg :presentation :initform (make-default-presentation))
   (window :reader window :initform nil)))

(defmethod initialize-instance :after ((doc document) &rest initargs &key &allow-other-keys)
  (setf (slot-value doc 'window)
        (make-instance 'document-window :document doc))
  (display (window doc)))

;;; (setq $doc (make-instance 'document))

