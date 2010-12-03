;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          item.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the Delectus data model
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defclass item ()
  ((value :type string :reader value :initform "" :initarg :value)))

(defmethod %set-value! ((it item)(val string))
  (with-slots (value) it
    (setf value val)))

(defsetf value %set-value!)

(defmethod make-item ((val string))
  (make-instance 'item :value val))

(defmethod ensure-item ((it item)) it)

(defmethod ensure-item ((val string))
  (make-item val))

(defmethod print-object ((it item)(s stream))
  (print-unreadable-object (it s :type t)
    (format s "~A" (value it))))

;;; (setq $it (make-item "Foo"))
;;; (setf (value $it) "Bar")
;;; $it
