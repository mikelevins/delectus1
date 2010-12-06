;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the Delectus data model
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; "delectus" is from the Latin "delectum", meaning something
;;; chosen. A delectus is an ordered set of selected items.

(defclass delectus ()
  ((keys :reader keys :initform (make-stretchy-vector) :initarg :keys)
   (items :reader items :initform (make-stretchy-vector) :initarg :items)))

(defun make-delectus ()
  (make-instance 'delectus))

(defmethod add-item! ((del delectus)(it item))
  (loop for attr in (as 'list (attributes it))
     do (add-key! del attr))
  (add-elt! (items del) it)
  del)

(defmethod remove-item! ((del delectus)(index integer))
  (remove-elt! (items del) index))

(defmethod add-key! ((del delectus)(k symbol))
  (unless (find k (keys del))
    (add-elt! (keys del) k))
  del)

(defmethod add-key! ((del delectus)(k string))
  (add-key! del (item-key k))
  del)

(defmethod remove-key! ((del delectus)(k symbol))
  (let ((k-index (position k (keys del))))
    (remove-elt! (keys del) k-index)
    (loop for it across (items del)
       do (delete! it k)))
  del)

(defmethod remove-key! ((del delectus)(k string))
  (remove-key! del (item-key k))
  del)

