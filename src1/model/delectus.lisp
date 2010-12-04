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
;;; A delectus is conceptually a list of selected things.
;;; it is represented as a list of items

(defclass delectus ()
  ((items :accessor items :initarg :items :initform nil)))

(defun make-delectus (&rest vals)
  (make-instance 'delectus :items (mapcar #'ensure-item vals)))

(defmethod print-object ((del delectus)(s stream))
  (print-unreadable-object (del s :type t)
    (format s "~{~A ~}" (mapcar #'value (items del)))))

;;; (setq $d (make-delectus "Fred" "Flintstone" "Bedrock"))