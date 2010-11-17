;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          application.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       the Delectus application object
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defclass application ()
  ((documents :accessor documents :initform nil)
   (active-interface :accessor %active-interface :initform nil)
   (untitled-index :accessor %untitled-index :initform 0))
  (:metaclass singleton-class))

(defun app ()
  (make-instance 'application))

(defmethod untitled-index ()
  (setf (%untitled-index (app))(1+ (%untitled-index (app))))
  (%untitled-index (app)))

(defmethod active-interface ((app application))
  (%active-interface (app)))

(defmethod activate-interface ((intf interface))
  (setf (%active-interface (app)) intf))

(defmethod new-untitled-document ()
  (let* ((document-name (format nil "Untitled ~A" (untitled-index)))
         (model (make-instance 'model))
         (presentation (make-instance 'presentation :model model))
         (document (make-instance 'document :name document-name :presentation presentation)))
    (setf (documents (app))(cons document (documents (app))))
    (show document)
    document))

(defmethod find-document ((win interface))
  (seq:find (^ (doc)(equal win (window doc))) 
            (documents (app))))
