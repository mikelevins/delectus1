(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; application
;;; ---------------------------------------------------------------------

(defclass application ()
  ((documents :accessor documents :initform nil)
   (untitled-index :initform 0))
  (:metaclass singleton-class))

(defmethod untitled-index ((app application))
  (setf (slot-value app 'untitled-index)
        (1+ (slot-value app 'untitled-index)))
  (slot-value app 'untitled-index))

(defmethod new-untitled-document ((app application))
  (let* ((document-name (format nil "Untitled ~A" (untitled-index app)))
         (doc (make-instance 'document :name document-name)))
    (setf (documents app)
          (cons doc (documents app)))
    (show doc)))

(defun app ()
  (make-instance 'application))

;;; (app)