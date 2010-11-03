(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; application
;;; ---------------------------------------------------------------------

(defclass application ()
  ((documents :accessor documents :initform nil)
   (untitled-index :accessor untitled-index :initform 0))
  (:metaclass singleton-class))

(defmethod untitled-index ((app application))
  (setf (untitled-index app)(1+ (untitled-index app)))
  (untitled-index app))

(defmethod new-untitled-document ((app application))
  (let* ((document-name (format nil "Untitled ~A" (untitled-index app)))
         (doc (make-instance 'document :name document-name)))
    (setf (documents app)(cons doc (documents app)))
    (show doc)
    doc))

(defun app ()
  (make-instance 'application))

(defmethod find-document ((win interface))
  (seq:find (^ (doc)(equal win (window doc))) 
            (documents (app))))

;;; (app)