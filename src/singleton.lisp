(in-package :delectus)

(defclass singleton-class (standard-class)
  ((instance :accessor instance :initform nil)))

(defmethod validate-superclass ((class singleton-class)(superclass standard-class)) t)
(defmethod validate-superclass ((class singleton-class)(superclass singleton-class)) t)
(defmethod validate-superclass ((class standard-class)(superclass singleton-class)) nil)

(defmethod make-instance ((class singleton-class) &key)
  (unless (instance class)
    (setf (instance class) (call-next-method)))
  (instance class))
