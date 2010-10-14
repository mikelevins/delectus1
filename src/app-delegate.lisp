(in-package :delectus)

(defparameter *app-delegate* nil)

(define-objc-class app-delegate ()
  ()
  (:objc-class-name "AppDelegate"))


(defun app-delegate ()
  (unless *app-delegate*
    (setf *app-delegate* (make-instance 'app-delegate)))
  *app-delegate*)

;;; (app-delegate)