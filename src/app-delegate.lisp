(in-package :delectus)

(defparameter *app-delegate* nil)

(define-objc-class app-delegate ()
  ()
  (:objc-class-name "AppDelegate"))

;;; ---------------------------------------------------------------------
;;; widget tracking
;;; ---------------------------------------------------------------------

(let ((next-id 0))
  (defun next-widget-id ()
    (let ((id next-id))
      (setf next-id (1+ next-id))
      id)))

;;; ---------------------------------------------------------------------
;;; fetching the delegate
;;; ---------------------------------------------------------------------

(defun app-delegate ()
  (unless *app-delegate*
    (setf *app-delegate* (make-instance 'app-delegate)))
  *app-delegate*)

;;; (app-delegate)