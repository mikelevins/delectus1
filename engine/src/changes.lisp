;;;; sync.lisp

(in-package :engine)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; implementation of change logs and the change protocol

(defclass changelog ()
  ((changes :accessor changes :initform nil :initarg :changes)))

(defmethod add-change ((log changelog)(message cons))
  (msg:validate-change-message message)
  (push message (changes log))
  log)

(defmethod get-state-token ((log changelog))
  (sha3:sha3-digest-vector (encode (changes log))))

;;; (defparameter $log (make-instance 'changelog))
;;; (defparameter $list-id (make-identity))
;;; (add-change $log (msg:create-list :list-id $list-id :timestamp (timestamp-now)))
