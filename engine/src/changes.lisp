;;;; sync.lisp

(in-package :engine)

;;; implementation of change logs and the change protocol

(define-index change-operations
    ;; field operations
    :UPDATE-FIELD
  ;; row operations
  :ADD-ROW
  :MARK-ROW-DELETED
  :DESTROY-ROW
  ;; column operations
  :ADD-COLUMN
  :RENAME-COLUMN
  :MARK-COLUMN-DELETED
  :DESTROY-COLUMN
  ;; list operations
  :CREATE-LIST
  :RENAME-LIST
  :MARK-LIST-DELETED
  :DESTROY-LIST
  ;; collection operations
  :CREATE-COLLECTION
  :RENAME-COLLECTION
  :MARK-COLLECTION-DELETED
  :DESTROY-COLLECTION
  )

;;; Utilities
;;; ---------------------------------------------------------------------

(defun print-message (message-vector &optional (stream *standard-output*))
  (format stream "~S" (decode message-vector)))

;;; Fields
;;; ---------------------------------------------------------------------

(defun message-update-field (list-id state-token row-id column-label new-value &optional (old-value nil))
  (encode `(:UPDATE-FIELD ,list-id ,state-token ,row-id ,column-label ,new-value ,old-value)))

;;; (defparameter $list-id (str "list:" (uuid:make-v4-uuid)))
;;; (defparameter $state-token (hash (encode '(:some "stuff"))))
;;; (defparameter $row-id (str "row:" (uuid:make-v4-uuid)))
;;; (defparameter $column-label "Fruits")
;;; (defparameter $new-value 101)
;;; (defparameter $old-value 1)
;;; (print-message (message-update-field $list-id $state-token $row-id $column-label $new-value $old-value))

;;; Rows
;;; ---------------------------------------------------------------------

(defun message-add-row (list-id state-token)
  (encode `(:ADD-ROW ,list-id ,state-token)))


;;; (defparameter $list-id (str "list:" (uuid:make-v4-uuid)))
;;; (defparameter $state-token (hash (encode '(:some "stuff"))))
;;; (print-message (message-add-row $list-id $state-token))
