;;;; sync.lisp

(in-package #:engine)

;;; implementation of change logs and the change protocol

(conspack:define-index change-operations
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

;;; (conspack:encode '(:UPDATE-FIELD "foo" 0 "bar" "Fred"))
;;; (conspack:decode (conspack:encode '(:UPDATE-FIELD "foo" 0 "bar" "Fred")))
