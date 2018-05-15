(in-package :message)

;;; ---------------------------------------------------------------------
;;; indexed symbols
;;; ---------------------------------------------------------------------
;;; defining this index enables conspack to represent the symbols as
;;; integers, greatly shrinking the size of our messages

(define-index change-operations
    ;; message fields
    :list-id
  :collection-id
  :parent-state
  :row-id
  :column-label
  :new-value
  :old-value
  :title
  :new-title
  :old-title
  :notes
  :new-notes
  :old-notes

  ;; field operations
  :update-field

  ;; row operations
  :add-row
  :mark-row-deleted
  :destroy-row

  ;; column operations
  :add-column
  :rename-column
  :mark-column-deleted
  :destroy-column

  ;; list operations
  :create-list
  :rename-list
  :mark-list-deleted
  :destroy-list
  :update-list-notes

  ;; collection operations
  :create-collection
  :rename-collection
  :mark-collection-deleted
  :destroy-collection
  :update-collection-notes
  )

;;; ---------------------------------------------------------------------
;;; Fields
;;; ---------------------------------------------------------------------

(defun update-field (&key
                       (list-id nil)
                       (parent-state nil)
                       (row-id nil)
                       (column-label nil)
                       (new-value nil)
                       (old-value nil))
  `(:UPDATE-FIELD :list-id ,list-id
                  :parent-state ,parent-state
                  :row-id ,row-id
                  :column-label ,column-label
                  :new-value ,new-value
                  :old-value ,old-value))

;;; (defparameter $list-id (str "list:" (uuid:make-v4-uuid)))
;;; (defparameter $parent-state (hash (encode '(:some "stuff"))))
;;; (defparameter $row-id (str "row:" (uuid:make-v4-uuid)))
;;; (defparameter $column-label "Fruits")
;;; (defparameter $new-value 101)
;;; (defparameter $old-value 1)
;;; (message:update-field $list-id $parent-state $row-id $column-label $new-value $old-value)

;;; ---------------------------------------------------------------------
;;; Rows
;;; ---------------------------------------------------------------------

(defun add-row (&key
                  (list-id nil)
                  (parent-state nil))
  `(:ADD-ROW :list-id ,list-id
             :parent-state ,parent-state))

;;; (defparameter $list-id (str "list:" (uuid:make-v4-uuid)))
;;; (defparameter $parent-state (hash (encode '(:some "stuff"))))
;;; (print-message (message-add-row $list-id $parent-state))

(defun mark-row-deleted (&key
                           (list-id nil)
                           (parent-state nil)
                           (row-id nil))
  `(:MARK-ROW-DELETED :list-id ,list-id
                      :parent-state ,parent-state
                      :row-id ,row-id))

(defun destroy-row (&key
                      (list-id nil)
                      (parent-state nil)
                      (row-id nil))
  `(:DESTROY-ROW :list-id ,list-id
                 :parent-state ,parent-state
                 :row-id ,row-id))

;;; ---------------------------------------------------------------------
;;; Columns
;;; ---------------------------------------------------------------------

(defun add-column (&key
                     (list-id nil)
                     (parent-state nil)
                     (column-label nil))
  `(:ADD-COLUMN :list-id ,list-id
                :parent-state ,parent-state
                :column-label ,column-label))

(defun rename-column (&key
                        (list-id nil)
                        (parent-state nil)
                        (new-column-label nil)
                        (old-column-label nil))
  `(:RENAME-COLUMN :list-id ,list-id
                   :parent-state ,parent-state
                   :new-column-label ,new-column-label
                   :old-column-label ,old-column-label))

(defun mark-column-deleted (&key
                              (list-id nil)
                              (parent-state nil)
                              (column-label nil))
  `(:MARK-COLUMN-DELETED :list-id ,list-id
                         :parent-state ,parent-state
                         :column-label ,column-label))

(defun destroy-column (&key
                         (list-id nil)
                         (parent-state nil)
                         (column-label nil))
  `(:DESTROY-COLUMN :list-id ,list-id
                    :parent-state ,parent-state
                    :column-label ,column-label))


;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defun create-list (&key
                      (title nil)
                      (notes nil))
  `(:CREATE-LIST :title ,title
                 :notes ,notes))

(defun rename-list (&key
                      (list-id nil)
                      (parent-state nil)
                      (new-title nil)
                      (old-title nil))
  `(:RENAME-LIST :list-id ,list-id
                 :parent-state ,parent-state
                 :new-title ,new-title
                 :old-title ,old-title))

(defun mark-list-deleted (&key
                            (list-id nil)
                            (parent-state nil))
  `(:MARK-LIST-DELETED :list-id ,list-id
                       :parent-state ,parent-state))

(defun update-list-notes (&key
                            (list-id nil)
                            (parent-state nil)
                            (new-notes nil)
                            (old-notes nil))
  `(:UPDATE-LIST-NOTES :list-id ,list-id
                       :parent-state ,parent-state
                       :new-notes ,new-notes
                       :old-notes ,old-notes))

(defun destroy-list (&key
                       (list-id nil))
  `(:DESTROY-LIST :list-id ,list-id))


;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

(defun create-collection (&key
                      (title nil))
  `(:CREATE-COLLECTION :title ,title))

(defun rename-collection (&key
                      (collection-id nil)
                      (parent-state nil)
                      (new-title nil)
                      (old-title nil))
  `(:RENAME-COLLECTION :collection-id ,collection-id
                 :parent-state ,parent-state
                 :new-title ,new-title
                 :old-title ,old-title))

(defun mark-collection-deleted (&key
                            (collection-id nil)
                            (parent-state nil))
  `(:MARK-COLLECTION-DELETED :collection-id ,collection-id
                       :parent-state ,parent-state))

(defun update-collection-notes (&key
                                  (collection-id nil)
                                  (parent-state nil)
                                  (new-notes nil)
                                  (old-notes nil))
  `(:UPDATE-COLLECTION-NOTES :collection-id ,collection-id
                             :parent-state ,parent-state
                             :new-notes ,new-notes
                             :old-notes ,old-notes))

(defun destroy-collection (&key
                             (collection-id nil))
  `(:DESTROY-COLLECTION :collection-id ,collection-id))

