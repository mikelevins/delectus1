(in-package :message)

;;; ---------------------------------------------------------------------
;;; indexed symbols
;;; ---------------------------------------------------------------------
;;; defining this index enables conspack to represent the symbols as
;;; integers, greatly shrinking the size of our messages

(define-index change-operations
  ;; message fields
  :collection-id
  :column-label
  :list-id
  :new-notes
  :new-title
  :new-value
  :notes
  :old-notes
  :old-title
  :originator-timestamp
  :old-value
  :parent-state
  :receiver-timestamp
  :row-id
  :title

  ;; field operations
  :update-field

  ;; row operations
  :add-row
  :mark-row-deleted

  ;; column operations
  :add-column
  :rename-column
  :mark-column-deleted

  ;; list operations
  :create-list
  :rename-list
  :mark-list-deleted
  :update-list-notes

  ;; collection operations
  :create-collection
  :rename-collection
  :mark-collection-deleted
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
                       (old-value nil)
                       (originator-timestamp nil)
                       (receiver-timestamp nil))
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
                  (parent-state nil)
                  (originator-timestamp nil)
                  (receiver-timestamp nil))
  `(:ADD-ROW :list-id ,list-id
             :parent-state ,parent-state))

;;; (defparameter $list-id (str "list:" (uuid:make-v4-uuid)))
;;; (defparameter $parent-state (hash (encode '(:some "stuff"))))
;;; (print-message (message-add-row $list-id $parent-state))

(defun mark-row-deleted (&key
                           (list-id nil)
                           (parent-state nil)
                           (row-id nil)
                           (originator-timestamp nil)
                       (receiver-timestamp nil))
  `(:MARK-ROW-DELETED :list-id ,list-id
                      :parent-state ,parent-state
                      :row-id ,row-id))

;;; ---------------------------------------------------------------------
;;; Columns
;;; ---------------------------------------------------------------------

(defun add-column (&key
                     (list-id nil)
                     (parent-state nil)
                     (column-label nil)
                     (originator-timestamp nil)
                     (receiver-timestamp nil))
  `(:ADD-COLUMN :list-id ,list-id
                :parent-state ,parent-state
                :column-label ,column-label))

(defun rename-column (&key
                        (list-id nil)
                        (parent-state nil)
                        (new-column-label nil)
                        (old-column-label nil)
                        (originator-timestamp nil)
                        (receiver-timestamp nil))
  `(:RENAME-COLUMN :list-id ,list-id
                   :parent-state ,parent-state
                   :new-column-label ,new-column-label
                   :old-column-label ,old-column-label))

(defun mark-column-deleted (&key
                              (list-id nil)
                              (parent-state nil)
                              (column-label nil)
                              (originator-timestamp nil)
                              (receiver-timestamp nil))
  `(:MARK-COLUMN-DELETED :list-id ,list-id
                         :parent-state ,parent-state
                         :column-label ,column-label))


;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defun create-list (&key
                      (title nil)
                      (notes nil)
                      (originator-timestamp nil)
                      (receiver-timestamp nil))
  `(:CREATE-LIST :title ,title
                 :notes ,notes))

(defun rename-list (&key
                      (list-id nil)
                      (parent-state nil)
                      (new-title nil)
                      (old-title nil)
                      (originator-timestamp nil)
                      (receiver-timestamp nil))
  `(:RENAME-LIST :list-id ,list-id
                 :parent-state ,parent-state
                 :new-title ,new-title
                 :old-title ,old-title))

(defun mark-list-deleted (&key
                            (list-id nil)
                            (parent-state nil)
                            (originator-timestamp nil)
                            (receiver-timestamp nil))
  `(:MARK-LIST-DELETED :list-id ,list-id
                       :parent-state ,parent-state))

(defun update-list-notes (&key
                            (list-id nil)
                            (parent-state nil)
                            (new-notes nil)
                            (old-notes nil)
                            (originator-timestamp nil)
                            (receiver-timestamp nil))
  `(:UPDATE-LIST-NOTES :list-id ,list-id
                       :parent-state ,parent-state
                       :new-notes ,new-notes
                       :old-notes ,old-notes))

;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

(defun create-collection (&key
                            (title nil)
                            (originator-timestamp nil)
                            (receiver-timestamp nil))
  `(:CREATE-COLLECTION :title ,title))

(defun rename-collection (&key
                            (collection-id nil)
                            (parent-state nil)
                            (new-title nil)
                            (old-title nil)
                            (originator-timestamp nil)
                            (receiver-timestamp nil))
  `(:RENAME-COLLECTION :collection-id ,collection-id
                       :parent-state ,parent-state
                       :new-title ,new-title
                       :old-title ,old-title))

(defun mark-collection-deleted (&key
                                  (collection-id nil)
                                  (parent-state nil)
                                  (originator-timestamp nil)
                                  (receiver-timestamp nil))
  `(:MARK-COLLECTION-DELETED :collection-id ,collection-id
                             :parent-state ,parent-state))

(defun update-collection-notes (&key
                                  (collection-id nil)
                                  (parent-state nil)
                                  (new-notes nil)
                                  (old-notes nil)
                                  (originator-timestamp nil)
                                  (receiver-timestamp nil))
  `(:UPDATE-COLLECTION-NOTES :collection-id ,collection-id
                             :parent-state ,parent-state
                             :new-notes ,new-notes
                             :old-notes ,old-notes))


