(in-package :message)

;;; ---------------------------------------------------------------------
;;; indexed symbols
;;; ---------------------------------------------------------------------
;;; defining this index enables conspack to represent the symbols as
;;; integers, greatly shrinking the size of our messages

(define-index change-operations
  ;; message attributes
  :collection-id
  :column-label
  :list-id
  :member-id
  :new-column-label
  :new-notes
  :new-title
  :new-value
  :old-column-label
  :old-notes
  :old-title
  :old-value
  :parent-state
  :row-id
  :timestamp

  ;; operations on fields
  :update-field

  ;; operations on rows
  :add-row
  :mark-row-deleted
  :unmark-row-deleted

  ;; operations on columns
  :add-column
  :rename-column
  :mark-column-deleted
  :unmark-column-deleted

  ;; operations on lists
  :create-list
  :rename-list
  :update-list-notes
  :mark-list-deleted
  :unmark-list-deleted

  ;; operations on collections
  :create-collection
  :rename-collection
  :update-collection-notes
  :add-member
  :mark-member-deleted
  :unmark-member-deleted
  :mark-collection-deleted
  :unmark-collection-deleted
  )

(defparameter *change-operations*
  '(  ;; operations on fields
    :update-field

    ;; operations on rows
    :add-row
    :mark-row-deleted
    :unmark-row-deleted

    ;; operations on columns
    :add-column
    :rename-column
    :mark-column-deleted
    :unmark-column-deleted

    ;; operations on lists
    :create-list
    :rename-list
    :update-list-notes
    :mark-list-deleted
    :unmark-list-deleted

    ;; operations on collections
    :create-collection
    :rename-collection
    :update-collection-notes
    :add-member
    :mark-member-deleted
    :unmark-member-deleted
    :mark-collection-deleted
    :unmark-collection-deleted
    ))

(defun change-message-p (thing)
  (and (consp thing)
       (symbolp (car thing))
       (member (car thing)
               *change-operations*)
       t))

(deftype change-message () '(satisfies change-message-p))

(defun validate-change-message (thing)
  (unless (typep thing 'change-message)
    (error "Not a change message: ~S" thing))
  t)

;;; ---------------------------------------------------------------------
;;; Fields
;;; ---------------------------------------------------------------------

(defun update-field (&key
                       (parent-state nil)
                       (list-id nil)
                       (row-id nil)
                       (column-label nil)
                       (new-value nil)
                       (old-value nil)
                       (timestamp nil))
  `(:UPDATE-FIELD :parent-state ,parent-state
                  :list-id ,list-id
                  :row-id ,row-id
                  :column-label ,column-label
                  :new-value ,new-value
                  :old-value ,old-value
                  :timestamp ,timestamp))

;;; ---------------------------------------------------------------------
;;; Rows
;;; ---------------------------------------------------------------------

(defun add-row (&key
                  (parent-state nil)
                  (list-id nil)
                  (timestamp nil))
  `(:ADD-ROW :parent-state ,parent-state
             :list-id ,list-id
             :timestamp ,timestamp))

(defun mark-row-deleted (&key
                           (parent-state nil)
                           (list-id nil)
                           (row-id nil)
                           (timestamp nil))
  `(:MARK-ROW-DELETED :parent-state ,parent-state
                      :list-id ,list-id
                      :row-id ,row-id
                      :timestamp ,timestamp))

(defun unmark-row-deleted (&key
                             (parent-state nil)
                             (list-id nil)
                             (row-id nil)
                             (timestamp nil))
  `(:UNMARK-ROW-DELETED :parent-state ,parent-state
                        :list-id ,list-id
                        :row-id ,row-id
                        :timestamp ,timestamp))

;;; ---------------------------------------------------------------------
;;; Columns
;;; ---------------------------------------------------------------------

(defun add-column (&key
                     (parent-state nil)
                     (list-id nil)
                     (column-label nil)
                     (timestamp nil))
  `(:ADD-COLUMN :parent-state ,parent-state
                :list-id ,list-id
                :column-label ,column-label
                :timestamp ,timestamp))

(defun rename-column (&key
                        (parent-state nil)
                        (list-id nil)
                        (new-column-label nil)
                        (old-column-label nil)
                        (timestamp nil))
  `(:RENAME-COLUMN :parent-state ,parent-state
                   :list-id ,list-id
                   :new-column-label ,new-column-label
                   :old-column-label ,old-column-label
                   :timestamp ,timestamp))

(defun mark-column-deleted (&key
                              (parent-state nil)
                              (list-id nil)
                              (column-label nil)
                              (timestamp nil))
  `(:MARK-COLUMN-DELETED :parent-state ,parent-state
                         :list-id ,list-id
                         :column-label ,column-label
                         :timestamp ,timestamp))

(defun unmark-column-deleted (&key
                                (parent-state nil)
                                (list-id nil)
                                (column-label nil)
                                (timestamp nil))
  `(:UNMARK-COLUMN-DELETED :parent-state ,parent-state
                           :list-id ,list-id
                           :column-label ,column-label
                           :timestamp ,timestamp))

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defun create-list (&key ; the parent-state when creating a list is always nil
                      (list-id nil) ; used when creating a copy of an exiting list
                      (timestamp nil))
  `(:CREATE-LIST :list-id ,list-id
                 :timestamp ,timestamp))

(defun rename-list (&key
                      (parent-state nil)
                      (list-id nil)
                      (new-title nil)
                      (old-title nil)
                      (timestamp nil))
  `(:RENAME-LIST :parent-state ,parent-state
                 :list-id ,list-id
                 :new-title ,new-title
                 :old-title ,old-title
                 :timestamp ,timestamp))

(defun update-list-notes (&key
                            (parent-state nil)
                            (list-id nil)
                            (new-notes nil)
                            (old-notes nil)
                            (timestamp nil))
  `(:UPDATE-LIST-NOTES :parent-state ,parent-state
                       :list-id ,list-id
                       :new-notes ,new-notes
                       :old-notes ,old-notes
                       :timestamp ,timestamp))

(defun mark-list-deleted (&key
                            (parent-state nil)
                            (list-id nil)
                            (timestamp nil))
  `(:MARK-LIST-DELETED :parent-state ,parent-state
                       :list-id ,list-id
                       :timestamp ,timestamp))

(defun unmark-list-deleted (&key
                              (parent-state nil)
                              (list-id nil)
                              (timestamp nil))
  `(:UNMARK-LIST-DELETED :parent-state ,parent-state
                         :list-id ,list-id
                         :timestamp ,timestamp))

;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

(defun create-collection (&key ; the parent-state when creating a collection is always nil
                            (collection-id nil) ; used when creating a copy of an exiting collection
                            (timestamp nil))
  `(:CREATE-COLLECTION :collection-id ,collection-id
                       :timestamp ,timestamp))

(defun rename-collection (&key
                            (parent-state nil)
                            (collection-id nil)
                            (new-title nil)
                            (old-title nil)
                            (timestamp nil))
  `(:RENAME-COLLECTION :parent-state ,parent-state
                       :collection-id ,collection-id
                       :new-title ,new-title
                       :old-title ,old-title
                       :timestamp ,timestamp))

(defun update-collection-notes (&key
                                  (parent-state nil)
                                  (collection-id nil)
                                  (new-notes nil)
                                  (old-notes nil)
                                  (timestamp nil))
  `(:UPDATE-COLLECTION-NOTES :parent-state ,parent-state
                             :collection-id ,collection-id
                             :new-notes ,new-notes
                             :old-notes ,old-notes
                             :timestamp ,timestamp))

(defun add-member (&key
                     (parent-state nil)
                     (collection-id nil)
                     (member-id nil) ; a known list or collection id
                     (timestamp nil))
  `(:ADD-MEMBER :parent-state ,parent-state
                :collection-id ,collection-id
                :member-id ,member-id
                :timestamp ,timestamp))

(defun mark-member-deleted (&key
                              (parent-state nil)
                              (collection-id nil)
                              (member-id nil)
                              (timestamp nil))
  `(:MARK-MEMBER-DELETED :parent-state ,parent-state
                         :collection-id ,collection-id
                         :member-id ,member-id
                         :timestamp ,timestamp))

(defun unmark-member-deleted (&key
                                (parent-state nil)
                                (collection-id nil)
                                (member-id nil)
                                (timestamp nil))
  `(:UNMARK-MEMBER-DELETED :parent-state ,parent-state
                           :collection-id ,collection-id
                           :member-id ,member-id
                           :timestamp ,timestamp))

(defun mark-collection-deleted (&key
                                  (parent-state nil)
                                  (collection-id nil)
                                  (timestamp nil))
  `(:MARK-COLLECTION-DELETED :parent-state ,parent-state
                             :collection-id ,collection-id
                             :timestamp ,timestamp))

(defun unmark-collection-deleted (&key
                                    (parent-state nil)
                                    (collection-id nil)
                                    (timestamp nil))
  `(:UNMARK-COLLECTION-DELETED :parent-state ,parent-state
                               :collection-id ,collection-id
                               :timestamp ,timestamp))

