;;;; package.lisp

(defpackage :engine
  (:use :cl :conspack))

(defpackage :message
  (:use :cl :conspack)
  (:export     ;; message fields
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
   ))

