;;;; package.lisp

(defpackage :engine
  (:use :cl :conspack))

(defpackage :message
  (:use :cl :conspack)
  (:export

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
   ))


