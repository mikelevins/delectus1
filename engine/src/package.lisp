;;;; package.lisp

(defpackage :engine
  (:use :cl :conspack))

(defpackage :message
  (:nicknames :msg)
  (:use :cl :conspack)
  (:export

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
   ))


