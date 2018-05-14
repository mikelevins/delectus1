;;;; store.lisp

(in-package #:engine)

(defclass delectus-store ()())

;;; ---------------------------------------------------------------------
;;; change protocol
;;; ---------------------------------------------------------------------
;;; all changes to the datastore are effected through this protocol
;;; executing each of these operations changes the state of the
;;; datastore, computes and stores a new state token in the datastore,
;;; marks the added state token as the current state token, and stores
;;; a record of the operation associated with the new state. Each
;;; operation that updates a stored value records both the old value
;;; and the new value in the change log.

(defmethod create-collection ((store delectus-store)))
(defmethod mark-collection-deleted ((store delectus-store)))
(defmethod destroy-collection ((store delectus-store)))
(defmethod add-list-to-collection ((store delectus-store) collection-id list-id))
(defmethod remove-list-from-collection ((store delectus-store) collection-id list-id))
(defmethod create-list ((store delectus-store)))
(defmethod mark-list-deleted ((store delectus-store) list-id))
(defmethod destroy-list ((store delectus-store) list-id))
(defmethod add-column-to-list ((store delectus-store) list-id column-label))
(defmethod mark-column-deleted ((store delectus-store) list-id column-label))
(defmethod destroy-column ((store delectus-store) list-id column-label))
(defmethod add-row-to-list ((store delectus-store) list-id &key row-elements))
(defmethod mark-row-deleted ((store delectus-store) list-id row-id))
(defmethod delete-row ((store delectus-store) list-id row-id))
(defmethod update-field ((store delectus-store) list-id row-id field-column-label))

