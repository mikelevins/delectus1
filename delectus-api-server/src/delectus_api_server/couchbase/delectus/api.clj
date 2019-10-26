(ns delectus-api-server.couchbase.delectus.api
  (:require 
   [delectus-api-server.identifiers :refer [makeid]]))

;;; ---------------------------------------------------------------------
;;; Users
;;; ---------------------------------------------------------------------

(defn register-user [email password])

(defn login-user [email password])

(defn logout-user [session-id])

(defn email->user-id [email])

(defn session-id->user-id [session-id])

;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

(defn list-collections [user-id])

(defn create-collection [& {:keys [id name owner-id]
                            :or {id (makeid)
                                 name nil
                                 owner-id nil}}])

(defn mark-collection-deleted [collection-id deleted?])

(defn find-collection-by-id [user-id collection-id])

(defn find-collection-by-name [user-id collection-id])

(defn get-collection-name [user-id collection-id])

(defn update-collection-name [user-id collection-id new-name])

(defn get-collection-lists [user-id collection-id])

(defn add-collection-list [user-id collection-id list-id])

(defn remove-collection-list [user-id collection-id list-id])

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defn list-lists [user-id])

(defn create-list [user-id name])

(defn mark-list-deleted [user-id list-id])

(defn find-list-by-id [user-id list-id])

(defn find-list-by-name [user-id list-name])

(defn list-name [user-id list-id])

(defn update-list-name [user-id list-id new-name])

(defn list-columns [user-id list-id])

(defn find-column-by-id [user-id list-id column-id])

(defn find-column-by-name [user-id list-id column-name])

(defn list-add-column [user-id list-id column-name])

(defn mark-column-deleted [user-id list-id column-id])

(defn column-name [user-id list-id column-id])

(defn update-column-name [user-id list-id column-id new-name])

(defn list-items [user-id list-id])

(defn find-item-by-id [user-id list-id item-id])

(defn list-add-item [user-id list-id])

(defn mark-item-deleted [user-id list-id deleted?])

(defn item-column-value [user-id list-id column-id])

(defn update-item-column-value [user-id list-id column-id new-value])
