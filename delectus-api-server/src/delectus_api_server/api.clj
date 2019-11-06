(ns delectus-api-server.api
  (:require
   [buddy.hashers :as hashers]
   [delectus-api-server.identifiers :refer [makeid]]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.couchbase.delectus.collections :as collections]
   [delectus-api-server.couchbase.delectus.lists :as lists]
   [delectus-api-server.couchbase.delectus.users :as users]
   [delectus-api-server.couchbase.io :as couch-io]
   [delectus-api-server.couchbase.marshal :as marshal]
   [delectus-api-server.utilities :refer [fmt]])
  (:import
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; Users
;;; ---------------------------------------------------------------------

(defn register-user [email password])

;;; PRIVATE: do not expose to the public API
(defn update-user! [userid new-values-map])

(defn user [email]
  (let [bucket (config/delectus-users-bucket)
        bucket-name (.name bucket)
        selector (str (fmt "SELECT * from `~A` " bucket-name)
                      (fmt "WHERE `type` = \"delectus_user\" ")
                      (fmt "AND `email` = \"~A\"" email))
        results (.query bucket (N1qlQuery/simple selector))
        objs (map #(.get (.value %) bucket-name) results)
        found-user (if (empty? objs) nil (first objs))]
    (or found-user nil)))

;;; (user "mikel@evins.net")
;;; (user "greer@evins.net")
;;; (user "nobody@nowhere.net")

(defn userid [email]
  (let [found-user (user email)]
    (if found-user
      (.get found-user "id")
      nil)))

;;; (userid "mikel@evins.net")
;;; (userid "greer@evins.net")
;;; (userid "nobody@nowhere.net")

;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

(defn list-collections [userid]
  (collections/delectus-collections userid))

(defn create-collection [& {:keys [id name owner-id]
                            :or {id (makeid)
                                 name nil
                                 owner-id nil}}])

(defn mark-collection-deleted [collection-id deleted?])

(defn find-collection-by-id [userid collection-id]
  (collections/find-collection-by-id userid collection-id))

(defn find-collection-by-name [userid collection-name]
  (collections/find-collection-by-name userid collection-name))

(defn get-collection-name [userid collection-id])

(defn update-collection-name [userid collection-id new-name])

(defn get-collection-lists [userid collection-id])

(defn add-collection-list [userid collection-id list-id])

(defn remove-collection-list [userid collection-id list-id])

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defn list-lists [userid]
  (lists/delectus-lists userid))

;;; (list-lists (email->userid "mikel@evins.net"))

(defn create-list [userid name])

(defn mark-list-deleted [userid list-id])

(defn find-list-by-id [userid list-id]
  (lists/find-list-by-id userid list-id))

(defn find-list-by-name [userid list-name]
  (lists/find-list-by-name userid list-name))

(defn list-name [userid list-id])

(defn update-list-name [userid list-id new-name])

(defn list-columns [userid list-id])

(defn find-column-by-id [userid list-id column-id])

(defn find-column-by-name [userid list-id column-name])

(defn list-add-column [userid list-id column-name])

(defn mark-column-deleted [userid list-id column-id])

(defn column-name [userid list-id column-id])

(defn update-column-name [userid list-id column-id new-name])

(defn list-items [userid list-id])

(defn find-item-by-id [userid list-id item-id])

(defn list-add-item [userid list-id])

(defn mark-item-deleted [userid list-id deleted?])

(defn item-column-value [userid list-id column-id])

(defn update-item-column-value [userid list-id column-id new-value])
