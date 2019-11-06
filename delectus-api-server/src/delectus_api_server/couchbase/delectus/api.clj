(ns delectus-api-server.couchbase.delectus.api
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

;;; (time (login-user "greer@evins.net" delectus-api-server.core/$greerpw))
;;; (time (login-user "greer@evins.net" "wrong-password"))

(defn logout-user [auth-token])

(defn email->userid [email]
  (users/delectus-user-email->id email))

;;; (email->userid "mikel@evins.net")

(defn auth-token->userid [auth-token])

;;; PRIVATE: do not expose to the public API
(defn update-user! [userid new-values-map]
  (let [old-user-map (marshal/to-map (couch-io/get-object (config/delectus-users-bucket) userid))]
    (couch-io/update-document! (config/delectus-users-bucket)
                               userid
                               (merge old-user-map
                                      new-values-map))
    userid))

;;; (def $granny-pw nil)
;;; (def $greer-pw nil)
;;; (def $mikel-pw nil)
;;; (def $granny-hash (hashers/derive $granny-pw))
;;; (hashers/check $granny-pw $granny-hash)
;;; (def $greer-hash (hashers/derive $greer-pw))
;;; (hashers/check $greer-pw $greer-hash)
;;; (def $mikel-hash (hashers/derive $mikel-pw))
;;; (hashers/check $mikel-pw $mikel-hash)
;;; (hashers/check $mikel-pw nil)
;;; (def $greerid (users/delectus-user-email->id "greer@evins.net"))
;;; (update-user! $greerid {:name "Greer Evins" :password-hash $greer-hash})
;;; (def $grannyid (users/delectus-user-email->id "granny@evins.net"))
;;; (update-user! $grannyid {:name "Sally Schuster" :password-hash $granny-hash})
;;; (def $mikelid (users/delectus-user-email->id "mikel@evins.net"))
;;; (update-user! $mikelid {:name "mikel evins" :password-hash $mikel-hash})

;;; ---------------------------------------------------------------------
;;; users
;;; ---------------------------------------------------------------------

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
