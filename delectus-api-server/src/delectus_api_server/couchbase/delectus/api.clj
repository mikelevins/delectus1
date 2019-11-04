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
   ))

;;; ---------------------------------------------------------------------
;;; Users
;;; ---------------------------------------------------------------------

(defn register-user [email password])

;;; (time (login-user "greer@evins.net" delectus-api-server.core/$greerpw))
;;; (time (login-user "greer@evins.net" "wrong-password"))

(defn logout-user [auth-token])

(defn email->user-id [email]
  (users/delectus-user-email->id email))

;;; (email->user-id "mikel@evins.net")

(defn auth-token->user-id [auth-token])

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
;;; Collections
;;; ---------------------------------------------------------------------

(defn list-collections [user-id]
  (collections/delectus-collections user-id))

(defn create-collection [& {:keys [id name owner-id]
                            :or {id (makeid)
                                 name nil
                                 owner-id nil}}])

(defn mark-collection-deleted [collection-id deleted?])

(defn find-collection-by-id [user-id collection-id]
  (collections/find-collection-by-id user-id collection-id))

(defn find-collection-by-name [user-id collection-name]
  (collections/find-collection-by-name user-id collection-name))

(defn get-collection-name [user-id collection-id])

(defn update-collection-name [user-id collection-id new-name])

(defn get-collection-lists [user-id collection-id])

(defn add-collection-list [user-id collection-id list-id])

(defn remove-collection-list [user-id collection-id list-id])

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defn list-lists [user-id]
  (lists/delectus-lists user-id))

;;; (list-lists (email->user-id "mikel@evins.net"))

(defn create-list [user-id name])

(defn mark-list-deleted [user-id list-id])

(defn find-list-by-id [user-id list-id])

(defn find-list-by-name [user-id list-name]
  (lists/find-list-by-name user-id list-name))

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
