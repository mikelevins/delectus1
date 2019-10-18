(ns delectus-api-server.couchbase.delectus.users
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [delectus-api-server.utilities :refer [uuid]]
            [delectus-api-server.couchbase.utilities :refer [map->JsonObject]]))

(defn make-user-account [username & {:keys [id password-hash collections lists]
                                     :or {id (str (uuid))
                                          password-hash nil
                                          collections nil
                                          lists nil}}]
  (if username
    {"type" "delectus-user"
     "username" username
     "id" id
     "password-hash" password-hash
     "collections" collections
     "lists" lists}
    (throw (ex-info "username missing" {:username username}))))

;;; (uuid)
;;; (make-user-account)
;;; (make-user-account "mikel")
;;; (map->JsonObject (make-user-account "mikel"))

