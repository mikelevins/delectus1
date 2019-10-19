(ns delectus-api-server.couchbase.delectus.users
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [delectus-api-server.utilities :refer [uuid]]))


(defn ->userid [username]
  (str "user::" username))

(defn make-user-account [username & {:keys [id password-hash collections lists]
                                     :or {id (->userid username)
                                          password-hash nil
                                          collections {}
                                          lists {}}}]
  (if username
    {"type" "user"
     "username" username
     "password-hash" password-hash
     "collections" collections
     "lists" lists}
    (throw (ex-info "username missing" {:username username}))))

;;; (uuid)
;;; (make-user-account)
;;; (make-user-account "mikel")
;;; (hash (make-user-account "mikel"))
