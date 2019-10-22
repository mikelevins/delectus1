(ns delectus-api-server.couchbase.delectus.users
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.utilities :refer [uuid valid-email?]]
            [delectus-api-server.couchbase.utilities :refer [for-couchbase map->JsonObject]]))

(defrecord delectus-user [type id primary-email email-addresses password-hash collections lists])

(defn make-user [& {:keys [type id primary-email email-addresses password-hash collections lists]
                    :or {type nil
                         id (makeid)
                         primary-email nil
                         email-addresses []
                         password-hash nil
                         collections {}
                         lists {}}}]
  (when (not primary-email)
    (throw (ex-info ":primary-email parameter missing" {})))
  (when (not (valid-email? primary-email))
    (throw (ex-info "invalid :primary-email parameter" {:value primary-email})))
  (map->delectus-user
   {:type "delectus_user"
    :id id
    :primary-email primary-email
    :email-addresses [primary-email]
    :password-hash password-hash
    :collections collections
    :lists lists}))

;;; (make-user)
;;; (def $mikel-id (makeid))
;;; (def $mikel (make-user :id $mikel-id :primary-email "mikel@evins.net"))
