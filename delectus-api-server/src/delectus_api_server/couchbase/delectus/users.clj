(ns delectus-api-server.couchbase.delectus.users
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.utilities :refer [uuid valid-email?]]))

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
  {:type "delectus_user"
   :id id
   :primary-email primary-email
   :email-addresses [primary-email]
   :password-hash password-hash
   :collections collections
   :lists lists})

;;; (def $mikel-id (makeid))
;;; (def $mikel (make-user :id $mikel-id :primary-email "mikel@evins.net"))
