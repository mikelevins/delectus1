(ns delectus-api-server.couchbase.delectus.users
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.utilities :refer [uuid valid-email?]]
            [delectus-api-server.couchbase.io :as couch-io]
            [delectus-api-server.couchbase.marshal
             :refer [Couchable JsonDocumentable JsonObjectable
                     make-couchable to-json-document to-json-object to-map]])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; User
;;; ---------------------------------------------------------------------

(defn the-user-document-type [] "delectus_user")

(defrecord User [id type primary-email email-addresses password-hash collections lists]
  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs))))
  JsonObjectable
  (to-json-object [data] (JsonObject/from (make-couchable data)))
  JsonDocumentable
  (to-json-document [data id] (JsonDocument/create id (to-json-object data))))

(defn make-user [& {:keys [id primary-email email-addresses password-hash collections lists]
                    :or {id (makeid)
                         primary-email nil
                         email-addresses []
                         password-hash nil
                         collections {}
                         lists {}}}]
  (when (not primary-email)
    (throw (ex-info ":primary-email parameter missing" {})))
  (when (not (valid-email? primary-email))
    (throw (ex-info "invalid :primary-email parameter" {:value primary-email})))
  (map->User {:id id
              :type (the-user-document-type)
              :primary-email primary-email
              :email-addresses [primary-email]
              :password-hash password-hash
              :collections collections
              :lists lists}))

;;; (def $mikel-id (makeid))
;;; (def $mikel (make-user :id $mikel-id :primary-email "mikel@evins.net"))
;;; (make-couchable $mikel)
;;; (to-json-object $mikel)
;;; (to-json-document $mikel $mikel-id)

;;; ---------------------------------------------------------------------
;;; creating user records
;;; ---------------------------------------------------------------------
;;; the Couchbase document that maps user email addresses to User ids

(defn list-delectus-users []
  (let [bucket (config/delectus-users-bucket)
        bucket-name (.name bucket)
        select-expression (cl-format nil "SELECT `primary-email`,`id` from `~A` WHERE type = \"delectus_user\""
                                     bucket-name)
        results (.query bucket (N1qlQuery/simple select-expression))
        result-vals (map #(.value %) results)]
    (map to-map result-vals)))

;;; (time (list-delectus-users))

(defn add-delectus-user! [email-address & {:keys [id email-addresses password-hash collections lists]
                                           :or {id (makeid)
                                                email-addresses []
                                                password-hash nil
                                                collections {}
                                                lists {}}}]
  (let [bucket (config/delectus-users-bucket)
        already-user-document (couch-io/get-document bucket id)]
    (if already-user-document
      (throw (ex-info "A user with the supplied ID already exists" {:id id :bucket (.name bucket)}))
      (let [email-addresses [email-address]
            new-user-map (make-user :id id
                                    :primary-email email-address
                                    :email-addresses email-addresses :password-hash password-hash
                                    :collections collections :lists lists)
            new-user-document (to-json-document new-user-map id)]
        (.insert bucket new-user-document)))))

;;; (add-delectus-user! "mikel@evins.net")
;;; (add-delectus-user! "greer@evins.net")
