(ns delectus-api-server.couchbase.delectus.users
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.utilities :refer [uuid valid-email?]]
            [delectus-api-server.couchbase.io :as couch-io]
            [delectus-api-server.couchbase.marshal
             :refer [Couchable JsonDocumentable JsonObjectable
                     make-couchable to-json-document to-json-object to-map]])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)))

;;; ---------------------------------------------------------------------
;;; User
;;; ---------------------------------------------------------------------

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
              :type "delectus_user"
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
;;; the user index
;;; ---------------------------------------------------------------------
;;; the Couchbase document that maps user email addresses to User ids

(defn the-user-index-document-id [] "the_user_index")

;;; (couch-io/get-document (config/delectus-bucket) (the-user-index-document-id))

(defn get-user-index [bucket]
  (couch-io/get-document bucket (the-user-index-document-id)))

;;; (get-user-index (config/delectus-bucket))

(defn initialize-user-index [bucket & {:keys [replace]}]
  (let [index (get-user-index bucket)]
    (when (and index (not replace))
      (throw (ex-info "The user-index document already exists" {:bucket (.name bucket)})))
    (when index
      (couch-io/delete-document! bucket (the-user-index-document-id)))
    (couch-io/create-document! bucket (the-user-index-document-id) {})
    (the-user-index-document-id)))

;;; (initialize-user-index (config/delectus-bucket))

(defn add-delectus-user! [bucket email-address & {:keys [id email-addresses password-hash collections lists]
                                                  :or {id (makeid)
                                                       email-addresses []
                                                       password-hash nil
                                                       collections {}
                                                       lists {}}}]
  (let [already-user-document (couch-io/get-document bucket id)]
    (if already-user-document
      (throw (ex-info "A user with the supplied ID already exists" {:id id}))
      (let [index-document (get-user-index bucket)]
        (when (not index-document)
          (initialize-user-index bucket))
        (let [index-document (get-user-index bucket)]
          (when (not index-document)
            (throw (ex-info "There was a problem initializing the user-account index" {:bucket (.name bucket)})))
          (let [index-map (to-map index-document)
                email-addresses [email-address]
                new-user-map (make-user :id id :primary-email email-address
                                        :email-addresses email-addresses :password-hash password-hash
                                        :collections collections :lists lists)
                new-index-map (merge index-map {email-address id})
                new-index-document (to-json-document new-index-map (.id index-document))
                new-user-document (to-json-document new-user-map id)]
            (.insert bucket new-user-document)
            (.upsert bucket new-index-document)))))))

;;; (add-delectus-user! (config/delectus-bucket) "mikel@evins.net")
;;; (add-delectus-user! (config/delectus-bucket) "greer@evins.net")
