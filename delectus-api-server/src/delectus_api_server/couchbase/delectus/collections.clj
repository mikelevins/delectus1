(ns delectus-api-server.couchbase.delectus.collections
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.identifiers :refer [makeid]]
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

;; (defn user-roles [] ["user"])

;; (defn the-user-document-type [] "delectus_user")

;; (defrecord User [id type primary-email email-addresses password-hash roles]
;;   Couchable
;;   (make-couchable [data]
;;     (let [ks (map make-couchable (keys data))
;;           vs (map make-couchable (vals data))]
;;       (java.util.HashMap. (zipmap ks vs))))
;;   JsonObjectable
;;   (to-json-object [data] (JsonObject/from (make-couchable data)))
;;   JsonDocumentable
;;   (to-json-document [data id] (JsonDocument/create id (to-json-object data))))

;; (defn make-user [& {:keys [id primary-email email-addresses password-hash roles]
;;                     :or {id (makeid)
;;                          primary-email nil
;;                          email-addresses []
;;                          password-hash nil
;;                          roles ["user"]}}]
;;   (when (not primary-email)
;;     (throw (ex-info ":primary-email parameter missing" {})))
;;   (when (not (valid-email? primary-email))
;;     (throw (ex-info "invalid :primary-email parameter" {:value primary-email})))
;;   (map->User {:id id
;;               :type (the-user-document-type)
;;               :primary-email primary-email
;;               :email-addresses [primary-email]
;;               :password-hash password-hash
;;               :roles roles}))

;;; (def $mikel-id (makeid))
;;; (def $mikel (make-user :id $mikel-id :primary-email "mikel@evins.net"))
;;; (make-couchable $mikel)
;;; (to-json-object $mikel)
;;; (to-json-document $mikel $mikel-id)
