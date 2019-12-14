(ns delectus-api.api
  (:require
   [compojure.api.sweet :refer :all]
   [delectus-api.auth :as auth]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.errors :as errors]
   [delectus-api.identifiers :refer [makeid]]
   [delectus-api.model :as model]
   [delectus-api.schema :as schema]
   [ring.handler.dump :refer [handle-dump]]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
   [tick.alpha.api :as t]
   )
  (:import
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

(defn authenticate [userid password]
  (auth/authenticate-user userid password))

(defn login [userid password]
  (auth/login-user userid password))

(defn userid [email]
  (couchio/email->userid email))

;;; (userid "mikel@evins.net")
;;; (userid "doo@evins.net")

(defn userdata [userid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      {:userid userid
       :name (.get found-user +name-attribute+)
       :email (.get found-user +email-attribute+)}
      nil)))
