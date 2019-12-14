(ns delectus-api.handlers
  (:require
   [compojure.api.sweet :refer :all]
   [delectus-api.auth :as auth]
   [delectus-api.api :as api]
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

;;; /api/user

(defn authenticate [userid password]
  (let [maybe-auth (api/authenticate userid password)]
    (if maybe-auth
      (ok {:token (auth/make-auth-token maybe-auth)})
      (unauthorized "Authentication failed"))))

(defn login [email password]
  (let [maybe-auth (api/login email password)]
    (if maybe-auth
      (ok {:token (auth/make-auth-token maybe-auth)})
      (unauthorized "Login failed"))))

(defn userid [email]
  (let [found-id (api/userid email)]
    (if found-id
      (ok found-id)
      (not-found "No such user"))))

(defn userdata [userid]
  (try
    (let [found-data (api/userdata userid)]
      (if found-data
        (ok found-data)
        (not-found "No such user")))
    (catch clojure.lang.ExceptionInfo ex
      (not-found (.getMessage ex)))))

;;; /api/collection

(defn collections [userid]
  (try
    (let [collections (api/collections userid)]
      (ok collections))
    (catch clojure.lang.ExceptionInfo ex
      (not-found (.getMessage ex)))))

(defn collection-with-id [userid collectionid]
  (let [collection (api/collection-with-id userid collectionid)]
    (if collection
      (ok collection)
      (not-found "No such collection"))))

(defn collection-name [userid collectionid]
  (let [name (api/collection-name userid collectionid)]
    (if name
      (ok name)
      (not-found "No such collection"))))

(defn collection-named [userid name]
  (let [collection (api/collection-named userid name)]
    (if collection
      (ok collection)
      (not-found "No such collection"))))

(defn rename-collection [userid collectionid newname]
  (let [collectionid (api/rename-collection userid collectionid newname)]
    (if (nil? collectionid)
      (not-found "No such collection")
      (ok newname))))

(defn new-collection [userid name]
  (try (let [newid (api/new-collection userid name)]
         (ok newid))
       (catch clojure.lang.ExceptionInfo ex
         (conflict "Name exists"))))

;;; /api/list


(defn lists [userid]
  (let [lists (api/lists userid)]
    (ok lists)))
