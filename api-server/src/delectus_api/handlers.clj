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

;;; ---------------------------------------------------------------------
;;; error handlers
;;; ---------------------------------------------------------------------

(def +exception-handlers+
  {
   :authentication-failed unauthorized
   :collection-name-exists conflict
   :collection-not-found not-found
   :login-failed unauthorized
   :user-not-found not-found
   })

(defn handle-exception [ex]
  (let [msg (.getMessage ex)
        data (ex-data ex)
        cause (:cause data)
        handler (get +exception-handlers+ cause internal-server-error)]
    (let [info (str msg ". " data)]
      (handler info))))

;;; ---------------------------------------------------------------------
;;; endpoint handlers
;;; ---------------------------------------------------------------------

;;; /api/user

(defn authenticate [userid password]
  (try
    (let [maybe-auth (api/authenticate userid password)]
      (if maybe-auth
        (ok {:token (auth/make-auth-token maybe-auth)})
        (unauthorized "Authentication failed")))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

(defn login [email password]
  (try
    (let [maybe-auth (api/login email password)]
      (if maybe-auth
        (ok {:token (auth/make-auth-token maybe-auth)})
        (unauthorized "Login failed")))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

(defn userid [email]
  (try
    (let [found-id (api/userid email)]
      (if found-id
        (ok found-id)
        (not-found "No such user")))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

(defn userdata [userid]
  (try
    (let [found-data (api/userdata userid)]
      (if found-data
        (ok found-data)
        (not-found "No such user")))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

;;; /api/collection

(defn collections [userid]
  (try
    (let [collections (api/collections userid)]
      (ok collections))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

(defn collection-with-id [userid collectionid]
  (try
    (let [collection (api/collection-with-id userid collectionid)]
      (ok collection))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

(defn collection-name [userid collectionid]
  (try
    (let [name (api/collection-name userid collectionid)]
      (ok name))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

(defn collection-named [userid name]
  (try
    (let [collection (api/collection-named userid name)]
      (ok collection))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

(defn rename-collection [userid collectionid newname]
  (try
    (let [collectionid (api/rename-collection userid collectionid newname)]
      (if (nil? collectionid)
        (not-found "No such collection")
        (ok newname)))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

(defn new-collection [userid name]
  (try
    (let [newid (api/new-collection userid name)]
      (ok newid))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

(defn delete-collection [userid collectionid]
  (try
    (api/delete-collection userid collectionid)
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

(defn undelete-collection [userid collectionid]
  (try
    (api/undelete-collection userid collectionid)
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))

;;; /api/list


(defn lists [userid]
  (try
    (let [lists (api/lists userid)]
      (ok lists))
    (catch clojure.lang.ExceptionInfo ex
      (handle-exception ex))))
