(ns delectus-api.handlers
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

(defn login [email password]
  (let [maybe-auth (auth/authenticate-user email password)]
    (if maybe-auth
      (ok {:token (auth/make-auth-token maybe-auth)})
      (unauthorized "Login failed"))))

(defn userid [email]
  (let [found-user (couchio/email->user email)]
    (if found-user
      (ok (.get found-user +id-attribute+))
      (not-found "No such user"))))

(defn userdata [userid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (ok {:id userid
           :name (.get found-user +name-attribute+)
           :email (.get found-user +email-attribute+)})
      (not-found "No such user"))))

(defn collections [email]
  (let [userid (couchio/email->userid email)]
    (if userid
      (let [collections (couchio/find-objects (config/delectus-content-bucket) []
                                              {"type" +collection-type+ "owner-id" userid})]
        (if (empty? collections)
          (ok [])
          (let [collection-maps (map #(.toMap %) collections)
                descriptions (map #(select-keys % ["name" "id"]) collection-maps)]
            (ok descriptions))))
      (not-found "No such user"))))

(defn collection-with-id [email id]
  (let [userid (couchio/email->userid email)]
    (if userid
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "id" id})]
        (if (empty? collections)
          (not-found "No such collection")
          (let [collection (first collections)
                collection-map {"name" (.get collection +name-attribute+)
                                "id" (.get collection +id-attribute+)}]
            (ok collection-map))))
      (not-found "No such user"))))

(defn collection-name [email id]
  (let [userid (couchio/email->userid email)]
    (if userid
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "id" id})]
        (if (empty? collections)
          (not-found "No such collection")
          (let [collection (first collections)
                name (.get collection +name-attribute+)]
            (ok name))))
      (not-found "No such user"))))

(defn collection-named [email name]
  (let [userid (couchio/email->userid email)]
    (if userid
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "name" name})]
        (if (empty? collections)
          (not-found "No such collection")
          (let [collection (first collections)
                collection-map {"name" (.get collection +name-attribute+)
                                "id" (.get collection +id-attribute+)}]
            (ok collection-map))))
      (not-found "No such user"))))

(defn rename-collection [email collectionid newname]
  (let [userid (couchio/email->userid email)]
    (if userid
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "id" collectionid})]
        (if (empty? collections)
          (not-found "No such collection")
          (let [content-bucket (config/delectus-content-bucket)
                mutator (.mutateIn content-bucket collectionid)
                updater (.upsert mutator +name-attribute+ newname)]
            (.execute updater)
            (ok newname))))
      (not-found "No such user"))))

(defn new-collection [email name]
  (let [userid (couchio/email->userid email)]
    (if userid
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "name" name})]
        (if (empty? collections)
          (let [id (makeid)
                collection-doc (model/make-collection-document :id id :name name :owner-id userid)]
            (.upsert (config/delectus-content-bucket)
                     collection-doc)
            (ok id))
          (conflict "Name exists")))
      (not-found "No such user"))))
