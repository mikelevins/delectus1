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

;;; TODO: add error checking for nonexistent users and other DB objects

;;; users
;;; ---------------------------------------------------------------------

(defn authenticate [userid password]
  (or (auth/authenticate-user userid password)
      (throw (ex-info "Authentication failed"
                      {:cause :authentication-failed
                       :userid userid}))))

(defn login [email password]
  (or (auth/login-user email password)
      (throw (ex-info "Login failed"
                      {:cause :login-failed
                       :email email}))))

(defn userid [email]
  (or (couchio/email->userid email)
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :email email}))))

;;; (userid "mikel@evins.net")
;;; (userid "doo@evins.net")

(defn userdata [userid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      {:userid userid
       :name (.get found-user +name-attribute+)
       :email (.get found-user +email-attribute+)}
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid})))))

;;; collections
;;; ---------------------------------------------------------------------

(defn collections [userid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (map #(select-keys (.toMap %) ["name" "id" "deleted"])
           (couchio/find-objects (config/delectus-content-bucket) []
                                 {"type" +collection-type+ "owner-id" userid}))
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid})))))

;;; (collections "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (collections "6235e7b7-eb83-47d9-a8ef-ac129601e810")
;;; (collections "BOGUS")

(defn collection-with-id [userid collectionid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "id" collectionid})]
        (if (empty? collections)
          (throw (ex-info "No such collection"
                          {:cause :collection-not-found
                           :userid userid :collectionid collectionid}))
          (let [collection (first collections)]
            {"name" (.get collection +name-attribute+)
             "id" (.get collection +id-attribute+)})))
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid :collectionid collectionid})))))

(defn collection-name [userid collectionid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "id" collectionid})]
        (if (empty? collections)
          (throw (ex-info "No such collection"
                          {:cause :collection-not-found
                           :userid userid :collectionid collectionid}))
          (let [collection (first collections)]
            (.get collection +name-attribute+))))
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid :collectionid collectionid})))))


(defn collection-named [userid name]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "name" name})]
        (if (empty? collections)
          (throw (ex-info "No such collection"
                          {:cause :collection-not-found
                           :userid userid}))
          (let [collection (first collections)]
            {"name" (.get collection +name-attribute+)
             "id" (.get collection +id-attribute+)})))
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid})))))

(defn rename-collection [userid collectionid newname]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "id" collectionid})]
        (if (empty? collections)
          (throw (ex-info "No such collection"
                          {:cause :collection-not-found
                           :userid userid :collectionid collectionid}))
          (try
            (let [content-bucket (config/delectus-content-bucket)
                  mutator (.mutateIn content-bucket collectionid)
                  updater (.upsert mutator +name-attribute+ newname)]
              (.execute updater)
              newname)
            (catch Exception ex
              (throw (ex-info "Couchbase Error"
                              {:cause :couchbase-exception
                               :exception-object ex
                               :userid userid :collectionid collectionid}))))))
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid :collectionid collectionid})))))

(defn new-collection [userid name]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "name" name})]
        (if (empty? collections)
          (let [id (makeid)
                collection-doc (model/make-collection-document :id id :name name :owner-id userid)]
            (.upsert (config/delectus-content-bucket)
                     collection-doc)
            id)
          (throw (ex-info "Name exists"
                          {:cause :collection-name-exists
                           :collectionname name
                           :userid userid}))))
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid})))))

(defn delete-collection [userid collectionid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "id" collectionid})]
        (if (empty? collections)
          (throw (ex-info "No such collection"
                          {:cause :collection-not-found
                           :userid userid :collectionid collectionid}))
          (let [content-bucket (config/delectus-content-bucket)
                mutator (.mutateIn content-bucket collectionid)
                updater (.upsert mutator +deleted-attribute+ true)]
            (.execute updater)
            collectionid)))
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid :collectionid collectionid})))))

(defn undelete-collection [userid collectionid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "id" collectionid})]
        (if (empty? collections)
          (throw (ex-info "No such collection"
                          {:cause :collection-not-found
                           :userid userid :collectionid collectionid}))
          (let [content-bucket (config/delectus-content-bucket)
                mutator (.mutateIn content-bucket collectionid)
                updater (.upsert mutator +deleted-attribute+ false)]
            (.execute updater)
            collectionid)))
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid :collectionid collectionid})))))

(defn collection-deleted? [userid collectionid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (let [collections (couchio/find-objects
                         (config/delectus-content-bucket) []
                         {"type" +collection-type+ "owner-id" userid "id" collectionid})]
        (if (empty? collections)
          (throw (ex-info "No such collection"
                          {:cause :collection-not-found
                           :userid userid :collectionid collectionid}))
          (let [collection (first collections)]
            (.get collection +deleted-attribute+))))
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid :collectionid collectionid})))))

;;; lists
;;; ---------------------------------------------------------------------

(defn lists [userid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (map #(select-keys (.toMap %) ["name" "id"])
           (couchio/find-objects (config/delectus-content-bucket) []
                                 {"type" +list-type+ "owner-id" userid}))
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :userid userid})))))



