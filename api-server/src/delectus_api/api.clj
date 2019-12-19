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
   [delectus-api.utilities :as util]
   [ring.handler.dump :refer [handle-dump]]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
   [tick.alpha.api :as t]
   )
  (:import
   (com.couchbase.client.core CouchbaseException)
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))


;;; ---------------------------------------------------------------------
;;; ensuring that users, collections, and lists exist and are of the right type
;;; ---------------------------------------------------------------------

(defmacro ensure-user-exists [userid]
  `(if (model/user-exists? ~userid)
     ~userid
     (throw (ex-info "No such user"
                     {:cause :user-not-found
                      :userid ~userid}))))

(defmacro ensure-user [userid]
  (let [found-user (gensym)]
    `(let [~found-user (model/get-user ~userid)]
       (or ~found-user
           (throw (ex-info "No such user"
                           {:cause :user-not-found
                            :userid ~userid}))))))

(defmacro ensure-collection-exists [collectionid]
  `(if (model/collection-exists? ~collectionid)
     ~collectionid
     (throw (ex-info "No such collection"
                     {:cause :collection-not-found
                      :collectionid ~collectionid}))))

(defmacro ensure-collection [collectionid]
  (let [found-collection (gensym)]
    `(let [~found-collection (model/get-collection ~collectionid)]
       (or ~found-collection
           (throw (ex-info "No such collection"
                           {:cause :collection-not-found
                            :collectionid ~collectionid}))))))

(defmacro ensure-list-exists [listid]
  `(if (model/list-exists? ~listid)
     ~listid
     (throw (ex-info "No such list"
                     {:cause :list-not-found
                      :listid ~listid}))))

(defmacro ensure-list [listid]
  (let [found-list (gensym)]
    `(let [~found-list (model/get-list ~listid)]
       (or ~found-list
           (throw (ex-info "No such list"
                           {:cause :list-not-found
                            :listid ~listid}))))))

;;; ---------------------------------------------------------------------
;;; ensuring that objects belong to the expected owner
;;; ---------------------------------------------------------------------

(defmacro ensure-owner [objectid userid]
  (let [found-owner (gensym)]
    `(let [~found-owner (couchio/get-object-attribute (config/delectus-content-bucket)
                                                      ~objectid +owner-id-attribute+)]
       (if (and ~found-owner
                (= ~found-owner ~userid))
         ~found-owner
         (throw (ex-info "Wrong owner ID"
                         {:cause :wrong-owner-id
                          :expected ~userid
                          :found ~found-owner}))))))


;;; ---------------------------------------------------------------------
;;; api functions
;;; ---------------------------------------------------------------------

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
  (or (model/email->userid email)
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :email email}))))

(defn userdata [userid]
  (into {} (.toMap (ensure-user userid))))

;;; (userdata $mikelid)

;;; collections
;;; ---------------------------------------------------------------------

(defn collections [userid]
  (ensure-user-exists userid)
  (map #(select-keys (.toMap %) ["name" "id" "deleted" "type"])
       (couchio/find-objects (config/delectus-content-bucket) []
                             {"type" +collection-type+ "owner-id" userid})))

(defn collection-with-id [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (into {} (.toMap (ensure-collection collectionid))))

(defn collection-name [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                collectionid +name-attribute+))

(defn find-collection-with-name [userid name]
  (ensure-user-exists userid)
  (map #(.get % +id-attribute+)
       (couchio/find-objects
        (config/delectus-content-bucket) []
        {"type" +collection-type+ "owner-id" userid "name" name})))

(defn rename-collection [userid collectionid newname]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (let [collections (couchio/find-objects
                     (config/delectus-content-bucket) []
                     {"type" +collection-type+ "owner-id" userid "name" newname})]
    (if (empty? collections)
      (let [collection (ensure-collection collectionid)]
        (couchio/with-couchbase-exceptions-rethrown
          (couchio/upsert-object-attribute! (config/delectus-content-bucket) collectionid +name-attribute+ newname)))
      (throw (ex-info "Name exists"
                      {:cause :collection-name-exists
                       :collectionname newname
                       :userid userid})))))

(defn new-collection [userid name]
  (ensure-user-exists userid)
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
                       :userid userid})))))

(defn delete-collection [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    collectionid
                                    +deleted-attribute+
                                    true))

(defn undelete-collection [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    collectionid
                                    +deleted-attribute+
                                    false))

(defn collection-deleted? [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                collectionid
                                +deleted-attribute+))

(defn collection-lists [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (map #(.get % +id-attribute+)
       (couchio/find-objects (config/delectus-content-bucket) []
                             {"type" +list-type+
                              "owner-id" userid
                              "collection" collectionid})))

;;; lists
;;; ---------------------------------------------------------------------

(defn lists [userid]
  (ensure-user-exists userid)
  (map #(.get % +id-attribute+)
       (couchio/find-objects (config/delectus-content-bucket) []
                             {"type" +list-type+ "owner-id" userid})))

(defn move-list-to-collection [userid listid collectionid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (couchio/upsert-object-attribute! (config/delectus-content-bucket)
                                    listid +collection-attribute+ collectionid)
  collectionid)

(defn make-list-uncollected [userid listid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (couchio/upsert-object-attribute! (config/delectus-content-bucket)
                                    listid +collection-attribute+ nil)
  nil)

(defn list-with-id [userid listid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (into {} (.toMap (ensure-list listid))))

(defn list-name [userid listid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                listid +name-attribute+))

(defn find-list-with-name [userid name]
  (ensure-user-exists userid)
  (map #(.get % +id-attribute+)
       (couchio/find-objects
        (config/delectus-content-bucket) []
        {"type" +list-type+ "owner-id" userid "name" name})))

(defn rename-list [userid listid newname]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (let [lists (couchio/find-objects
               (config/delectus-content-bucket) []
               {"type" +list-type+ "owner-id" userid "name" name})]
    (if (empty? lists)
      (let [found-list (ensure-list listid)]
        (try
          (couchio/upsert-object-attribute! (config/delectus-content-bucket) listid +name-attribute+ newname)
          (catch Exception ex
            (throw (ex-info "Couchbase Error"
                            {:cause :couchbase-exception
                             :exception-object ex
                             :userid userid :listid listid})))))
      (throw (ex-info "Name exists"
                      {:cause :list-name-exists
                       :listname newname
                       :userid userid})))))

(defn new-list [userid name]
  (ensure-user-exists userid)
  (let [found-lists (couchio/find-objects
                     (config/delectus-content-bucket) []
                     {"type" +list-type+ "owner-id" userid "name" name})]
    (if (empty? found-lists)
      (let [listid (makeid)
            list-doc (model/make-list-document :id listid :name name :owner-id userid)]
        (.upsert (config/delectus-content-bucket)
                 list-doc)
        listid)
      (throw (ex-info "Name exists"
                      {:cause :list-name-exists
                       :listname name
                       :userid userid})))))

(defn delete-list [userid listid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    listid
                                    +deleted-attribute+
                                    true))

(defn undelete-list [userid listid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    listid
                                    +deleted-attribute+
                                    false))

(defn list-deleted? [userid listid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (let [found-list (ensure-list listid)]
    (try
      (.get found-list +deleted-attribute+)
      (catch Exception ex
        (throw (ex-info "Couchbase Error"
                        {:cause :couchbase-exception
                         :exception-object ex
                         :userid userid :listid listid}))))))

(defn list-columns [userid listid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  )

(defn new-column [userid listid name]
  )
