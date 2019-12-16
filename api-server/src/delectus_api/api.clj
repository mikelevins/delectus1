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
   (com.couchbase.client.core CouchbaseException)
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))



;;; ---------------------------------------------------------------------
;;; ensuring that users, collections, and lists exist and are of the right type
;;; ---------------------------------------------------------------------

(defmacro ensure-user-exists [userid]
  `(if (couchio/user-exists? ~userid)
     ~userid
     (throw (ex-info "No such user"
                     {:cause :user-not-found
                      :userid ~userid}))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "8a61bdbc-3910-4257-afec-9ba34ac3fa45")
;;; (ensure-user-exists $mikelid)
;;; (ensure-user-exists $listid)

(defmacro ensure-user [userid]
  (let [found-user (gensym)]
    `(let [~found-user (couchio/get-user ~userid)]
       (if (and ~found-user
                (= +user-type+ (.get ~found-user +type-attribute+)))
         ~found-user
         (throw (ex-info "No such user"
                         {:cause :user-not-found
                          :userid ~userid}))))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "8a61bdbc-3910-4257-afec-9ba34ac3fa45")
;;; (ensure-user $mikelid)
;;; (ensure-user $listid)
;;; (ensure-user "NOPE!")

(defmacro ensure-collection-exists [collectionid]
  `(if (couchio/collection-exists? ~collectionid)
     ~collectionid
     (throw (ex-info "No such collection"
                     {:cause :collection-not-found
                      :collectionid ~collectionid}))))


;;; (def $collectionid "14bae88e-70c4-4c89-981e-1c744ede469c")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (ensure-collection-exists $collectionid)
;;; (ensure-collection-exists $mikelid)

(defmacro ensure-collection [collectionid]
  (let [found-collection (gensym)]
    `(let [~found-collection (couchio/get-collection ~collectionid)]
       (if (and ~found-collection
                (= +collection-type+ (.get ~found-collection +type-attribute+)))
         ~found-collection
         (throw (ex-info "No such collection"
                         {:cause :collection-not-found
                          :collectionid ~collectionid}))))))

;;; (def $collectionid "14bae88e-70c4-4c89-981e-1c744ede469c")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (ensure-collection $collectionid)

(defmacro ensure-list-exists [listid]
  `(if (couchio/list-exists? ~listid)
     ~listid
     (throw (ex-info "No such list"
                     {:cause :list-not-found
                      :listid ~listid}))))

;;; (def $listid "8a61bdbc-3910-4257-afec-9ba34ac3fa45")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (ensure-list-exists $listid)
;;; (ensure-list-exists $mikelid)

(defmacro ensure-list [listid]
  (let [found-list (gensym)]
    `(let [~found-list (couchio/get-list ~listid)]
       (if (and ~found-list
                (= +list-type+ (.get ~found-list +type-attribute+)))
         ~found-list
         (throw (ex-info "No such list"
                         {:cause :list-not-found
                          :listid ~listid}))))))

;;; (def $listid "8a61bdbc-3910-4257-afec-9ba34ac3fa45")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (ensure-list $listid)

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

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $good-collection-id "14bae88e-70c4-4c89-981e-1c744ede469c")
;;; (def $bad-collection-id "868df782-df55-4528-8034-695eebbff3f3")
;;; (ensure-owner $good-collection-id $mikelid)
;;; (ensure-owner $bad-collection-id $mikelid)

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
  (or (couchio/email->userid email)
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :email email}))))

;;; (userid "mikel@evins.net")
;;; (userid "doo@evins.net")
;;; (ensure-user (:delectus-test-user-id (config/delectus-configuration)))
;;; (ensure-user "NOPE!")

(defn userdata [userid]
  (let [found-user (ensure-user userid)]
    {:userid userid
       :name (.get found-user +name-attribute+)
       :email (.get found-user +email-attribute+)}))

;;; (userdata (:delectus-test-user-id (config/delectus-configuration)))
;;; (userdata "NOPE!")

;;; collections
;;; ---------------------------------------------------------------------

(defn collections [userid]
  (ensure-user-exists userid)
  (map #(select-keys (.toMap %) ["name" "id" "deleted"])
       (couchio/find-objects (config/delectus-content-bucket) []
                             {"type" +collection-type+ "owner-id" userid})))

;;; (userid "mikel@evins.net")
;;; (collections "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (collections "6235e7b7-eb83-47d9-a8ef-ac129601e810")
;;; (collections "BOGUS")

(defn collection-with-id [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  {"name" (couchio/get-object-attribute (config/delectus-content-bucket)
                                        collectionid +name-attribute+)
   "id" (couchio/get-object-attribute (config/delectus-content-bucket)
                                        collectionid +id-attribute+)})

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $collectionid "14bae88e-70c4-4c89-981e-1c744ede469c")
;;; (collection-with-id $mikelid $collectionid)

(defn collection-name [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                collectionid +name-attribute+))

(defn collection-named [userid name]
  (ensure-user-exists userid)
  (let [collections (couchio/find-objects
                     (config/delectus-content-bucket) []
                     {"type" +collection-type+ "owner-id" userid "name" name})]
    (if (empty? collections)
      (throw (ex-info "No such collection"
                      {:cause :collection-not-found
                       :userid userid}))
      (let [collection (first collections)]
        {"name" (.get collection +name-attribute+)
         "id" (.get collection +id-attribute+)}))))

(defn rename-collection [userid collectionid newname]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (let [collection (ensure-collection collectionid)]
    (try
      (couchio/upsert-object-attribute! (config/delectus-content-bucket) collectionid +name-attribute+ newname)
      (catch Exception ex
        (throw (ex-info "Couchbase Error"
                        {:cause :couchbase-exception
                         :exception-object ex
                         :userid userid :collectionid collectionid}))))))

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
  (let [collection (ensure-collection collectionid)]
    (try
      (let [content-bucket (config/delectus-content-bucket)
            mutator (.mutateIn content-bucket collectionid)
            updater (.upsert mutator +deleted-attribute+ true)]
        (.execute updater)
        collectionid)
      (catch Exception ex
        (throw (ex-info "Couchbase Error"
                        {:cause :couchbase-exception
                         :exception-object ex
                         :userid userid :collectionid collectionid}))))))

(defn undelete-collection [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (let [collection (ensure-collection collectionid)]
    (try
      (let [content-bucket (config/delectus-content-bucket)
            mutator (.mutateIn content-bucket collectionid)
            updater (.upsert mutator +deleted-attribute+ false)]
        (.execute updater)
        collectionid)
      (catch Exception ex
        (throw (ex-info "Couchbase Error"
                        {:cause :couchbase-exception
                         :exception-object ex
                         :userid userid :collectionid collectionid}))))))

(defn collection-deleted? [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (let [collection (ensure-collection collectionid)]
    (try
      (.get collection +deleted-attribute+)
      (catch Exception ex
        (throw (ex-info "Couchbase Error"
                        {:cause :couchbase-exception
                         :exception-object ex
                         :userid userid :collectionid collectionid}))))))

(defn collection-lists [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (let [collection-lists (couchio/get-object-attribute (config/delectus-content-bucket)
                                                       collectionid +lists-attribute+)]
    (if collection-lists
      (let [list-map (.toMap collection-lists)
            list-keys (keys list-map)
            found-lists (map couchio/get-list list-keys)
            list-descriptions (map #(if %
                                      {+name-attribute+ (.get % +name-attribute+)
                                       +id-attribute+ (.get % +id-attribute+)}
                                      nil)
                                   found-lists)]
        (into [] (filter identity list-descriptions)))
      nil)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $collectionid "14bae88e-70c4-4c89-981e-1c744ede469c")
;;; (collection-lists $mikelid $collectionid)

;;; lists
;;; ---------------------------------------------------------------------

(defn lists [userid]
  (ensure-user-exists userid)
  (map #(select-keys (.toMap %) ["name" "id"])
       (couchio/find-objects (config/delectus-content-bucket) []
                             {"type" +list-type+ "owner-id" userid})))



