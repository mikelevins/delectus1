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
  `(if (couchio/user-exists? ~userid)
     ~userid
     (throw (ex-info "No such user"
                     {:cause :user-not-found
                      :userid ~userid}))))

(defmacro ensure-user [userid]
  (let [found-user (gensym)]
    `(let [~found-user (couchio/get-user ~userid)]
       (or ~found-user
           (throw (ex-info "No such user"
                           {:cause :user-not-found
                            :userid ~userid}))))))

(defmacro ensure-collection-exists [collectionid]
  `(if (couchio/collection-exists? ~collectionid)
     ~collectionid
     (throw (ex-info "No such collection"
                     {:cause :collection-not-found
                      :collectionid ~collectionid}))))

(defmacro ensure-collection [collectionid]
  (let [found-collection (gensym)]
    `(let [~found-collection (couchio/get-collection ~collectionid)]
       (or ~found-collection
           (throw (ex-info "No such collection"
                           {:cause :collection-not-found
                            :collectionid ~collectionid}))))))

(defmacro ensure-list-exists [listid]
  `(if (couchio/list-exists? ~listid)
     ~listid
     (throw (ex-info "No such list"
                     {:cause :list-not-found
                      :listid ~listid}))))

(defmacro ensure-list [listid]
  (let [found-list (gensym)]
    `(let [~found-list (couchio/get-list ~listid)]
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
  (or (couchio/email->userid email)
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :email email}))))

(defn userdata [userid]
  (let [found-user (ensure-user userid)]
    {:userid userid
       :name (.get found-user +name-attribute+)
       :email (.get found-user +email-attribute+)}))

;;; collections
;;; ---------------------------------------------------------------------

(defn collections [userid]
  (ensure-user-exists userid)
  (map #(select-keys (.toMap %) ["name" "id" "deleted"])
       (couchio/find-objects (config/delectus-content-bucket) []
                             {"type" +collection-type+ "owner-id" userid})))

(defn collection-with-id [userid collectionid]
  (ensure-user-exists userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  {"name" (couchio/get-object-attribute (config/delectus-content-bucket)
                                        collectionid +name-attribute+)
   "id" (couchio/get-object-attribute (config/delectus-content-bucket)
                                        collectionid +id-attribute+)})

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
  (map #(select-keys (.toMap %) ["name" "id"])
       (couchio/find-objects (config/delectus-content-bucket) []
                             {"type" +list-type+
                              "owner-id" userid
                              "collection" collectionid})))

;;; lists
;;; ---------------------------------------------------------------------

(defn lists [userid]
  (ensure-user-exists userid)
  (map #(select-keys (.toMap %) ["name" "id" "collection" "deleted"])
       (couchio/find-objects (config/delectus-content-bucket) []
                             {"type" +list-type+ "owner-id" userid})))

(defn list-move-to-collection [userid listid collectionid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (ensure-collection-exists collectionid)
  (ensure-owner collectionid userid)
  (couchio/upsert-object-attribute! (config/delectus-content-bucket)
                                    listid +collection-attribute+ collectionid)
  collectionid)

(defn list-make-uncollected [userid listid]
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
  {"name" (couchio/get-object-attribute (config/delectus-content-bucket)
                                        listid +name-attribute+)
   "id" (couchio/get-object-attribute (config/delectus-content-bucket)
                                      listid +id-attribute+)})

(defn list-name [userid listid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                listid +name-attribute+))

(defn list-named [userid name]
  (ensure-user-exists userid)
  (let [lists (couchio/find-objects
               (config/delectus-content-bucket) []
               {"type" +list-type+ "owner-id" userid "name" name})]
    (if (empty? lists)
      (throw (ex-info "No such list"
                      {:cause :list-not-found
                       :userid userid}))
      (let [found-list (first lists)]
        {"name" (.get found-list +name-attribute+)
         "id" (.get found-list +id-attribute+)}))))

(defn rename-list [userid listid newname]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (let [found-list (ensure-list listid)]
    (try
      (couchio/upsert-object-attribute! (config/delectus-content-bucket) listid +name-attribute+ newname)
      (catch Exception ex
        (throw (ex-info "Couchbase Error"
                        {:cause :couchbase-exception
                         :exception-object ex
                         :userid userid :listid listid}))))))

(defn new-list [userid name]
  (ensure-user-exists userid)
  (let [found-lists (couchio/find-objects
                     (config/delectus-content-bucket) []
                     {"type" +list-type+ "owner-id" userid "name" name})]
    (if (empty? found-lists)
      (let [id (makeid)
            list-doc (model/make-list-document :id id :name name :owner-id userid)]
        (.upsert (config/delectus-content-bucket)
                 list-doc)
        id)
      (throw (ex-info "Name exists"
                      {:cause :list-name-exists
                       :listname name
                       :userid userid})))))


(defn delete-list [userid listid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (let [found-list (ensure-list listid)]
    (try
      (let [content-bucket (config/delectus-content-bucket)
            mutator (.mutateIn content-bucket listid)
            updater (.upsert mutator +deleted-attribute+ true)]
        (.execute updater)
        listid)
      (catch Exception ex
        (throw (ex-info "Couchbase Error"
                        {:cause :couchbase-exception
                         :exception-object ex
                         :userid userid :listid listid}))))))

(defn undelete-list [userid listid]
  (ensure-user-exists userid)
  (ensure-list-exists listid)
  (ensure-owner listid userid)
  (let [found-list (ensure-list listid)]
    (try
      (let [content-bucket (config/delectus-content-bucket)
            mutator (.mutateIn content-bucket listid)
            updater (.upsert mutator +deleted-attribute+ false)]
        (.execute updater)
        listid)
      (catch Exception ex
        (throw (ex-info "Couchbase Error"
                        {:cause :couchbase-exception
                         :exception-object ex
                         :userid userid :listid listid}))))))

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
  (let [columns (couchio/get-document-path (config/delectus-content-bucket) listid "columns")]
    (if columns
      (let [column-names (.getNames columns)]
        (map (fn [nm]
               (let [col (.get columns nm)]
                 {"id" nm
                  "name" (.get col +name-attribute+)}))
             column-names))
      nil)))

(defn new-column [userid listid name]
  (let [column-descriptions (list-columns userid listid)
        already? (some #(= name (get % "name"))
                       column-descriptions)]
    (if already?
      (throw (ex-info "Column name exists"
                        {:cause :column-name-exists
                         :column-name name
                         :userid userid :listid listid}))
      :create-the-column)))
