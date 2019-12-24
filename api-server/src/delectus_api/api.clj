(ns delectus-api.api
  (:require
   [compojure.api.sweet :refer :all]
   [delectus-api.auth :as auth]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.ensure :as ensure]
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
;;; api functions
;;; ---------------------------------------------------------------------

;;; users
;;; ---------------------------------------------------------------------

;;; authenticate [userid password] => user-map
(defn authenticate [userid password]
  (let [authenticated-user (auth/authenticate-user userid password)]
    (if authenticated-user
      (into {} (.toMap authenticated-user))
      (throw (ex-info "Authentication failed"
                      {:cause :authentication-failed
                       :userid userid})))))

;;; login [email password] => user-map
(defn login [email password]
  (let [authenticated-user (auth/login-user email password)]
    (if authenticated-user
      (into {} (.toMap authenticated-user))
      (throw (ex-info "Login failed"
                      {:cause :login-failed
                       :email email})))))

;;; userid [email] => id-string
(defn userid [email]
  (or (model/email->userid email)
      (throw (ex-info "No such user"
                      {:cause :user-not-found
                       :email email}))))

;;; userdata [userid fields] => user-map
(defn userdata [userid fields]
  (if (empty? fields)
    (into {} (.toMap (ensure/ensure-user userid)))
    (select-keys (into {} (.toMap (ensure/ensure-user userid)))
                 fields)))

;;; (userdata $mikelid)
;;; (userdata $mikelid [+name-attribute+ "id"])

;;; collections
;;; ---------------------------------------------------------------------

;;; collections [userid fields] => list of collection-map
(defn collections [userid fields]
  (ensure/ensure-user-exists userid)
  (map #(into {} (.toMap %))
       (couchio/find-objects (config/delectus-content-bucket) fields
                             {+type-attribute+ +collection-type+ +owner-attribute+ userid})))

;;; (collections $mikelid)
;;; (collections $mikelid [+name-attribute+ "id"])

;;; collection-with-id [userid collectionid] => collection-map
(defn collection-with-id [userid collectionid fields]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (if (empty? fields)
    (into {} (.toMap (ensure/ensure-collection collectionid)))
    (select-keys (into {} (.toMap (ensure/ensure-collection collectionid)))
                 fields)))

;;; (collection-with-id $mikelid "5b541f1a-d34a-4a83-a4bd-9c2b309423bf")
;;; (collection-with-id $mikelid "5b541f1a-d34a-4a83-a4bd-9c2b309423bf" [+name-attribute+ "id" "deleted"])

;;; collection-name [userid collectionid] => name-string
(defn collection-name [userid collectionid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                collectionid +name-attribute+))

;;; find-collections-with-name [userid name] => list of collection-map
(defn find-collections-with-name [userid name fields]
  (ensure/ensure-user-exists userid)
  (map #(into {} (.toMap %))
       (couchio/find-objects (config/delectus-content-bucket) fields
                             {+type-attribute+ +collection-type+ +owner-attribute+ userid +name-attribute+ name})))

;;; (find-collections-with-name $mikelid "Widgets" [])

;;; rename-collection [userid collectionid newname] => name-string
(defn rename-collection [userid collectionid newname]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/with-couchbase-exceptions-rethrown
    (couchio/upsert-object-attribute! (config/delectus-content-bucket) collectionid +name-attribute+ newname))
  newname)

;;; new-collection [userid name] => id-string
(defn new-collection [userid name]
  (ensure/ensure-user-exists userid)
  (let [collections (couchio/find-objects
                     (config/delectus-content-bucket) []
                     {+type-attribute+ +collection-type+ +owner-attribute+ userid +name-attribute+ name})]
    (if (empty? collections)
      (let [id (makeid)
            collection-doc (model/make-collection-document :id id :name name :owner userid)]
        (.upsert (config/delectus-content-bucket)
                 collection-doc)
        id)
      (throw (ex-info "Name exists"
                      {:cause :collection-name-exists
                       :collectionname name
                       :userid userid})))))

;;; delete-collection [userid collectionid] => id-string
(defn delete-collection [userid collectionid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    collectionid
                                    +deleted-attribute+
                                    true)
  collectionid)

;;; undelete-collection [userid collectionid] => id-string
(defn undelete-collection [userid collectionid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    collectionid
                                    +deleted-attribute+
                                    false)
  collectionid)

;;; collection-deleted? [userid collectionid] => Boolean
(defn collection-deleted? [userid collectionid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                collectionid
                                +deleted-attribute+))

;;; collection-lists [userid collectionid] => list of list-map
(defn collection-lists [userid collectionid fields]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (map #(into {} (.toMap %))
       (couchio/find-objects (config/delectus-content-bucket) fields
                             {+type-attribute+ +list-type+
                              +owner-attribute+ userid
                              "collection" collectionid})))

;;; (collection-lists $mikelid "7941ad3f-12b2-409c-9120-18ea9cbc94d5" [])
;;; (collection-lists $mikelid "7941ad3f-12b2-409c-9120-18ea9cbc94d5" [+name-attribute+ "id"])


;;; lists
;;; ---------------------------------------------------------------------

;;; lists [userid] => list of list-map
(defn lists [userid fields]
  (ensure/ensure-user-exists userid)
  (map #(into {} (.toMap %))
       (couchio/find-objects (config/delectus-content-bucket) fields
                             {+type-attribute+ +list-type+ +owner-attribute+ userid})))

;;; (lists $mikelid)
;;; (lists $mikelid ["id" +name-attribute+])

;;; move-list-to-collection [userid listid collectionid] => id-string
(defn move-list-to-collection [userid listid collectionid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/upsert-object-attribute! (config/delectus-content-bucket)
                                    listid +collection-attribute+ collectionid)
  collectionid)

;;; make-list-uncollected [userid listid] => nil
(defn make-list-uncollected [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/upsert-object-attribute! (config/delectus-content-bucket)
                                    listid +collection-attribute+ nil)
  nil)

;;; list-with-id [userid listid] => list-map
(defn list-with-id [userid listid fields]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (if (empty? fields)
    (into {} (.toMap (ensure/ensure-list listid)))
    (select-keys (into {} (.toMap (ensure/ensure-list listid)))
                 fields)))

;;; (list-with-id $mikelid "9bd33bf4-7ef9-458b-b0f6-ca5e65787fbf")
;;; (list-with-id $mikelid "9bd33bf4-7ef9-458b-b0f6-ca5e65787fbf" [+name-attribute+])

;;; list-name [userid listid] => name-string
(defn list-name [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                listid +name-attribute+))

;;; list-collection [userid listid] => id-string
(defn list-collection [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                listid +collection-attribute+))

;;; find-lists-with-name [userid name] => list of list-map
(defn find-lists-with-name [userid name fields]
  (ensure/ensure-user-exists userid)
  (map #(into {} (.toMap %))
       (couchio/find-objects (config/delectus-content-bucket) fields
                             {+type-attribute+ +list-type+ +owner-attribute+ userid +name-attribute+ name})))

;;; (find-lists-with-name $mikelid "Movies")
;;; (find-lists-with-name $mikelid "Movies" ["id" +name-attribute+])

;;; rename-list [userid listid newname] => name-string
(defn rename-list [userid listid newname]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/with-couchbase-exceptions-rethrown
    (couchio/upsert-object-attribute! (config/delectus-content-bucket) listid +name-attribute+ newname))
  newname)

;;; new-list [userid name] => id-string
(defn new-list [userid name]
  (ensure/ensure-user-exists userid)
  (let [found-lists (couchio/find-objects
                     (config/delectus-content-bucket) []
                     {+type-attribute+ +list-type+ +owner-attribute+ userid +name-attribute+ name})]
    (if (empty? found-lists)
      (let [listid (makeid)
            list-doc (model/make-list-document :id listid :name name :owner userid)]
        (.upsert (config/delectus-content-bucket)
                 list-doc)
        listid)
      (throw (ex-info "Name exists"
                      {:cause :list-name-exists
                       :listname name
                       :userid userid})))))

;;; delete-list [userid listid] => id-string
(defn delete-list [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    listid
                                    +deleted-attribute+
                                    true)
  listid)

;;; undelete-list [userid listid] => id-string
(defn undelete-list [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    listid
                                    +deleted-attribute+
                                    false)
  listid)

;;; list-deleted? [userid listid] => Boolean
(defn list-deleted? [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                listid
                                +deleted-attribute+))

(defn list-columns [userid listid fields]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                listid
                                +columns-attribute+)
  )

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "9bd33bf4-7ef9-458b-b0f6-ca5e65787fbf")
;;; (list-columns $mikelid $listid [])
