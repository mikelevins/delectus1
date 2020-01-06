(ns delectus-api.api
  (:require
   [clj-time.core :as t]
   [clj-time.local :refer [local-now]]
   [clojure.pprint :as pp]
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
   )
  (:import
   (com.couchbase.client.core CouchbaseException)
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; TODO
;;; ---------------------------------------------------------------------

;;; 1. new-column: check to see if the offered column name exists in a
;;;    deleted column; if so, undelete the column (it will contain
;;;    legacy data)

;;; ---------------------------------------------------------------------
;;; api functions
;;; ---------------------------------------------------------------------

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (collections $mikelid ["name","type"])
;;; (collections $mikelid [+name-key+ "id"])
;;; (def $collid )
;;; (def $listid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (class (column-named $mikelid $listid "Costar"))

;;; diagnostic
;;; ---------------------------------------------------------------------

(defn motd []
  (let [timestr (str (local-now))]
    (str "Delectus 2.0.1 " timestr)))

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
;;; (userdata $mikelid [+name-key+ "id"])

;;; collections
;;; ---------------------------------------------------------------------

;;; collections [userid fields] => list of collection-map
(defn collections [userid fields]
  (ensure/ensure-user-exists userid)
  (map #(into {} (.toMap %))
       (couchio/find-objects (config/delectus-content-bucket)
                             :keys fields
                             :match {+type-key+ +collection-type+ +owner-key+ userid})))

;;; (collections $mikelid [])
;;; (collections $mikelid [+name-key+ +type-key+])

;;; collection-with-id [userid collectionid] => collection-map
(defn collection-with-id [userid collectionid fields]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (if (empty? fields)
    (into {} (.toMap (ensure/ensure-collection collectionid)))
    (select-keys (into {} (.toMap (ensure/ensure-collection collectionid)))
                 fields)))

;;; (collection-with-id $mikelid "5b541f1a-d34a-4a83-a4bd-9c2b309423bf" [])
;;; (collection-with-id $mikelid "5b541f1a-d34a-4a83-a4bd-9c2b309423bf" [+name-key+ "id" "deleted"])

;;; collection-name [userid collectionid] => name-string
(defn collection-name [userid collectionid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                collectionid +name-key+))

;;; find-collections-with-name [userid name] => list of collection-map
(defn find-collections-with-name [userid name fields]
  (ensure/ensure-user-exists userid)
  (map #(into {} (.toMap %))
       (couchio/find-objects (config/delectus-content-bucket)
                             :keys fields
                             :match {+type-key+ +collection-type+ +owner-key+ userid +name-key+ name})))

;;; (find-collections-with-name $mikelid "Things" [])

;;; rename-collection [userid collectionid newname] => name-string
(defn rename-collection [userid collectionid newname]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/with-couchbase-exceptions-rethrown
    (couchio/upsert-object-attribute! (config/delectus-content-bucket) collectionid +name-key+ newname))
  newname)

;;; new-collection [userid name] => id-string
(defn new-collection [userid name]
  (ensure/ensure-user-exists userid)
  (let [collections (couchio/find-objects (config/delectus-content-bucket)
                                          :keys []
                                          :match {+type-key+ +collection-type+ +owner-key+ userid +name-key+ name})]
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

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $collid (new-collection $mikelid "Plans"))

;;; delete-collection [userid collectionid] => id-string
(defn delete-collection [userid collectionid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    collectionid
                                    +deleted-key+
                                    true)
  collectionid)

;;; undelete-collection [userid collectionid] => id-string
(defn undelete-collection [userid collectionid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    collectionid
                                    +deleted-key+
                                    false)
  collectionid)

;;; collection-deleted? [userid collectionid] => Boolean
(defn collection-deleted? [userid collectionid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                collectionid
                                +deleted-key+))

;;; collection-lists [userid collectionid fields] => list of list-map
(defn collection-lists [userid collectionid fields]
  (ensure/ensure-user-exists userid)
  (if (nil? collectionid)
    (map #(into {} (.toMap %))
         (couchio/find-objects (config/delectus-content-bucket)
                               :keys fields
                               :match {+type-key+ +list-type+
                                       +owner-key+ userid
                                       +collection-key+ nil}))
    (do (ensure/ensure-collection-exists collectionid)
        (ensure/ensure-owner collectionid userid)
        (map #(into {} (.toMap %))
             (couchio/find-objects (config/delectus-content-bucket)
                                   :keys fields
                                   :match {+type-key+ +list-type+
                                           +owner-key+ userid
                                           +collection-key+ collectionid})))))

;;; (time (collection-lists $mikelid nil ["name"]))


;;; lists
;;; ---------------------------------------------------------------------

;;; lists [userid] => list of list-map
(defn lists [userid fields]
  (ensure/ensure-user-exists userid)
  (map #(into {} (.toMap %))
       (couchio/find-objects (config/delectus-content-bucket)
                             :keys fields
                             :match {+type-key+ +list-type+ +owner-key+ userid})))

;;; (lists $mikelid [+name-key+])

;;; move-list-to-collection [userid listid collectionid] => id-string
(defn move-list-to-collection [userid listid collectionid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (ensure/ensure-collection-exists collectionid)
  (ensure/ensure-owner collectionid userid)
  (couchio/upsert-object-attribute! (config/delectus-content-bucket)
                                    listid +collection-key+ collectionid)
  collectionid)

;;; make-list-uncollected [userid listid] => nil
(defn make-list-uncollected [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/upsert-object-attribute! (config/delectus-content-bucket)
                                    listid +collection-key+ nil)
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
;;; (list-with-id $mikelid "9bd33bf4-7ef9-458b-b0f6-ca5e65787fbf" [+name-key+])

;;; list-name [userid listid] => name-string
(defn list-name [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                listid +name-key+))

;;; list-collection [userid listid] => id-string
(defn list-collection [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                listid +collection-key+))

;;; find-lists-with-name [userid name] => list of list-map
(defn find-lists-with-name [userid name fields]
  (ensure/ensure-user-exists userid)
  (map #(into {} (.toMap %))
       (couchio/find-objects (config/delectus-content-bucket)
                             :keys fields
                             :match {+type-key+ +list-type+ +owner-key+ userid +name-key+ name})))

;;; (find-lists-with-name $mikelid "Movies" ["id" +name-key+])
;;; (find-lists-with-name $mikelid "Zipcodes" ["id" +name-key+])

;;; rename-list [userid listid newname] => name-string
(defn rename-list [userid listid newname]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/with-couchbase-exceptions-rethrown
    (couchio/upsert-object-attribute! (config/delectus-content-bucket) listid +name-key+ newname))
  newname)

;;; new-list [userid name] => id-string
(defn new-list [userid name]
  (ensure/ensure-user-exists userid)
  (let [found-lists (couchio/find-objects (config/delectus-content-bucket)
                                          :keys []
                                          :match {+type-key+ +list-type+
                                                  +owner-key+ userid
                                                  +name-key+ name})]
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
                                    +deleted-key+
                                    true)
  listid)

;;; undelete-list [userid listid] => id-string
(defn undelete-list [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/update-object-attribute! (config/delectus-content-bucket)
                                    listid
                                    +deleted-key+
                                    false)
  listid)

;;; list-deleted? [userid listid] => Boolean
(defn list-deleted? [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (couchio/get-object-attribute (config/delectus-content-bucket)
                                listid
                                +deleted-key+))

(defn list-columns [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [cols (model/get-list-columns listid)]
    (if cols
      (into {} (.toMap cols))
      nil)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (list-columns $mikelid $listid [])

(defn new-column [userid listid name]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [col-id (model/next-column-id listid)]
    (model/assert-column! listid col-id name)
    col-id))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (new-column $mikelid $listid "Genre")

(defn column-with-id [userid listid columnid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [col (model/get-column listid columnid)]
    (if col
      (into {} (.toMap col))
      (throw (ex-info "Column not found"
                      {:cause :column-not-found
                       :userid userid :listid listid :columnid columnid})))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $listid "12c8b02b-8bba-4179-b328-94010ede7f01")
;;; (column-with-id $mikelid $listid "9")

(defn column-name [userid listid columnid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [col (model/get-column listid columnid)]
    (if col
      (.get col +name-key+)
      (throw (ex-info "Column not found"
                      {:cause :column-not-found
                       :userid userid :listid listid :columnid columnid})))))

(defn column-named [userid listid name]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [cols (model/get-list-columns listid)]
    (if cols
      (let [cols-map (into {} (.toMap cols))
            found-cols (keep #(when (= (get (val %) +name-key+)
                                       name)
                                (val %))
                             cols-map)]
        (if (empty? found-cols)
          (throw (ex-info "Column not found"
                          {:cause :column-not-found
                           :userid userid :listid listid :name name}))
          (into {} (first found-cols))))
      (throw (ex-info "Column not found"
                      {:cause :column-not-found
                       :userid userid :listid listid :name name})))))


(defn column-deleted [userid listid columnid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [col (model/get-column listid columnid)]
    (if col
      (.get col +deleted-key+)
      (throw (ex-info "Column not found"
                      {:cause :column-not-found
                       :userid userid :listid listid :columnid columnid})))))

;;; delete-column [userid listid columnid] => id-string
(defn delete-column [userid listid columnid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [bucket (config/delectus-content-bucket)
        keypath (str +columns-key+ "." columnid "." +deleted-key+)]
    (couchio/update-document-path! bucket
                                   listid
                                   keypath
                                   true))
  columnid)

;;; undelete-column [userid listid columnid] => id-string
(defn undelete-column [userid listid columnid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [bucket (config/delectus-content-bucket)
        keypath (str +columns-key+ "." columnid "." +deleted-key+)]
    (couchio/update-document-path! bucket
                                   listid
                                   keypath
                                   false))
  columnid)

;;; rename-column [userid listid columnid name] => id-string
(defn rename-column [userid listid columnid name]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [bucket (config/delectus-content-bucket)
        keypath (str +columns-key+ "." columnid "." +name-key+)]
    (couchio/update-document-path! bucket
                                   listid
                                   keypath
                                   name))
  columnid)

(defn list-items [userid listid & {:keys [offset limit]
                                   :or {offset 0 limit 100}}]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (map #(into {} (.toMap %))
       (couchio/find-objects (config/delectus-content-bucket)
                             :offset offset :limit limit
                             :match {+type-key+ +item-type+
                                     +owner-key+ userid
                                     +list-key+ listid})))


(defn list-item-with-id [userid listid itemid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (let [found-doc (ensure/ensure-item itemid)]
    (if (nil? found-doc)
      nil
      (let [found-obj (.content found-doc)]
        (if (and (couchio/json-object-type? found-obj +item-type+)
                 (couchio/json-object-owner? found-obj userid)
                 (= listid (.get found-obj +list-key+)))
          (into {} (.toMap found-obj))
          nil)))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $greerid "6235e7b7-eb83-47d9-a8ef-ac129601e810")
;;; (def $moviesid "b65f029c-6108-4c9c-a973-7fa16b8841c0")
;;; (def $itemid "002153f5-431a-47a3-82d5-f2161ed1d4d0")
;;; (def $item (list-item-with-id $mikelid $moviesid $itemid))
;;; (list-item-with-id $greerid $moviesid $itemid)

(defn new-list-item [userid listid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [found-columns (model/get-list-columns listid)]
    (if (nil? found-columns)
      (throw (ex-info "No columns found" {:context 'new-list-item :listid listid}))
      (let [columns-map (.getNames found-columns)
            column-count (count (keys columns-map))
            vals (take column-count (repeat nil))
            item-doc (model/values->item-document userid listid vals)
            asserted-doc (model/assert-item! item-doc)]
        (if (nil? asserted-doc)
          (throw (ex-info "Couchbase error" {:context 'new-list-item :listid listid
                                             :cause :new-list-item-failed}))
          (.get (.content asserted-doc) +id-key+))))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $moviesid "b65f029c-6108-4c9c-a973-7fa16b8841c0")
;;; (def $new-item (new-list-item $mikelid $moviesid))

(defn delete-list-item [userid listid itemid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [item-exists? (ensure/ensure-item-exists itemid)]
    (if item-exists?
      (do (couchio/update-object-attribute! (config/delectus-content-bucket)
                                            itemid +deleted-key+ true)
          itemid)
      (throw (ex-info "No such item" {:context 'delete-list-item :listid listid :itemid itemid})))))

(defn undelete-list-item [userid listid itemid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [item-exists? (ensure/ensure-item-exists itemid)]
    (if item-exists?
      (do (couchio/update-object-attribute! (config/delectus-content-bucket)
                                            itemid +deleted-key+ false)
          itemid)
      (throw (ex-info "No such item" {:context 'undelete-list-item :listid listid :itemid itemid})))))


(defn list-item-deleted [userid listid itemid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (let [item-exists? (ensure/ensure-item-exists itemid)]
    (if item-exists?
      (couchio/get-object-attribute (config/delectus-content-bucket)
                                    itemid +deleted-key+)
      (throw (ex-info "No such item" {:context 'list-item-deleted :listid listid :itemid itemid})))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $moviesid "b65f029c-6108-4c9c-a973-7fa16b8841c0")
;;; (def $itemid "002153f5-431a-47a3-82d5-f2161ed1d4d0")
;;; (list-item-deleted $mikelid $moviesid $itemid)
;;; (delete-list-item $mikelid $moviesid $itemid)
;;; (undelete-list-item $mikelid $moviesid $itemid)

(defn item-column-value [userid listid itemid columnid]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (ensure/ensure-item-exists itemid)
  (ensure/ensure-owner itemid userid)
  (let [keypath (str +fields-key+ "." columnid)]
    (couchio/get-document-path (config/delectus-content-bucket)
                               itemid keypath)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $moviesid "b65f029c-6108-4c9c-a973-7fa16b8841c0")
;;; (def $itemid "002153f5-431a-47a3-82d5-f2161ed1d4d0")
;;; (item-column-value $mikelid $moviesid $itemid 3)

(defn set-item-column-value! [userid listid itemid columnid value]
  (ensure/ensure-user-exists userid)
  (ensure/ensure-list-exists listid)
  (ensure/ensure-owner listid userid)
  (ensure/ensure-item-exists itemid)
  (ensure/ensure-owner itemid userid)
  (let [keypath (str +fields-key+ "." columnid)
        columnpath (str +columns-key+ "." columnid)]
    ;; we use upsert because adding a column to the list does not
    ;; automatically add a field to every item in the list. The
    ;; destination field may therefore not exist in the destination
    ;; item. In order to avoid adding bogus fields, we must first
    ;; check that the target column exists in the list, and reject the
    ;; upsert if it does not.
    (if (couchio/document-path-exists? (config/delectus-content-bucket)
                                       listid columnpath)
      (couchio/upsert-document-path! (config/delectus-content-bucket)
                                     itemid keypath value)
      (throw (ex-info "No such column" {:cause :no-such-column :context 'set-item-column-value
                                        :listid listid :itemid itemid :columnid columnid})))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $moviesid "b65f029c-6108-4c9c-a973-7fa16b8841c0")
;;; (list-with-id $mikelid $moviesid ["columns"])
;;; (def $itemid "002153f5-431a-47a3-82d5-f2161ed1d4d0")
;;; (item-with-id $mikelid $moviesid $itemid)
;;; (item-column-value $mikelid $moviesid $itemid 6)
;;; (set-item-column-value! $mikelid $moviesid $itemid 101 "bogus")
;;; (set-item-column-value! $mikelid $moviesid $itemid 6 "a test comment")
