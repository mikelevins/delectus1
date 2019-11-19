(ns delectus-api-server.api
  (:require
   [buddy.hashers :as hashers]
   [clojure.edn :as edn]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.constants :refer :all]
   [delectus-api-server.couchio :as couchio]
   [delectus-api-server.errors :as errors]
   [delectus-api-server.identifiers :refer [makeid]]
   [delectus-api-server.itemid :as itemid]
   [delectus-api-server.model :as model]
   [delectus-api-server.utilities :as utils])
  (:import
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; =====================================================================
;;; helper functions
;;; =====================================================================

(defn ensure-default-collection [owner-id collection-id]
  (errors/error-if-nil owner-id "Missing :owner-id parameter" {:context 'ensure-default-collection})
  (errors/error-if-nil (couchio/get-user owner-id)
                       "No such user"
                       {:parameter :owner-id :value owner-id :context 'ensure-default-collection})

  (let [already (model/name->collection owner-id +standard-default-collection-name+)]
    (if already
      (.get already +id-attribute+)
      (let [collection-id (or collection-id (makeid))
            collection-doc (model/make-default-collection
                            :id collection-id
                            :owner-id owner-id)]

        (.upsert (config/delectus-content-bucket)
                 collection-doc)
        collection-id))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (ensure-default-collection $mikelid nil)

;;; =====================================================================
;;; API-endpoint functions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; admin
;;; ---------------------------------------------------------------------

;;; private admin endpoints

;;; TODO
;;; api/register-user
(defn register-user [email password])
;;; api/update-user!
(defn update-user! [userid new-values-map])

;;; ---------------------------------------------------------------------
;;; users & sessions
;;; ---------------------------------------------------------------------

;;; /delectus/login
;;; ---------------------------------------------------------------------

(defn login [email password]
  (let [found-user (model/email->user email)]
    (if found-user
      (if (hashers/check password (.get found-user "password-hash"))
        found-user
        false)
      false)))

;;; (def $username (:delectus-test-user (config/delectus-configuration)))
;;; (def $password (:delectus-test-user-password (config/delectus-configuration)))
;;; (login $username $password)
;;; (login "foo" "bar")

;;; /delectus/userid
(defn userid [email] (model/email->userid email))

;;; (def $email (:delectus-test-user (config/delectus-configuration)))
;;; (def $userid (userid $email))
;;; (def $userid (userid "NOPE!"))

;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

;;; /delectus/collections
;;; ---------------------------------------------------------------------

(defn collections [userid]
  (couchio/find-objects (config/delectus-content-bucket)
                        []
                        {+type-attribute+ +collection-type+
                         +owner-id-attribute+ userid}))

;;; (def $colls (collections (model/email->userid "mikel@evins.net")))
;;; (every? #(couchio/json-object-type? % +collection-type+) $colls)
;;; (some #(and (not (couchio/json-object-type? % +collection-type+)) %) $colls)
;;; (collections (model/email->userid "nobody@nowhere.net"))

;;; /delectus/collection_with_id
;;; ---------------------------------------------------------------------

(defn collection-with-id [userid collection-id]
  (let [found (couchio/get-collection collection-id)]
    (if (and found
             (couchio/json-object-owner? found userid)
             (couchio/json-object-type? found +collection-type+))
      found
      nil)))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (collection-with-id $mikelid $defaultid)
;;; (def $greerid "6235e7b7-eb83-47d9-a8ef-ac129601e810")
;;; (collection-with-id $greerid $defaultid)

;;; /delectus/collection_name
;;; ---------------------------------------------------------------------

(defn collection-name [userid collection-id]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The collection doesn't exist" content-bucket collection-id)

    (let [collection-cbmap (CouchbaseMap. collection-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus Collection" collection-cbmap +collection-type+)
      (couchio/error-if-wrong-owner "Can't inspect collection" collection-cbmap userid)

      (.get collection-cbmap +name-attribute+))))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (collection-name $mikelid $defaultid)

;;; /delectus/collection_named
;;; ---------------------------------------------------------------------

(defn collection-named [userid collection-name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id" "items"]
                                    {+type-attribute+ +collection-type+
                                     +name-attribute+ collection-name
                                     +owner-id-attribute+ userid})]
    (if (empty? found)
      nil
      (first found))))

;;; (collection-named (model/email->userid "mikel@evins.net") "Default Collection")
;;; (collection-named (model/email->userid "mikel@evins.net") "NOPE!")


;;; /delectus/rename_collection
;;; ---------------------------------------------------------------------

(defn rename-collection [userid collection-id new-name]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The collection doesn't exist" content-bucket collection-id)

    (let [collection-cbmap (CouchbaseMap. collection-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus Collection" collection-cbmap +collection-type+)
      (couchio/error-if-wrong-owner "Can't update collection" collection-cbmap userid)

      (let [mutator (.mutateIn content-bucket collection-id)
            updater (.upsert mutator +name-attribute+ new-name)]
        (.execute updater))
      collection-id)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (collections (model/email->userid "mikel@evins.net"))
;;; (def $planets (collection-named $mikelid "Planets"))
;;; (def $collid (.get $planets "id"))
;;; (rename-collection $mikelid $collid "My Planets")
;;; (collection-with-id $mikelid $collid)

;;; /delectus/new_collection
;;; ---------------------------------------------------------------------

(defn new-collection [& {:keys [id name owner-id]
                         :or {id (makeid)
                              name nil
                              owner-id nil}}]
  (couchio/error-if-id-exists id)
  (errors/error-if-nil name "Missing :name parameter" {:context 'new-collection})
  (errors/error-if-nil owner-id "Missing :owner-id parameter" {:context 'new-collection})
  (errors/error-if-nil (couchio/get-user owner-id)
                       "No such user"
                       {:parameter :owner-id :value owner-id :context 'new-collection})
  (errors/error-if (model/name->collection owner-id name)
                   "Collection name exists"
                   {:parameter :name :value name})

  (let [collection-doc (model/make-collection-document
                        :id id
                        :name name
                        :owner-id owner-id)]

    (.upsert (config/delectus-content-bucket)
             collection-doc)
    id))

;;; (new-collection :id (makeid) :name "Parts" :owner-id (model/email->userid "mikel@evins.net"))
;;; (new-collection :id (makeid) :name "Stuff" :owner-id (model/email->userid "nobody@evins.net"))


;;; /delectus/delete_collection
;;; /delectus/undelete_collection
;;; ---------------------------------------------------------------------
;;; TODO: make it impossible to delete the default collection

(defn mark-collection-deleted [userid collection-id deleted?]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)
        new-deleted-value (if deleted? true false)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The collection doesn't exist" content-bucket collection-id)

    (let [collection-cbmap (CouchbaseMap. collection-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus Collection" collection-cbmap +collection-type+)
      (couchio/error-if-wrong-owner "Can't update collection" collection-cbmap userid)

      (let [mutator (.mutateIn content-bucket collection-id)
            updater (.upsert mutator +deleted-attribute+ new-deleted-value)]
        (.execute updater))
      collection-id)))


;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $coll (collection-with-id $mikelid $defaultid))
;;; (mark-collection-deleted $mikelid $defaultid false)

;;; /delectus/collection_deleted
;;; ---------------------------------------------------------------------

(defn collection-deleted? [userid collection-id]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The collection doesn't exist" content-bucket collection-id)

    (let [collection-cbmap (CouchbaseMap. collection-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus Collection" collection-cbmap +collection-type+)
      (couchio/error-if-wrong-owner "Can't inspect collection" collection-cbmap userid)

      (.get collection-cbmap +deleted-attribute+))))


;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $coll (collection-with-id $mikelid $defaultid))
;;; (collection-deleted? $mikelid $defaultid)


;;; /delectus/collection_lists
;;; ---------------------------------------------------------------------

(defn collection-lists [userid collection-id]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The collection doesn't exist" content-bucket collection-id)

    (let [collection-cbmap (CouchbaseMap. collection-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus Collection" collection-cbmap +collection-type+)
      (couchio/error-if-wrong-owner "Can't inspect collection" collection-cbmap userid)

      (let [lists-set (.get collection-cbmap +lists-attribute+)]
        (if (nil? lists-set)
          nil
          (let [listids (into [] (.getNames lists-set))
                list-names (map #(if (couchio/object-attribute-exists? content-bucket % +name-attribute+)
                                   (couchio/get-object-attribute content-bucket % +name-attribute+)
                                   nil)
                                listids)]
            (zipmap list-names listids)))))))


;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (collection-lists $mikelid $defaultid)

;;; /delectus/collection_add_list
;;; ---------------------------------------------------------------------

(defn collection-add-list [userid collection-id list-id]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The collection doesn't exist" content-bucket collection-id)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)

    (let [collection-cbmap (CouchbaseMap. collection-id content-bucket)
          list-cbmap (CouchbaseMap. list-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus Collection" collection-cbmap +collection-type+)
      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't update collection" collection-cbmap userid)
      (couchio/error-if-wrong-owner "Can't update list" list-cbmap userid)

      ;;; NOTE: the lists in a collection are represented as the ID strings of the
      ;;;       list documents. The collection's "lists" field is a set of these
      ;;;       ID strings. CouchbaseArrayList does not support removing a value
      ;;;       by value, only by index, which would mean we must fetch the
      ;;;       lists object and search it for the ID to be removed. Instead,
      ;;;       we represent the set as a map (that is, as a JSON object). The
      ;;;       IDs are stored as the keys of the map, which means we can remove them
      ;;;       using the subdocument API without the extra fetch-and-compare.
      ;;;       The value stored on each key is arbitrary, so we store nil
      (let [mutator (.mutateIn content-bucket collection-id)
            updater (.upsert mutator (str +lists-attribute+ "." list-id) nil)]
        (.execute updater))
      collection-id)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (def $default (couchio/get-collection $defaultid))
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (collection-add-list $mikelid $defaultid $thingsid)

;;; /delectus/collection_remove_list
;;; ---------------------------------------------------------------------

(defn collection-remove-list [userid collection-id list-id]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The collection doesn't exist" content-bucket collection-id)

    (let [collection-cbmap (CouchbaseMap. collection-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus Collection" collection-cbmap +collection-type+)
      (couchio/error-if-wrong-owner "Can't update collection" collection-cbmap userid)

      (let [lookup (.lookupIn content-bucket collection-id)
            list-path (str +lists-attribute+ "." list-id)
            list-exists-test (.exists lookup (into-array [list-path]))
            list-exists? (.content (.execute list-exists-test) 0)]
        (if list-exists?
          (let [mutator (.mutateIn content-bucket collection-id)
                updater (.remove mutator list-path)]
            (.execute updater))))
      collection-id)))

;;; (def $bucket (config/delectus-content-bucket))
;;; (def $collid (.get (collection-named (model/email->userid "mikel@evins.net") "Default Collection") "id"))
;;; (def $coll (couchio/get-document $bucket $collid))
;;; (.toMap (.content $coll))
;;; (def $thingsid (.get (list-named (model/email->userid "mikel@evins.net") "Things") "id"))
;;; (def $things (couchio/get-document $bucket $thingsid))
;;; (.toMap (.content $things))
;;; (def $mikelid (model/email->userid "mikel@evins.net"))
;;; (collection-remove-list $mikelid $collid $thingsid)

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------


;;; /delectus/lists
;;; ---------------------------------------------------------------------

(defn lists [userid]
  (let [bucket (config/delectus-content-bucket)]
    (couchio/find-objects bucket ["name" "id"]
                          {+type-attribute+ +list-type+
                           +owner-id-attribute+ userid})))

;;; (lists (model/email->userid "mikel@evins.net"))

;;; /delectus/list_with_id
;;; ---------------------------------------------------------------------

(defn list-with-id [userid list-id]
  (let [found (couchio/get-list list-id)]
    (if (and found
             (couchio/json-object-owner? found userid)
             (couchio/json-object-type? found +list-type+))
      found
      nil)))

;;; (def $listid (.get (find-list-by-name (model/email->userid "mikel@evins.net") "Things") "id"))
;;; (find-list-by-id (model/email->userid "mikel@evins.net") $listid)

;;; /delectus/list_name
;;; ---------------------------------------------------------------------

(defn list-name [userid list-id]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)

    (let [list-cbmap (CouchbaseMap. list-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't inspect list" list-cbmap userid)

      (.get list-cbmap +name-attribute+))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (list-name $mikelid $thingsid)

;;; /delectus/list_named
;;; ---------------------------------------------------------------------

(defn list-named [userid list-name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id"]
                                    {+type-attribute+ +list-type+
                                     +name-attribute+ list-name})]
    (if (empty? found)
      nil
      (first found))))

;;; (find-list-by-name (model/email->userid "mikel@evins.net") "Things")
;;; (find-list-by-name (model/email->userid "mikel@evins.net") "NOPE!")

;;; /delectus/rename_list
;;; ---------------------------------------------------------------------

(defn rename-list [userid list-id new-name]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)

    (let [list-cbmap (CouchbaseMap. list-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't update list" list-cbmap userid)

      (let [mutator (.mutateIn content-bucket list-id)
            updater (.upsert mutator +name-attribute+ new-name)]
        (.execute updater))
      list-id)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (lists (model/email->userid "mikel@evins.net"))
;;; (def $stuff (list-named $mikelid "Stuff"))
;;; (def $listid (.get $stuff "id"))
;;; (rename-list $mikelid $listid "My Stuff")
;;; (list-with-id $mikelid $listid)

;;; /delectus/new_list
;;; ---------------------------------------------------------------------

(defn new-list [& {:keys [id name owner-id]
                   :or {id (makeid)
                        name nil
                        owner-id nil}}]
  (couchio/error-if-id-exists id)
  (errors/error-if-nil name "Missing :name parameter" {:context 'new-list})
  (errors/error-if-nil owner-id "Missing :owner-id parameter" {:context 'new-list})
  (errors/error-if-nil (couchio/get-user owner-id)
                       "No such user"
                       {:parameter :owner-id :value owner-id :context 'new-list})
  (errors/error-if (model/name->list owner-id name)
                   "List name exists"
                   {:parameter :name :value name})

  (let [list-doc (model/make-list-document :id id
                                           :name name
                                           :owner-id owner-id)]

    (.upsert (config/delectus-content-bucket)
             list-doc)
    id))

;;; (new-list :id (makeid) :name "Stuff" :owner-id (model/email->userid "mikel@evins.net"))

;;; /delectus/delete_list
;;; /delectus/undelete_list
;;; ---------------------------------------------------------------------

(defn mark-list-deleted [userid list-id deleted?]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)
        new-deleted-value (if deleted? true false)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)

    (let [list-cbmap (CouchbaseMap. list-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't update list" list-cbmap userid)

      (let [mutator (.mutateIn content-bucket list-id)
            updater (.upsert mutator +deleted-attribute+ new-deleted-value)]
        (.execute updater))
      list-id)))

;;; /delectus/list_deleted
;;; ---------------------------------------------------------------------

(defn list-deleted? [userid list-id]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)

    (let [list-cbmap (CouchbaseMap. list-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't inspect list" list-cbmap userid)

      (.get list-cbmap +deleted-attribute+))))

;;; /delectus/list_columns
;;; ---------------------------------------------------------------------

(defn list-columns [userid list-id]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)

    (let [list-cbmap (CouchbaseMap. list-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't inspect list" list-cbmap userid)

      (.get list-cbmap +columns-attribute+))))


;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (list-columns $mikelid $thingsid)


;;; /delectus/column_with_id
;;; ---------------------------------------------------------------------

(defn column-with-id [& {:keys [list-id owner-id column-id]
                         :or {list-id nil
                              owner-id nil
                              column-id nil}}]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]
    
    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket owner-id)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)
    (errors/error-if-nil column-id "Missing :column-id parameter" {:context 'column-with-id})

    (let [lookup (.lookupIn content-bucket list-id)
          column-path (str +columns-attribute+ "." column-id)
          value-getter (.get lookup (into-array [column-path]))]
      (.content (.execute value-getter) 0))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (.toMap (column-with-id  :owner-id $mikelid :list-id $thingsid :column-id "0"))
;;; (column-with-id :owner-id $mikelid :list-id $thingsid :column-id "NOPE!")

;;; /delectus/column_name
;;; ---------------------------------------------------------------------

(defn column-name [& {:keys [list-id owner-id column-id]
                      :or {list-id nil
                           owner-id nil
                           column-id nil}}]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]
    
    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket owner-id)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)
    (errors/error-if-nil column-id "Missing :column-id parameter" {:context 'column-with-id})

    (let [lookup (.lookupIn content-bucket list-id)
          column-path (str +columns-attribute+ "." column-id "." +name-attribute+)
          value-getter (.get lookup (into-array [column-path]))]
      (.content (.execute value-getter) 0))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (column-name :owner-id $mikelid :list-id $thingsid :column-id "0")
;;; (column-name :owner-id $mikelid :list-id $thingsid :column-id "NOPE!")

;;; /delectus/column_named
;;; ---------------------------------------------------------------------

(defn column-named [& {:keys [list-id owner-id column-name]
                       :or {list-id nil
                            owner-id nil
                            column-name nil}}]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]
    
    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket owner-id)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)
    (errors/error-if-nil column-name "Missing :column-name parameter" {:context 'column-named})

    (let [list-cbmap (CouchbaseMap. list-id content-bucket)]

      (errors/error-if-nil list-cbmap "No List found" {:id list-id})
      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't inspect list" list-cbmap owner-id)
      
      (let [columns (.get list-cbmap "columns")]
        (errors/error-if-nil columns "Couldn't get list columns" {:id list-id})
        (let [column-keys (.getNames columns)]
          (some (fn [key]
                  (let [col (.get columns key)]
                    (and (= column-name (.get col +name-attribute+))
                         col)))
                column-keys))))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (column-named :owner-id $mikelid :list-id $thingsid :column-name "Title")
;;; (column-named :owner-id $mikelid :list-id $thingsid :column-name "Star")
;;; (column-named :owner-id $mikelid :list-id $thingsid :column-name "NOPE!")

;;; /delectus/column_deleted
;;; ---------------------------------------------------------------------

(defn column-deleted? [& {:keys [list-id owner-id column-id]
                          :or {list-id nil
                               owner-id nil
                               column-id nil}}]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]
    
    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket owner-id)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)
    (errors/error-if-nil column-id "Missing :column-id parameter" {:context 'column-with-id})
    
    (let [lookup (.lookupIn content-bucket list-id)
          column-path (str +columns-attribute+ "." column-id "." +deleted-attribute+)
          value-getter (.get lookup (into-array [column-path]))]
      (.content (.execute value-getter) 0))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (column-deleted? :owner-id $mikelid :list-id $thingsid :column-id "0")
;;; (column-deleted? :owner-id $mikelid :list-id $thingsid :column-id "NOPE!")


;;; /delectus/new_column
;;; ---------------------------------------------------------------------

(defn new-column [& {:keys [name list-id owner-id]
                     :or {name nil
                          list-id nil
                          owner-id nil}}]

  (errors/error-if-nil name "Missing :name parameter" {:context 'new-column})
  (errors/error-if-nil name "Missing :list-id parameter" {:context 'new-column})
  (errors/error-if-nil owner-id "Missing :owner-id parameter" {:context 'new-column})
  (errors/error-if-not (model/user-exists? owner-id)
                       "No such user"
                       {:parameter :owner-id :value owner-id :context 'new-column})
  (errors/error-if-not (model/list-exists? list-id)
                       "No such list"
                       {:parameter :list-id :value list-id :context 'new-column})
  
  (let [bucket (config/delectus-content-bucket)
        list-cbmap (CouchbaseMap. list-id bucket)]
    
    (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
    (couchio/error-if-wrong-owner "Can't update list" list-cbmap owner-id)

    (let [old-columns (get list-cbmap +columns-attribute+)
          old-column-ids (into [] (.getNames old-columns))
          new-column-id (itemid/next-itemid old-column-ids)
          column-obj (model/make-column-object
                      :id new-column-id :name name)
          mutator (.mutateIn bucket list-id)
          updater (.upsert mutator (str +columns-attribute+ "." new-column-id) column-obj)]
      (.execute updater))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (new-column :owner-id $mikelid :list-id $thingsid :name "Title")
;;; (new-column :owner-id $mikelid :list-id $thingsid :name "Star")


;;; /delectus/delete_column
;;; /delectus/undelete_column
;;; ---------------------------------------------------------------------

(defn mark-column-deleted [owner-id list-id column-id deleted?]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]
    
    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket owner-id)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)
    (errors/error-if-nil column-id "Non-null column-id parameter required" {:context 'column-with-id})
    
    (let [lookup (.lookupIn content-bucket list-id)
          column-exists-path (str +columns-attribute+ "." column-id)
          value-getter (.exists lookup (into-array [column-exists-path]))
          column-exists? (.content (.execute value-getter) 0)]
      (if column-exists?
        (let [mutator (.mutateIn content-bucket list-id)
              column-deleted-path (str +columns-attribute+ "." column-id "." +deleted-attribute+)
              updater (.upsert mutator column-deleted-path (if deleted? true false))]
          (.execute updater)
          list-id)))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (mark-column-deleted $mikelid $thingsid "0" false)
;;; (column-deleted? :owner-id $mikelid :list-id $thingsid :column-id "0")


;;; /delectus/rename_column
;;; ---------------------------------------------------------------------

(defn rename-column [owner-id list-id column-id new-name]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]
    
    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket owner-id)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)
    (errors/error-if-nil column-id "Non-null column-id parameter required" {:context 'column-with-id})
    (errors/error-if-nil new-name "Non-null new-name parameter required" {:context 'column-with-id})
    
    (let [lookup (.lookupIn content-bucket list-id)
          column-exists-path (str +columns-attribute+ "." column-id)
          value-getter (.exists lookup (into-array [column-exists-path]))
          column-exists? (.content (.execute value-getter) 0)]
      (if column-exists?
        (let [mutator (.mutateIn content-bucket list-id)
              column-name-path (str +columns-attribute+ "." column-id "." +name-attribute+)
              updater (.upsert mutator column-name-path new-name)]
          (.execute updater)
          list-id)))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (column-name :owner-id $mikelid :list-id $thingsid :column-id "0")
;;; (rename-column $mikelid $thingsid "0" "Thing name")


;;; /delectus/list_items
;;; ---------------------------------------------------------------------

(defn list-items [userid list-id]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)

    (let [list-cbmap (CouchbaseMap. list-id content-bucket)]

      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't inspect list" list-cbmap userid)

      (.get list-cbmap +items-attribute+))))


;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (list-items $mikelid $thingsid)

;;; /delectus/item_with_id
;;; ---------------------------------------------------------------------

(defn item-with-id [owner-id list-id item-id]
  (errors/error-if-nil name "Missing list-id parameter" {:context 'item-with-id})
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context 'item-with-id})
  (errors/error-if-nil item-id "Missing item-id parameter" {:context 'item-with-id})
  (errors/error-if-not (model/user-exists? owner-id)
                       "No such user"
                       {:parameter :owner-id :value owner-id :context 'item-with-id})
  (errors/error-if-not (model/list-exists? list-id)
                       "No such list"
                       {:parameter :list-id :value list-id :context 'item-with-id})
  
  (let [bucket (config/delectus-content-bucket)
        list-cbmap (CouchbaseMap. list-id bucket)]
    
    (errors/error-if-nil list-cbmap "List not found" {:id list-id})
    (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
    (couchio/error-if-wrong-owner "Can't update list" list-cbmap owner-id)
    
    (let [items (get list-cbmap +items-attribute+)]
      (errors/error-if-nil items "List items not found" {:context 'item-with-id})
      (.get items item-id))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (item-with-id $mikelid $thingsid "0")
;;; (item-with-id $mikelid $thingsid "2")

;;; /delectus/new_item
;;; ---------------------------------------------------------------------
;;; TODO: prevent adding an item when there are no columns

(defn new-item [& {:keys [list-id owner-id]
                   :or {name nil
                        list-id nil
                        owner-id nil}}]

  (errors/error-if-nil name "Missing :list-id parameter" {:context 'new-item})
  (errors/error-if-nil owner-id "Missing :owner-id parameter" {:context 'new-item})
  (errors/error-if-not (model/user-exists? owner-id)
                       "No such user"
                       {:parameter :owner-id :value owner-id :context 'new-item})
  (errors/error-if-not (model/list-exists? list-id)
                       "No such list"
                       {:parameter :list-id :value list-id :context 'new-item})
  
  (let [bucket (config/delectus-content-bucket)
        list-cbmap (CouchbaseMap. list-id bucket)]
    
    (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
    (couchio/error-if-wrong-owner "Can't update list" list-cbmap owner-id)
    (let [columns (get list-cbmap +columns-attribute+)
          column-ids (into [] (.getNames columns))]
      (errors/error-if-empty column-ids "Can't create items: no columns" {:id list-id})
      (let [old-rows (get list-cbmap +items-attribute+)
            old-row-ids (into [] (.getNames old-rows))
            new-row-id (itemid/next-itemid old-row-ids)
            fields-map (zipmap column-ids (repeat nil))
            row-obj (model/make-row-object :id new-row-id :fields fields-map)
            mutator (.mutateIn bucket list-id)
            updater (.upsert mutator (str +items-attribute+ "." new-row-id) row-obj)]
        (.execute updater)))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (new-item :owner-id $mikelid :list-id $thingsid)
;;; (.toMap (list-items $mikelid $thingsid))
;;; (def $poodlesid "a0208f05-c9fa-48ac-bd3d-0142d533ec8d")
;;; (list-columns $mikelid $poodlesid)
;;; (list-items $mikelid $poodlesid)
;;; (new-item :owner-id $mikelid :list-id $poodlesid)


;;; /delectus/delete_item
;;; /delectus/undelete_item
;;; ---------------------------------------------------------------------

(defn mark-item-deleted [owner-id list-id item-id deleted?]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)
        new-deleted-value (if deleted? true false)]

    (errors/error-if-nil name "Missing list-id parameter" {:context 'mark-item-deleted})
    (errors/error-if-nil owner-id "Missing owner-id parameter" {:context 'mark-item-deleted})
    (errors/error-if-nil item-id "Missing item-id parameter" {:context 'mark-item-deleted})
    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket owner-id)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)

    (let [list-cbmap (CouchbaseMap. list-id content-bucket)]

      (errors/error-if-nil list-cbmap "List not found" {:id list-id})
      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't update list" list-cbmap owner-id)

      (let [items (get list-cbmap +items-attribute+)]

        (errors/error-if-nil items "List items not found" {:id list-id})

        (let [item (.get items item-id)]
          (errors/error-if-nil item "List item not found" {:id item-id})
          
          (let [mutator (.mutateIn content-bucket list-id)
                updater (.upsert mutator (str +items-attribute+ "." item-id "." +deleted-attribute+) deleted?)]
            (.execute updater))))
      list-id)))


;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (mark-item-deleted $mikelid $thingsid "0" false)
;;; (item-deleted? $mikelid $thingsid "0")
;;; (mark-item-deleted $mikelid $thingsid "1" false)
;;; (item-deleted? $mikelid $thingsid "1")


;;; /delectus/item_deleted
;;; ---------------------------------------------------------------------

(defn item-deleted? [owner-id list-id item-id]
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context 'item-deleted?})
  (errors/error-if-nil list-id "Missing list-id parameter" {:context 'item-deleted?})
  (errors/error-if-nil item-id "Missing item-id parameter" {:context 'item-deleted?})
  (errors/error-if-not (model/user-exists? owner-id)
                       "No such user"
                       {:parameter :owner-id :value owner-id :context 'item-deleted?})
  (errors/error-if-not (model/list-exists? list-id)
                       "No such list"
                       {:parameter :list-id :value list-id :context 'item-deleted?})
  
  (let [bucket (config/delectus-content-bucket)
        list-cbmap (CouchbaseMap. list-id bucket)]
    
    (errors/error-if-nil list-cbmap "List not found" {:id list-id})
    (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
    (couchio/error-if-wrong-owner "Can't update list" list-cbmap owner-id)
    
    (let [items (get list-cbmap +items-attribute+)]
      (errors/error-if-nil items "List items not found" {:context 'item-deleted?})
      (let [item (.get items item-id)]
        (if (nil? item)
          nil
          (.get item +deleted-attribute+))))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (item-deleted? $mikelid $thingsid "0")
;;; (item-deleted? $mikelid $thingsid "1")
;;; (item-deleted? $mikelid $thingsid "101")


;;; /delectus/item_column_value
;;; ---------------------------------------------------------------------
;;; TODO: use errors to handle cases where items and columns within
;;; items are not found

(defn item-column-value [owner-id list-id item-id column-id]
  (errors/error-if-nil owner-id "Missing owner-id parameter" {:context 'item-column-value})
  (errors/error-if-nil list-id "Missing list-id parameter" {:context 'item-column-value})
  (errors/error-if-nil item-id "Missing item-id parameter" {:context 'item-column-value})
  (errors/error-if-not (model/user-exists? owner-id)
                       "No such user"
                       {:parameter :owner-id :value owner-id :context 'item-column-value})
  (errors/error-if-not (model/list-exists? list-id)
                       "No such list"
                       {:parameter :list-id :value list-id :context 'item-column-value})
  
  (let [bucket (config/delectus-content-bucket)
        list-cbmap (CouchbaseMap. list-id bucket)]
    
    (errors/error-if-nil list-cbmap "List not found" {:id list-id})
    (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
    (couchio/error-if-wrong-owner "Can't update list" list-cbmap owner-id)
    
    (let [items (get list-cbmap +items-attribute+)]
      (errors/error-if-nil items "List items not found" {:context 'item-column-value})
      (let [item (.get items item-id)]
        (if (nil? item)
          nil
          (let [fields (.get item +fields-attribute+)]
            (if (nil? fields)
              nil
              (.get fields column-id))))))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))
;;; (item-column-value $mikelid $thingsid "0" "0")

;;; /delectus/set_item_column_value
;;; ---------------------------------------------------------------------

(defn set-item-column-value [owner-id list-id item-id column-id new-value]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]

    (errors/error-if-nil name "Missing list-id parameter" {:context 'set-item-column-value})
    (errors/error-if-nil owner-id "Missing owner-id parameter" {:context 'set-item-column-value})
    (errors/error-if-nil item-id "Missing item-id parameter" {:context 'set-item-column-value})
    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket owner-id)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)

    (let [list-cbmap (CouchbaseMap. list-id content-bucket)]

      (errors/error-if-nil list-cbmap "List not found" {:id list-id})
      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't update list" list-cbmap owner-id)

      (let [items (get list-cbmap +items-attribute+)]

        (errors/error-if-nil items "List items not found" {:id list-id})

        (let [item (.get items item-id)]
          (errors/error-if-nil item "List item not found" {:id item-id})
          
          (let [fields (.get item +fields-attribute+)]
            (errors/error-if-nil fields "Item fields not found" {:list-id list-id :item-id item-id})
            (let [mutator (.mutateIn content-bucket list-id)
                  updater (.upsert mutator
                                   (str +items-attribute+ "." item-id "." +fields-attribute+ "." column-id)
                                   new-value)]
              (.execute updater)))))
      list-id)))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $thingsid (.get (list-named (userid "mikel@evins.net") "Things") "id"))

;;; (set-item-column-value $mikelid $thingsid "0" "0" "Thing One")
;;; (set-item-column-value $mikelid $thingsid "0" "0" "Thing #1")
;;; (item-column-value $mikelid $thingsid "0" "0")
;;; (set-item-column-value $mikelid $thingsid "0" "1" "A helpful thing")
;;; (item-column-value $mikelid $thingsid "0" "1")

;;; (set-item-column-value $mikelid $thingsid "1" "0" "Thing Two")
;;; (item-column-value $mikelid $thingsid "1" "0")
;;; (set-item-column-value $mikelid $thingsid "1" "1" "Another helpful thing")
;;; (item-column-value $mikelid $thingsid "1" "1")

