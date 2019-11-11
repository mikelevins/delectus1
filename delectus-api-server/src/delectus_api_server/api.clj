(ns delectus-api-server.api
  (:require
   [buddy.hashers :as hashers]
   [clojure.edn :as edn]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.constants :refer :all]
   [delectus-api-server.couchio :as couchio]
   [delectus-api-server.errors :as errors]
   [delectus-api-server.identifiers :refer [makeid]]
   [delectus-api-server.utilities :as utils])
  (:import
   (com.couchbase.client.java.datastructures.collections CouchbaseArrayList CouchbaseMap)
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)))

;;; =====================================================================
;;; support functions
;;; =====================================================================

;;; users
;;; ---------------------------------------------------------------------

(defn id->user [userid]
  (couchio/get-user userid))

;;; (def $mikelid (email->userid "mikel@evins.net"))
;;; (def $mikel (id->user $mikelid))

(defn email->user [email]
  (let [found (couchio/find-objects
               (config/delectus-users-bucket) []
               {+type-attribute+ +user-type+
                +email-attribute+ email})]
    (if (empty? found)
      nil
      (first found))))


;;; (email->user "mikel@evins.net")
;;; (email->user "greer@evins.net")
;;; (email->user "nobody@nowhere.net")

(defn email->userid [email]
  (let [found-user (email->user email)]
    (if found-user
      (.get found-user "id")
      nil)))

;;; (email->userid "mikel@evins.net")
;;; (email->userid "greer@evins.net")
;;; (email->userid "nobody@nowhere.net")


;;; collections
;;; ---------------------------------------------------------------------

(defn id->collection [collection-id]
  (couchio/get-collection collection-id))

(defn name->collection [userid name]
  (let [found (couchio/find-objects
               (config/delectus-content-bucket) []
               {+type-attribute+ +collection-type+
                +owner-id-attribute+ userid
                +name-attribute+ name})]
    (if (empty? found)
      nil
      (first found))))

;;; lists
;;; ---------------------------------------------------------------------

(defn id->list [list-id]
  (couchio/get-list list-id))

(defn name->list [userid name]
  (let [found (couchio/find-objects
               (config/delectus-content-bucket) []
               {+type-attribute+ +list-type+
                +owner-id-attribute+ userid
                +name-attribute+ name})]
    (if (empty? found)
      nil
      (first found))))


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
  (let [found-user (email->user email)]
    (if found-user
      (if (hashers/check password (.get found-user "password-hash"))
        found-user
        false)
      false)))

;;; /delectus/userid
(defn userid [email] (email->userid email))

;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

;;; /delectus/collections
;;; ---------------------------------------------------------------------

(defn collections [userid]
  (couchio/find-objects (config/delectus-content-bucket)
                        ["name" "id"]
                        {+type-attribute+ +collection-type+
                         +owner-id-attribute+ userid}))

;;; (collections (email->userid "mikel@evins.net"))

;;; /delectus/collection_with_id
;;; ---------------------------------------------------------------------

(defn collection-with-id [userid collection-id]
  (let [found (couchio/get-collection collection-id)]
    (if (and found (couchio/json-object-owner? found userid))
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

;;; (collection-named (email->userid "mikel@evins.net") "Default Collection")
;;; (collection-named (email->userid "mikel@evins.net") "NOPE!")


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
;;; (collections (email->userid "mikel@evins.net"))
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
  (errors/error-if-nil (id->user owner-id)
                       "No such user"
                       {:parameter :owner-id :value owner-id :context 'new-collection})
  (errors/error-if (name->collection owner-id name)
                   "Collection name exists"
                   {:parameter :name :value name})

  (let [collection-doc (couchio/make-collection-document
                        :id id
                        :name name
                        :owner-id owner-id)]

    (.upsert (config/delectus-content-bucket)
             collection-doc)
    id))

;;; (new-collection :id (makeid) :name "Parts" :owner-id (email->userid "mikel@evins.net"))
;;; (new-collection :id (makeid) :name "Stuff" :owner-id (email->userid "nobody@evins.net"))


;;; /delectus/delete_collection
;;; /delectus/undelete_collection
;;; ---------------------------------------------------------------------

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
;;; (def $collid (.get (collection-named (email->userid "mikel@evins.net") "Default Collection") "id"))
;;; (def $coll (couchio/get-document $bucket $collid))
;;; (.toMap (.content $coll))
;;; (def $thingsid (.get (list-named (email->userid "mikel@evins.net") "Things") "id"))
;;; (def $things (couchio/get-document $bucket $thingsid))
;;; (.toMap (.content $things))
;;; (def $mikelid (email->userid "mikel@evins.net"))
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

;;; (lists (email->userid "mikel@evins.net"))

;;; /delectus/list_with_id
;;; ---------------------------------------------------------------------

(defn list-with-id [userid list-id]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id" "deleted"]
                                    {+type-attribute+ +list-type+
                                     +id-attribute+ list-id})]
    (if (empty? found)
      nil
      (first found))))

;;; (def $listid (.get (find-list-by-name (email->userid "mikel@evins.net") "Things") "id"))
;;; (find-list-by-id (email->userid "mikel@evins.net") $listid)

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

;;; (find-list-by-name (email->userid "mikel@evins.net") "Things")
;;; (find-list-by-name (email->userid "mikel@evins.net") "NOPE!")

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
;;; (lists (email->userid "mikel@evins.net"))
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
  (errors/error-if-nil (id->user owner-id)
                       "No such user"
                       {:parameter :owner-id :value owner-id :context 'new-list})
  (errors/error-if (name->list owner-id name)
                   "List name exists"
                   {:parameter :name :value name})

  (let [list-doc (couchio/make-list-document :id id
                                             :name name
                                             :owner-id owner-id)]

    (.upsert (config/delectus-content-bucket)
             list-doc)
    id))

;;; (new-list :id (makeid) :name "Stuff" :owner-id (email->userid "mikel@evins.net"))

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

;;; TODO
;;; /delectus/list_columns
;;; ---------------------------------------------------------------------

(defn list-columns [userid list-id])

;;; TODO
;;; /delectus/column_with_id
;;; ---------------------------------------------------------------------

(defn column-with-id [userid list-id column-id])

;;; TODO
;;; /delectus/column_named
;;; ---------------------------------------------------------------------

(defn column-named [userid list-id column-name])

;;; TODO
;;; /delectus/new_column
;;; ---------------------------------------------------------------------

(defn new-column [userid list-id column-name])

;;; TODO
;;; /delectus/delete_column
;;; /delectus/undelete_column
;;; ---------------------------------------------------------------------

(defn mark-column-deleted [userid list-id column-id])

;;; TODO
;;; /delectus/column_name
;;; ---------------------------------------------------------------------

(defn column-name [userid list-id column-id])

;;; TODO
;;; /delectus/rename_column
;;; ---------------------------------------------------------------------

(defn rename-column [userid list-id column-id new-name])

;;; TODO
;;; /delectus/list_items
;;; ---------------------------------------------------------------------

(defn list-items [userid list-id])

;;; TODO
;;; /delectus/item_with_id
;;; ---------------------------------------------------------------------

(defn item-with-id [userid list-id item-id])

;;; TODO
;;; /delectus/new_item
;;; ---------------------------------------------------------------------

(defn new-item [userid list-id])

;;; TODO
;;; /delectus/delete_item
;;; /delectus/undelete_item
;;; ---------------------------------------------------------------------

(defn mark-item-deleted [userid list-id deleted?])

;;; TODO
;;; /delectus/item_column_value
;;; ---------------------------------------------------------------------

(defn item-column-value [userid list-id column-id])

;;; TODO
;;; /delectus/set_item_column_value
;;; ---------------------------------------------------------------------

(defn set-item-column-value [userid list-id column-id new-value])
