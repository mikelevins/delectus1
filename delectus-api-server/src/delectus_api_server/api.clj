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

;;; ---------------------------------------------------------------------
;;; users
;;; ---------------------------------------------------------------------

(defn id->user [userid]
  (couchio/get-user userid))

;;; (def $mikelid (email->userid "mikel@evins.net"))
;;; (def $mikel (id->user $mikelid))

;;; ---------------------------------------------------------------------
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

;;; =====================================================================
;;; API-endpoint functions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; admin
;;; ---------------------------------------------------------------------

;;; private admin endpoints

;;; TODO
(defn register-user [email password])
(defn update-user! [userid new-values-map])

;;; ---------------------------------------------------------------------
;;; users & sessions
;;; ---------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

(defn list-collections [userid]
  (couchio/find-objects (config/delectus-content-bucket)
                        ["name" "id"]
                        {+type-attribute+ +collection-type+
                         +owner-id-attribute+ userid}))

;;; (list-collections (email->userid "mikel@evins.net"))

(defn create-collection [& {:keys [id name owner-id]
                            :or {id (makeid)
                                 name nil
                                 owner-id nil}}]
  (couchio/error-if-collection-id-exists id)
  (errors/error-if-nil name "Missing :name parameter" {:context 'create-collection})
  (errors/error-if-nil owner-id "Missing :owner-id parameter" {:context 'create-collection})
  (errors/error-if-nil (id->user owner-id) "No such user" {:parameter :owner-id
                                                           :value owner-id
                                                           :context 'create-collection})
  (errors/error-if (name->collection owner-id name)
                   "Collection name exists" {:parameter :name :value name})

  (let [collection-doc (couchio/make-collection-document :id id :name name :owner-id owner-id)]
    (.upsert (config/delectus-content-bucket) collection-doc)
    id))

;;; (create-collection :id (makeid) :name "Parts" :owner-id (email->userid "mikel@evins.net"))
;;; (create-collection :id (makeid) :name "Stuff" :owner-id (email->userid "nobody@evins.net"))

;;; TODO
(defn mark-collection-deleted [userid collection-id deleted?])

(defn find-collection-by-id [userid collection-id]
  (let [found (couchio/get-collection collection-id)]
    (if (and found (couchio/json-object-owner? found userid))
      found
      nil)))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (find-collection-by-id $mikelid $defaultid)
;;; (def $greerid "6235e7b7-eb83-47d9-a8ef-ac129601e810")
;;; (find-collection-by-id $greerid $defaultid)

(defn find-collection-by-name [userid collection-name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id" "items"]
                                    {+type-attribute+ +collection-type+
                                     +name-attribute+ collection-name
                                     +owner-id-attribute+ userid})]
    (if (empty? found)
      nil
      (first found))))

;;; (find-collection-by-name (email->userid "mikel@evins.net") "Default Collection")
;;; (find-collection-by-name (email->userid "mikel@evins.net") "NOPE!")

;;; TODO
(defn get-collection-name [userid collection-id])

;;; TODO
(defn update-collection-name [userid collection-id new-name])

;;; TODO
(defn get-collection-lists [userid collection-id])

(defn collection-add-list [userid collection-id list-id]
  (let [users-bucket (config/delectus-users-bucket)
        content-bucket (config/delectus-content-bucket)]
    (couchio/error-if-no-such-id "The user doesn't exist" users-bucket userid)
    (couchio/error-if-no-such-id "The collection doesn't exist" content-bucket collection-id)
    (couchio/error-if-no-such-id "The list doesn't exist" content-bucket list-id)

    (let [bucket (config/delectus-content-bucket)
          collection-cbmap (CouchbaseMap. collection-id bucket)
          list-cbmap (CouchbaseMap. list-id bucket)]

      (couchio/error-if-wrong-type "Not a Delectus Collection" collection-cbmap +collection-type+)
      (couchio/error-if-wrong-type "Not a Delectus List" list-cbmap +list-type+)
      (couchio/error-if-wrong-owner "Can't update collection" collection-cbmap userid)
      (couchio/error-if-wrong-owner "Can't update list" list-cbmap userid)
      
      (.get collection-cbmap "lists"))))

;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (def $default (couchio/get-collection $defaultid))
;;; (def $thingsid (.get (find-list-by-name (email->userid "mikel@evins.net") "Things") "id"))
;;; (collection-add-list $mikelid $defaultid $thingsid)


(defn collection-remove-list [userid collection-id list-id]
  )

;;; (def $bucket (config/delectus-content-bucket))
;;; (def $collid (.get (find-collection-by-name (email->userid "mikel@evins.net") "Default Collection") "id"))
;;; (def $coll (couchio/get-document $bucket $collid))
;;; (.toMap (.content $coll))
;;; (def $thingsid (.get (find-list-by-name (email->userid "mikel@evins.net") "Things") "id"))
;;; (def $things (couchio/get-document $bucket $thingsid))
;;; (.toMap (.content $things))
;;; (def $mikelid (email->userid "mikel@evins.net"))
;;; (collection-remove-list $mikelid $collid $thingsid)

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defn list-lists [userid]
  (let [bucket (config/delectus-content-bucket)]
    (couchio/find-objects bucket ["name" "id"]
                          {+type-attribute+ +list-type+
                           +owner-id-attribute+ userid})))

;;; (list-lists (email->userid "mikel@evins.net"))

;;; TODO
(defn create-list [userid name])

;;; TODO
(defn mark-list-deleted [userid list-id])

(defn find-list-by-id [userid list-id]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id"]
                                    {+type-attribute+ +list-type+
                                     +id-attribute+ list-id})]
    (if (empty? found)
      nil
      (first found))))

;;; (def $listid (.get (find-list-by-name (email->userid "mikel@evins.net") "Things") "id"))
;;; (find-list-by-id (email->userid "mikel@evins.net") $listid)

(defn find-list-by-name [userid list-name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id"]
                                    {+type-attribute+ +list-type+
                                     +name-attribute+ list-name})]
    (if (empty? found)
      nil
      (first found))))

;;; (find-list-by-name (email->userid "mikel@evins.net") "Things")
;;; (find-list-by-name (email->userid "mikel@evins.net") "NOPE!")

;;; TODO
(defn list-name [userid list-id])

;;; TODO
(defn update-list-name [userid list-id new-name])

;;; TODO
(defn list-columns [userid list-id])

;;; TODO
(defn find-column-by-id [userid list-id column-id])

;;; TODO
(defn find-column-by-name [userid list-id column-name])

;;; TODO
(defn list-add-column [userid list-id column-name])

;;; TODO
(defn mark-column-deleted [userid list-id column-id])

;;; TODO
(defn column-name [userid list-id column-id])

;;; TODO
(defn update-column-name [userid list-id column-id new-name])

;;; TODO
(defn list-items [userid list-id])

;;; TODO
(defn find-item-by-id [userid list-id item-id])

;;; TODO
(defn list-add-item [userid list-id])

;;; TODO
(defn mark-item-deleted [userid list-id deleted?])

;;; TODO
(defn item-column-value [userid list-id column-id])

;;; TODO
(defn update-item-column-value [userid list-id column-id new-value])
