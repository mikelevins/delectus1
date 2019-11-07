(ns delectus-api-server.api
  (:require
   [buddy.hashers :as hashers]
   [clojure.edn :as edn]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.constants :as constants]
   [delectus-api-server.couchio :as couchio]
   [delectus-api-server.errors :as errors]
   [delectus-api-server.identifiers :refer [makeid]]
   [delectus-api-server.utilities :as utils])
  (:import
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

(defn name->collection [name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket []
                                    {"type" constants/+delectus-collection-document-type+
                                     "name" name})]
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

(defn register-user [email password])
(defn update-user! [userid new-values-map])

;;; ---------------------------------------------------------------------
;;; users & sessions
;;; ---------------------------------------------------------------------

(defn email->user [email]
  (let [bucket (config/delectus-users-bucket)
        found (couchio/find-objects bucket []
                                    {"type" constants/+delectus-user-document-type+
                                     "email" email})]
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
  (let [bucket (config/delectus-content-bucket)]
    (couchio/find-objects bucket ["name" "id"]
                          {"type" constants/+delectus-collection-document-type+
                           "owner-id" userid})))

;;; (list-collections (email->userid "mikel@evins.net"))

(defn create-collection [& {:keys [id name owner-id]
                            :or {id (makeid)
                                 name nil
                                 owner-id nil}}]
  (couchio/error-if-collection-id-exists id)
  (errors/error-if-nil name "name parameter is required" {:missing :name})
  (errors/error-if-nil owner-id "owner-id parameter is required" {:missing :owner-id})
  (errors/error-if-nil (id->user owner-id) "No such user" {:id owner-id})
  (errors/error-if (name->collection name) "Collection exists" {:name name})

  (let [collection-map {"type" constants/+delectus-collection-document-type+
                        "id" id
                        "name" name
                        "owner-id" owner-id
                        "items" {}}
        collection-doc (JsonDocument/create id (JsonObject/from collection-map))]
    (.upsert (config/delectus-content-bucket)
             collection-doc)
    id))

;;; (create-collection :id (makeid) :name "Stuff" :owner-id (email->userid "mikel@evins.net"))

(defn mark-collection-deleted [collection-id deleted?])

(defn find-collection-by-id [userid collection-id]
  (let [found (couchio/get-collection collection-id)]
    (or (and found
             (couchio/json-object-owner? found userid)
             found)
        nil)))

;;; (def $defaultid "b8b933f2-1eb0-4d7d-9ecd-a221efb6ced5")
;;; (def $mikelid "5d7f805d-5712-4e8b-bdf1-6e24cf4fe06f")
;;; (find-collection-by-id $mikelid $defaultid)
;;; (def $greerid "6235e7b7-eb83-47d9-a8ef-ac129601e810")
;;; (find-collection-by-id $greerid $defaultid)

(defn find-collection-by-name [userid collection-name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id" "items"]
                                    {"type" constants/+delectus-collection-document-type+
                                     "name" collection-name
                                     "owner-id" userid})]
    (if (empty? found)
      nil
      (first found))))

;;; (find-collection-by-name (email->userid "mikel@evins.net") "Default Collection")
;;; (find-collection-by-name (email->userid "mikel@evins.net") "NOPE!")

(defn get-collection-name [userid collection-id])

(defn update-collection-name [userid collection-id new-name])

(defn get-collection-lists [userid collection-id])

(defn collection-add-list [userid collection-id list-id]
  (let [bucket (config/delectus-content-bucket)
        collection-doc (couchio/get-document bucket collection-id)
        list-doc (couchio/get-document bucket list-id)]

    ;; make sure the list and collection actually exist
    (errors/error-if-nil collection-doc "No such collection" {:id collection-id})
    (errors/error-if-nil list-doc "No such list" {:id list-id})

    (let [found-collection (.content collection-doc)
          collection-ownerid (.get found-collection "owner-id")
          found-list (.content list-doc)
          list-ownerid (.get found-list "owner-id")]

      ;; make sure the user owns the list and collection
      (errors/error-if-not (= userid collection-ownerid) "Cannot update collection" {:reason "wrong collection owner"})
      (errors/error-if-not (= userid list-ownerid) "Cannot update list" {:reason "wrong list owner"})

      ;; prepare to add the list to the collection
      (let [old-collection-items (.get found-collection "items")
            found-property (couchio/find-json-object-key-for-value old-collection-items list-id)]
        (if found-property
          ;; it's already present; no need to add it
          collection-id
          ;; didn;t find it; better add it
          (let [old-collection-map (into {} (.toMap found-collection))
                old-collection-indexes (map edn/read-string (into [] (.getNames old-collection-items)))
                new-index (if (empty? old-collection-indexes)
                            (str 0)
                            (str (+ 1 (apply max old-collection-indexes))))
                new-list-id (.get found-list "id")
                new-collection-items (couchio/put-key-if-changed old-collection-items new-index new-list-id)
                new-collection-map (merge old-collection-map {"items" new-collection-items})
                new-collection-doc (JsonDocument/create collection-id (JsonObject/from new-collection-map))]
            (.upsert bucket new-collection-doc)
            collection-id))))))

;;; (def $bucket (config/delectus-content-bucket))
;;; (def $collid (.get (find-collection-by-name (email->userid "mikel@evins.net") "Default Collection") "id"))
;;; (def $coll (couchio/get-document $bucket $collid))
;;; (.toMap (.content $coll))
;;; (def $thingsid (.get (find-list-by-name (email->userid "mikel@evins.net") "Things") "id"))
;;; (def $things (couchio/get-document $bucket $thingsid))
;;; (.toMap (.content $things))
;;; (def $mikelid (email->userid "mikel@evins.net"))
;;; (collection-add-list $mikelid $collid $thingsid)

(defn collection-remove-list [userid collection-id list-id]
  (let [bucket (config/delectus-content-bucket)
        collection-doc (couchio/get-document bucket collection-id)
        list-doc (couchio/get-document bucket list-id)]

    ;; make sure the list and collection actually exist
    (errors/error-if-nil collection-doc "No such collection" {:id collection-id})
    (errors/error-if-nil list-doc "No such list" {:id list-id})

    (let [found-collection (.content collection-doc)
          collection-ownerid (.get found-collection "owner-id")
          found-list (.content list-doc)
          list-ownerid (.get found-list "owner-id")]

      ;; make sure the user owns the list and collection
      (errors/error-if-not (= userid collection-ownerid) "Cannot update collection" {:reason "wrong collection owner"})
      (errors/error-if-not (= userid list-ownerid) "Cannot update list" {:reason "wrong list owner"})

      ;; prepare to remove the list from the collection
      (let [items (.get found-collection "items") ; a JsonObject
            items-map (into {} (.toMap items))
            found-key (utils/find-map-key-for-value items-map list-id)]
        (if found-key
          ;; remove the list-id
          (let [new-items-map (dissoc items-map found-key)
                new-collection-map (merge (into {} (.toMap found-collection))
                                          {"items" new-items-map})
                new-collection-object (JsonObject/from new-collection-map)
                new-collection-doc (JsonDocument/create collection-id new-collection-object)]
            (.upsert bucket new-collection-doc)
            collection-id)
          ;; the list isn't in the collection
          collection-id)))))

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
                          {"type" constants/+delectus-list-document-type+
                           "owner-id" userid})))

;;; (list-lists (email->userid "mikel@evins.net"))

(defn create-list [userid name])

(defn mark-list-deleted [userid list-id])

(defn find-list-by-id [userid list-id]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id"]
                                    {"type" constants/+delectus-list-document-type+
                                     "id" list-id})]
    (if (empty? found)
      nil
      (first found))))

;;; (def $listid (.get (find-list-by-name (email->userid "mikel@evins.net") "Things") "id"))
;;; (find-list-by-id (email->userid "mikel@evins.net") $listid)

(defn find-list-by-name [userid list-name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id"]
                                    {"type" constants/+delectus-list-document-type+
                                     "name" list-name})]
    (if (empty? found)
      nil
      (first found))))

;;; (find-list-by-name (email->userid "mikel@evins.net") "Things")
;;; (find-list-by-name (email->userid "mikel@evins.net") "NOPE!")

(defn list-name [userid list-id])

(defn update-list-name [userid list-id new-name])

(defn list-columns [userid list-id])

(defn find-column-by-id [userid list-id column-id])

(defn find-column-by-name [userid list-id column-name])

(defn list-add-column [userid list-id column-name])

(defn mark-column-deleted [userid list-id column-id])

(defn column-name [userid list-id column-id])

(defn update-column-name [userid list-id column-id new-name])

(defn list-items [userid list-id])

(defn find-item-by-id [userid list-id item-id])

(defn list-add-item [userid list-id])

(defn mark-item-deleted [userid list-id deleted?])

(defn item-column-value [userid list-id column-id])

(defn update-item-column-value [userid list-id column-id new-value])
