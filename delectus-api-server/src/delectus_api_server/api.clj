(ns delectus-api-server.api
  (:require
   [buddy.hashers :as hashers]
   [clojure.edn :as edn]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.couchio :as couchio]
   [delectus-api-server.errors :as errors]
   [delectus-api-server.identifiers :refer [makeid]]
   [delectus-api-server.utilities :as utils])
  (:import
   (com.couchbase.client.java.document.json JsonObject)
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; Couchbase helpers
;;; ---------------------------------------------------------------------

(defn get-document [bucket docid]
  (.get bucket docid))

;;; (def $bucket (config/delectus-content-bucket))
;;; (def $docid (.get (find-collection-by-name (email->userid "mikel@evins.net") "Default Collection") "id"))
;;; (def $doc (get-document $bucket $docid))
;;; (assoc (into {} (.toMap (.content $doc))) :test "test value")
;;; (time (get-document $bucket "NOPE!"))

;;; ---------------------------------------------------------------------
;;; Users
;;; ---------------------------------------------------------------------

(defn register-user [email password])

;;; PRIVATE: do not expose to the public API
(defn update-user! [userid new-values-map])

(defn id->user [userid]
  (let [candidate-doc (get-document (config/delectus-users-bucket) userid)]
    (if (nil? candidate-doc)
      nil
      (let [obj (.content candidate-doc)]
        (if (= "delectus_user" (.get obj "type"))
          obj
          nil)))))

;;; (def $mikelid (email->userid "mikel@evins.net"))
;;; (def $mikel (id->user $mikelid))

(defn email->user [email]
  (let [bucket (config/delectus-users-bucket)
        found (couchio/find-objects bucket []
                                    {"type" "delectus_user"
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

(defn id->collection [collection-id]
  (let [candidate-doc (get-document (config/delectus-users-bucket) collection-id)]
    (if (nil? candidate-doc)
      nil
      (let [obj (.content candidate-doc)]
        (if (= "delectus_collection" (.get obj "type"))
          obj
          nil)))))

(defn name->collection [name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket []
                                    {"type" "delectus_collection"
                                     "name" name})]
    (if (empty? found)
      nil
      (first found))))

(defn list-collections [userid]
  (let [bucket (config/delectus-content-bucket)]
    (couchio/find-objects bucket ["name" "id"]
                          {"type" "delectus_collection"
                           "owner-id" userid})))

;;; (list-collections (email->userid "mikel@evins.net"))

(defn create-collection [& {:keys [id name owner-id]
                            :or {id (makeid)
                                 name nil
                                 owner-id nil}}]
  (let [found (get-document (config/delectus-content-bucket) id)]
    (if found
      (errors/error "Document exists" {:id id :type (.get (.content found) "type")})))
  (errors/error-if-nil name "name parameter is required" {:missing :name})
  (errors/error-if-nil owner-id "owner-id parameter is required" {:missing :owner-id})
  (errors/error-if-nil (id->user owner-id) "No such user" {:id owner-id})
  (errors/error-if (name->collection name) "Collection exists" {:name name})

  (let [collection-map {"type" "delectus_collection"
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
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id" "items"]
                                    {"type" "delectus_collection"
                                     "id" collection-id})]
    (if (empty? found)
      nil
      (first found))))

;;; (def $coll (find-collection-by-name (email->userid "mikel@evins.net") "Default Collection"))
;;; (def $collid (.get $coll "id"))
;;; (find-collection-by-id (email->userid "mikel@evins.net") $collid)

(defn find-collection-by-name [userid collection-name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id" "items"]
                                    {"type" "delectus_collection"
                                     "name" collection-name})]
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
        collection-doc (get-document bucket collection-id)
        list-doc (get-document bucket list-id)]

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
;;; (def $coll (get-document $bucket $collid))
;;; (.toMap (.content $coll))
;;; (def $thingsid (.get (find-list-by-name (email->userid "mikel@evins.net") "Things") "id"))
;;; (def $things (get-document $bucket $thingsid))
;;; (.toMap (.content $things))
;;; (def $mikelid (email->userid "mikel@evins.net"))
;;; (collection-add-list $mikelid $collid $thingsid)

(defn collection-remove-list [userid collection-id list-id]
  (let [bucket (config/delectus-content-bucket)
        collection-doc (get-document bucket collection-id)
        list-doc (get-document bucket list-id)]

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
;;; (def $coll (get-document $bucket $collid))
;;; (.toMap (.content $coll))
;;; (def $thingsid (.get (find-list-by-name (email->userid "mikel@evins.net") "Things") "id"))
;;; (def $things (get-document $bucket $thingsid))
;;; (.toMap (.content $things))
;;; (def $mikelid (email->userid "mikel@evins.net"))
;;; (collection-remove-list $mikelid $collid $thingsid)

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defn list-lists [userid]
  (let [bucket (config/delectus-content-bucket)]
    (couchio/find-objects bucket ["name" "id"]
                          {"type" "delectus_list"
                           "owner-id" userid})))

;;; (list-lists (email->userid "mikel@evins.net"))

(defn create-list [userid name])

(defn mark-list-deleted [userid list-id])

(defn find-list-by-id [userid list-id]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id"]
                                    {"type" "delectus_list"
                                     "id" list-id})]
    (if (empty? found)
      nil
      (first found))))

;;; (def $listid (.get (find-list-by-name (email->userid "mikel@evins.net") "Things") "id"))
;;; (find-list-by-id (email->userid "mikel@evins.net") $listid)

(defn find-list-by-name [userid list-name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id"]
                                    {"type" "delectus_list"
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
