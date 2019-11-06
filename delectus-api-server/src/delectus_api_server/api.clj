(ns delectus-api-server.api
  (:require
   [buddy.hashers :as hashers]
   [clojure.edn :as edn]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.couchio :as couchio]
   [delectus-api-server.identifiers :refer [makeid]])
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
;;; (def $docid (.get (find-collection-by-name (userid "mikel@evins.net") "Default Collection") "id"))
;;; (def $doc (get-document $bucket $docid))
;;; (assoc (into {} (.toMap (.content $doc))) :test "test value")
;;; (time (get-document $bucket "NOPE!"))

;;; ---------------------------------------------------------------------
;;; Users
;;; ---------------------------------------------------------------------

(defn register-user [email password])

;;; PRIVATE: do not expose to the public API
(defn update-user! [userid new-values-map])

(defn user [email]
  (let [bucket (config/delectus-users-bucket)
        found (couchio/find-objects bucket []
                                    {"type" "delectus_user"
                                     "email" email})]
    (if (empty? found)
      nil
      (first found))))

;;; (user "mikel@evins.net")
;;; (user "greer@evins.net")
;;; (user "nobody@nowhere.net")

(defn userid [email]
  (let [found-user (user email)]
    (if found-user
      (.get found-user "id")
      nil)))

;;; (userid "mikel@evins.net")
;;; (userid "greer@evins.net")
;;; (userid "nobody@nowhere.net")

;;; ---------------------------------------------------------------------
;;; Collections
;;; ---------------------------------------------------------------------

(defn list-collections [userid]
  (let [bucket (config/delectus-content-bucket)]
    (couchio/find-objects bucket ["name" "id"]
                          {"type" "delectus_collection"
                           "owner-id" userid})))

;;; (list-collections (userid "mikel@evins.net"))

(defn create-collection [& {:keys [id name owner-id]
                            :or {id (makeid)
                                 name nil
                                 owner-id nil}}])

(defn mark-collection-deleted [collection-id deleted?])

(defn find-collection-by-id [userid collection-id]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id"]
                                    {"type" "delectus_collection"
                                     "id" collection-id})]
    (if (empty? found)
      nil
      (first found))))

;;; (def $coll (find-collection-by-name (userid "mikel@evins.net") "Default Collection"))
;;; (def $collid (.get $coll "id"))
;;; (find-collection-by-id (userid "mikel@evins.net") $collid)

(defn find-collection-by-name [userid collection-name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id"]
                                    {"type" "delectus_collection"
                                     "name" collection-name})]
    (if (empty? found)
      nil
      (first found))))

;;; (find-collection-by-name (userid "mikel@evins.net") "Default Collection")
;;; (find-collection-by-name (userid "mikel@evins.net") "NOPE!")

(defn get-collection-name [userid collection-id])

(defn update-collection-name [userid collection-id new-name])

(defn get-collection-lists [userid collection-id])

(defn collection-add-list [userid collection-id list-id]
  (let [bucket (config/delectus-content-bucket)
        collection-doc (get-document bucket collection-id)
        list-doc (get-document bucket list-id)]

    ;; make sure the list and collection actually exist
    (if (nil? collection-doc) (throw (ex-info "No such collection" (ex-info {:id collection-id}))))
    (if (nil? list-doc) (throw (ex-info "No such list" (ex-info {:id list-id}))))

    (let [found-collection (.content collection-doc)
          collection-ownerid (.get found-collection "owner-id")
          found-list (.content list-doc)
          list-ownerid (.get found-list "owner-id")]

      ;; make sure the user owns the list and collection
      (if-not (= userid collection-ownerid)
        (throw (ex-info "Cannot update collection" (ex-info {:reason "wrong collection owner"}))))
      (if-not (= userid list-ownerid)
        (throw (ex-info "Cannot update list" (ex-info {:reason "wrong list owner"}))))

      ;; prepare to add the list to the collection
      (let [old-collection-map (into {} (.toMap found-collection))
            old-collection-items (into {} (get old-collection-map "items"))
            old-collection-indexes (map edn/read-string (keys old-collection-items))
            new-index (if (empty? old-collection-indexes) 0 (+ 1 (apply max old-collection-indexes)))
            new-list-id (.get found-list "id")]

        ;; don't add the list if it's already in the collection
        (if (some #{new-list-id} (vals old-collection-items))
          collection-id
          (do (let [new-collection-items (merge old-collection-items {(str new-index) new-list-id})
                    new-collection-map (merge old-collection-map {"items" new-collection-items})
                    new-collection-doc (JsonDocument/create collection-id (JsonObject/from new-collection-map))]
                (.upsert bucket new-collection-doc))
              collection-id))))))

;;; (def $bucket (config/delectus-content-bucket))
;;; (def $collid (.get (find-collection-by-name (userid "mikel@evins.net") "Default Collection") "id"))
;;; (def $coll (get-document $bucket $collid))
;;; (.toMap (.content $coll))
;;; (def $thingsid (.get (find-list-by-name (userid "mikel@evins.net") "Things") "id"))
;;; (def $things (get-document $bucket $thingsid))
;;; (.toMap (.content $things))
;;; (def $mikelid (userid "mikel@evins.net"))
;;; (collection-add-list $mikelid $collid $thingsid)

(defn collection-remove-list [userid collection-id list-id]
  (let [bucket (config/delectus-content-bucket)
        collection-doc (get-document bucket collection-id)
        list-doc (get-document bucket list-id)]

    ;; make sure the list and collection actually exist
    (if (nil? collection-doc) (throw (ex-info "No such collection" (ex-info {:id collection-id}))))
    (if (nil? list-doc) (throw (ex-info "No such list" (ex-info {:id list-id}))))

    (let [found-collection (.content collection-doc)
          collection-ownerid (.get found-collection "owner-id")
          found-list (.content list-doc)
          list-ownerid (.get found-list "owner-id")]

      ;; make sure the user owns the list and collection
      (if-not (= userid collection-ownerid)
        (throw (ex-info "Cannot update collection" (ex-info {:reason "wrong collection owner"}))))
      (if-not (= userid list-ownerid)
        (throw (ex-info "Cannot update list" (ex-info {:reason "wrong list owner"}))))

      ;; prepare to remove the list from the collection
      (let [old-collection-map (into {} (.toMap found-collection))
            old-collection-items (into {} (get old-collection-map "items"))]

        ;; don't remove the list if it's not in the collection
        (if (some #{list-id} (vals old-collection-items))
          (do (let [new-collection-items (into {} (filter #(not (= list-id (second %))))
                                               old-collection-items)
                    new-collection-map (merge old-collection-map {"items" new-collection-items})
                    new-collection-doc (JsonDocument/create collection-id (JsonObject/from new-collection-map))]
                (.upsert bucket new-collection-doc))
              collection-id)
          collection-id)))))

;;; (def $bucket (config/delectus-content-bucket))
;;; (def $collid (.get (find-collection-by-name (userid "mikel@evins.net") "Default Collection") "id"))
;;; (def $coll (get-document $bucket $collid))
;;; (.toMap (.content $coll))
;;; (def $thingsid (.get (find-list-by-name (userid "mikel@evins.net") "Things") "id"))
;;; (def $things (get-document $bucket $thingsid))
;;; (.toMap (.content $things))
;;; (def $mikelid (userid "mikel@evins.net"))
;;; (collection-remove-list $mikelid $collid $thingsid)

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defn list-lists [userid]
  (let [bucket (config/delectus-content-bucket)]
    (couchio/find-objects bucket ["name" "id"]
                          {"type" "delectus_list"
                           "owner-id" userid})))

;;; (list-lists (userid "mikel@evins.net"))

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

;;; (def $listid (.get (find-list-by-name (userid "mikel@evins.net") "Things") "id"))
;;; (find-list-by-id (userid "mikel@evins.net") $listid)

(defn find-list-by-name [userid list-name]
  (let [bucket (config/delectus-content-bucket)
        found (couchio/find-objects bucket ["name" "id"]
                                    {"type" "delectus_list"
                                     "name" list-name})]
    (if (empty? found)
      nil
      (first found))))

;;; (find-list-by-name (userid "mikel@evins.net") "Things")
;;; (find-list-by-name (userid "mikel@evins.net") "NOPE!")

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
