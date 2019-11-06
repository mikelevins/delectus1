(ns delectus-api-server.api
  (:require
   [buddy.hashers :as hashers]
   [clojure.edn :as edn]
   [delectus-api-server.identifiers :refer [makeid]]
   [delectus-api-server.configuration :as config])
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
        bucket-name (.name bucket)
        selector (str (str "SELECT * from `" bucket-name "` ")
                      (str "WHERE `type` = \"delectus_user\" ")
                      (str "AND `email` = \"" email "\""))
        results (.query bucket (N1qlQuery/simple selector))
        objs (map #(.get (.value %) bucket-name) results)
        found-user (if (empty? objs) nil (first objs))]
    (or found-user nil)))

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
  (let [bucket (config/delectus-content-bucket)
        bucket-name (.name bucket)
        select-expression (str (str "SELECT id,name from `" bucket-name "` ")
                               (str "WHERE type = \"delectus_collection\" ")
                               (str "AND `owner-id` = \"" userid "\""))
        results (.query bucket (N1qlQuery/simple select-expression))]
    (map #(.value %) results)))

;;; (list-collections (userid "mikel@evins.net"))

(defn create-collection [& {:keys [id name owner-id]
                            :or {id (makeid)
                                 name nil
                                 owner-id nil}}])

(defn mark-collection-deleted [collection-id deleted?])

(defn find-collection-by-id [userid collection-id]
  (let [bucket (config/delectus-content-bucket)
        bucket-name (.name bucket)
        select-expression (str (str "SELECT name,id,items from `" bucket-name "` ")
                               (str "WHERE type = \"delectus_collection\" ")
                               (str "AND `owner-id` = \"" userid "\" ")
                               (str "AND `id` = \"" collection-id "\""))
        results (.query bucket (N1qlQuery/simple select-expression))
        objects (map #(.value %) results)]
    (if (empty? objects)
      nil
      (first objects))))

(defn find-collection-by-name [userid collection-name]
  (let [bucket (config/delectus-content-bucket)
        bucket-name (.name bucket)
        select-expression (str (str "SELECT name,id,items from `" bucket-name "` ")
                               (str "WHERE type = \"delectus_collection\" ")
                               (str "AND `owner-id` = \"" userid "\" ")
                               (str "AND `name` = \"" collection-name "\""))
        results (.query bucket (N1qlQuery/simple select-expression))
        objects (map #(.value %) results)]
    (if (empty? objects)
      nil
      (first objects))))

;;; (find-collection-by-name (userid "mikel@evins.net") "Default Collection")
;;; (find-collection-by-name (userid "mikel@evins.net") "NOPE!")

(defn get-collection-name [userid collection-id])

(defn update-collection-name [userid collection-id new-name])

(defn get-collection-lists [userid collection-id])

(defn collection-add-list [userid collection-id list-id]
  (let [bucket (config/delectus-content-bucket)
        collection-doc (get-document bucket collection-id)
        list-doc (get-document bucket list-id)]

    (if (nil? collection-doc) (throw (ex-info "No such collection" (ex-info {:id collection-id}))))
    (if (nil? list-doc) (throw (ex-info "No such list" (ex-info {:id list-id}))))

    (let [found-collection (.content collection-doc)
          collection-ownerid (.get found-collection "owner-id")
          found-list (.content list-doc)
          list-ownerid (.get found-list "owner-id")]
      
      (if-not (= userid collection-ownerid)
        (throw (ex-info "Cannot update collection" (ex-info {:reason "wrong collection owner"}))))
      (if-not (= userid list-ownerid)
        (throw (ex-info "Cannot update list" (ex-info {:reason "wrong list owner"}))))

      (let [old-collection-map (into {} (.toMap found-collection))
            old-collection-items (into {} (get old-collection-map "items"))
            old-collection-indexes (map edn/read-string (keys old-collection-items))
            new-index (if (empty? old-collection-indexes) 0 (+ 1 (apply max old-collection-indexes)))
            new-list-id (.get found-list "id")]

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

(defn collection-remove-list [userid collection-id list-id])

;;; ---------------------------------------------------------------------
;;; Lists
;;; ---------------------------------------------------------------------

(defn list-lists [userid]
  (let [bucket (config/delectus-content-bucket)
        bucket-name (.name bucket)
        selector (str (str "SELECT id,name from `" bucket-name "` ")
                      (str "WHERE type = \"delectus_list\" ")
                      (str "AND `owner-id` =\"" userid "\"")) 
        results (.query bucket (N1qlQuery/simple selector))]
    (map #(.value %) results)))

;;; (list-lists (userid "mikel@evins.net"))

(defn create-list [userid name])

(defn mark-list-deleted [userid list-id])

(defn find-list-by-id [userid list-id]
  (let [bucket (config/delectus-content-bucket)
        bucket-name (.name bucket)
        select-expression (str (str "SELECT name,id,items from `" bucket-name "` ")
                               (str "WHERE type = \"delectus_list\" ")
                               (str "AND `owner-id` = \"" userid "\" ")
                               (str "AND `id` = \"" list-id "\""))
        results (.query bucket (N1qlQuery/simple select-expression))
        objects (map #(.value %) results)]
    (if (empty? objects)
      nil
      (first objects))))

;;; (def $listid (.get (find-list-by-name (userid "mikel@evins.net") "Things") "id"))
;;; (find-list-by-id (userid "mikel@evins.net") $listid)

(defn find-list-by-name [userid list-name]
  (let [bucket (config/delectus-content-bucket)
        bucket-name (.name bucket)
        select-expression (str (str "SELECT name,id,items from `" bucket-name "` ")
                               (str "WHERE type = \"delectus_list\" ")
                               (str "AND `owner-id` = \"" userid "\" ")
                               (str "AND `name` = \"" list-name "\""))
        results (.query bucket (N1qlQuery/simple select-expression))
        objects (map #(.value %) results)]
    (if (empty? objects)
      nil
      (first objects))))

;;; (find-list-by-name (userid "mikel@evins.net") "Things")

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
