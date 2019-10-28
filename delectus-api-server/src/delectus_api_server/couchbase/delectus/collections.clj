(ns delectus-api-server.couchbase.delectus.collections
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.couchbase.marshal
             :refer [Couchable JsonDocumentable JsonObjectable
                     make-couchable to-json-document to-json-object to-map]]
            [delectus-api-server.couchbase.delectus.users :as delectus-users]
            [delectus-api-server.couchbase.delectus.identifiable :refer [Identifiable get-id]]
            [delectus-api-server.couchbase.delectus.itemizing
             :refer [Itemizing add-item get-items item-at max-item-index remove-item-at update-items upsert-item-at]]
            [delectus-api-server.couchbase.delectus.typable :refer [Typable get-type]]
            [delectus-api-server.couchbase.delectus.nameable :refer [Nameable get-name rename]]
            [delectus-api-server.couchbase.delectus.ownable :refer [Ownable get-owner-id update-owner-id]])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; Collection
;;; ---------------------------------------------------------------------

(defn the-collection-document-type [] "delectus_collection")

(defrecord Collection [id type name owner-id items]
  Identifiable
  (get-id [data] (:id data))

  Typable
  (get-type [data] (:type data))

  Nameable
  (get-name [data] (:name data))
  (rename [data new-name] (map->Collection (merge data {:name new-name})))

  Ownable
  (get-owner-id [data] (:owner-id data))
  (update-owner-id [data new-owner-id] (map->Collection (merge data {:owner-id new-owner-id})))

  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs))))

  Itemizing
  (add-item [data item]
    (let [max-index (max-item-index data)
          new-index (if max-index (+ 1 max-index) 0)]
      (upsert-item-at data new-index item)))
  (get-items [data](:items data))
  (item-at [data index] (get (:items data) index))
  (max-item-index [data]
    (if (empty? (get-items data))
      nil
      (apply max (keys (get-items data)))))
  (remove-item-at [data index]
    (update-items data
                  (dissoc (get-items data)
                          index)))
  (update-items [data new-items]
    (map->Collection (merge data {:items new-items})))
  (upsert-item-at [data index new-item]
    (let [old-items (get-items data)
          new-items (merge old-items {index new-item})]
      (map->Collection (merge data {:items new-items}))))
  JsonObjectable
  (to-json-object [data] (JsonObject/from (make-couchable data)))

  JsonDocumentable
  (to-json-document [data id] (JsonDocument/create id (to-json-object data))))

(defn make-collection [& {:keys [id name owner-id lists]
                          :or {id (makeid)
                               name nil
                               owner-id nil
                               lists {}}}]
  (when (not name)
    (throw (ex-info ":name parameter missing" {})))
  (when (not owner-id)
    (throw (ex-info ":owner-id parameter missing" {})))
  (map->Collection {:id id
                    :type (the-collection-document-type)
                    :name name
                    :owner-id owner-id
                    :items lists}))

;;; (def $things-id (makeid))
;;; (def $mikel-id (makeid))
;;; (def $greer-id (makeid))
;;; (def $things (make-collection :id $things-id :name "Things" :owner-id $mikel-id))
;;; (make-couchable $things)
;;; (def $things2 (rename (update-owner-id $things $greer-id) "My Things"))
;;; (make-couchable $things2)
;;; (def $things3 (add-item $things2 {:id "foo" :name "A List Name"}))
;;; (make-couchable $things3)
;;; (max-item-index $things3)
;;; (def $things4 (add-item $things3 {:id "bar" :name "Another List Name"}))
;;; (make-couchable $things4)
;;; (def $things5 (remove-item-at $things4 0))
;;; (make-couchable $things5)
