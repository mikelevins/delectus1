(ns delectus-api-server.couchbase.delectus.lists
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.delectus.columnar
             :refer [Columnar add-column column-at find-column-name
                     get-columns max-column-index update-columns upsert-column-at]]
            [delectus-api-server.couchbase.delectus.columns :refer [make-column]]
            [delectus-api-server.couchbase.delectus.deletable :refer [Deletable mark-deleted]]
            [delectus-api-server.couchbase.delectus.identifiable :refer [Identifiable get-id]]
            [delectus-api-server.couchbase.delectus.itemizing
             :refer [Itemizing get-items item-at update-item-at update-items]]
            [delectus-api-server.couchbase.delectus.nameable :refer [Nameable get-name rename]]
            [delectus-api-server.couchbase.delectus.ownable :refer [Ownable get-owner-id update-owner-id]]
            [delectus-api-server.couchbase.delectus.typable :refer [Typable get-type]]
            [delectus-api-server.couchbase.delectus.users :as delectus-users]
            [delectus-api-server.couchbase.marshal
             :refer [Couchable JsonDocumentable JsonObjectable make-couchable to-json-document to-json-object to-map]]
            [delectus-api-server.identifiers :refer [makeid]]
            )
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; List
;;; ---------------------------------------------------------------------

(defn the-list-document-type [] "delectus_list")

(defrecord List [id type name owner-id deleted columns items]

  Columnar
  (add-column [data column-name]
    (when (find-column-name data column-name)
      (throw (ex-info "Column already exists" {:name column-name})))
    (let [max-index (max-column-index data)
          new-index (if max-index (+ 1 max-index) 0)]
      (upsert-column-at data new-index (make-column :name column-name))))
  (column-at [data index] (get (:columns data) index))
  (find-column-name [data column-name]
    (let [cols (get-columns data)]
      (some #(and (= column-name (get-name (column-at data %)))
                  %)
            (keys cols))))
  (get-columns [data] (:columns data))
  (max-column-index [data]
    (let [cols (get-columns data)]
      (if (empty? cols) nil (apply max (keys cols)))))
  (update-columns [data new-columns]
    (map->List (merge data {:columns new-columns})))
  (upsert-column-at [data index new-column]
    (let [old-columns (:columns data)]
      (map->List (merge data
                        {:columns (merge old-columns
                                         {index new-column})}))))

  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs))))

  Deletable
  (mark-deleted [data deleted?]
    (map->List (merge data {:deleted deleted?})))

  Identifiable
  (get-id [data] (:id data))

  Itemizing
  (get-items [data] (:items data))
  (item-at [data index]
    (get (:items data) index))
  (update-items [data new-items]
    (map->List (merge data {:items new-items})))
  (upsert-item-at [data index new-item]
    (let [old-items (:items data)]
      (map->List (merge data
                        {:items (merge old-items
                                       {index new-item})}))))
  
  JsonDocumentable
  (to-json-document [data id] (JsonDocument/create id (to-json-object data)))

  JsonObjectable
  (to-json-object [data] (JsonObject/from (make-couchable data)))

  Nameable
  (get-name [data] (:name data))
  (rename [data new-name] (map->List (merge data {:name new-name})))

  Ownable
  (get-owner-id [data] (:owner-id data))
  (update-owner-id [data new-owner-id] (map->List (merge data {:owner-id new-owner-id})))

  Typable
  (get-type [data] (:type data)))

(defn make-list [& {:keys [id name owner-id columns items]
                    :or {id (makeid)
                         name nil
                         owner-id nil
                         columns {}
                         items {}}}]
  (when (not name)
    (throw (ex-info ":name parameter missing" {})))
  (when (not owner-id)
    (throw (ex-info ":owner-id parameter missing" {})))
  (map->List {:id id
              :type (the-list-document-type)
              :name name
              :owner-id owner-id
              :columns columns
              :items items}))

;;; (def $mikel-id (makeid))
;;; (def $stuffid (makeid))
;;; (def $stuff (make-list :id $stuffid :name "Stuff" :owner-id $mikel-id))
;;; (make-couchable $stuff)
;;; (max-column-index $stuff)
;;; (find-column-name $stuff "Title")
;;; (def $stuff2 (add-column $stuff "Title"))
;;; (make-couchable $stuff2)
;;; (get-columns $stuff2)
;;; (find-column-name $stuff2 "Title")
;;; (find-column-name $stuff2 "NOPE!")
