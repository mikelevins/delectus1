(ns delectus-api-server.couchbase.delectus.lists
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.io :as couch-io]
            [delectus-api-server.couchbase.delectus.columnar
             :refer [Columnar add-column column-at find-column-name
                     get-column-indexes get-column-names  get-columns
                     max-column-index update-columns upsert-column-at]]
            [delectus-api-server.couchbase.delectus.columns :refer [make-column]]
            [delectus-api-server.couchbase.delectus.deletable :refer [Deletable mark-deleted]]
            [delectus-api-server.couchbase.delectus.identifiable :refer [Identifiable get-id]]
            [delectus-api-server.couchbase.delectus.itemizing
             :refer [Itemizing add-item get-items item-at max-item-index remove-item-at update-items upsert-item-at]]
            [delectus-api-server.couchbase.delectus.items
             :refer [add-field field-at get-fields max-field-index remove-field-at
                     update-fields upsert-field-at values->item]]
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
  (get-column-indexes [data] (into () (keys (get-columns data))))
  (get-column-names [data] (map get-name (vals (get-columns data))))
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
    (map->List (merge data {:items new-items})))
  (upsert-item-at [data index new-item]
    (let [old-items (get-items data)
          new-items (merge old-items {index new-item})]
      (map->List (merge data {:items new-items}))))

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

(defn make-list [& {:keys [id name owner-id deleted columns items]
                    :or {id (makeid)
                         name nil
                         owner-id nil
                         deleted false
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
              :deleted deleted
              :columns columns
              :items items}))

;;; (def $mikel-id (makeid))
;;; (def $stuffid (makeid))
;;; (def $stuff (make-list :id $stuffid :name "Stuff" :owner-id $mikel-id))
;;; (make-couchable $stuff)
;;; (max-column-index $stuff)
;;; (max-item-index $stuff)
;;; (find-column-name $stuff "Title")
;;; (def $stuff2 (add-column $stuff "Title"))
;;; (make-couchable $stuff2)
;;; (get-columns $stuff2)
;;; (find-column-name $stuff2 "Title")
;;; (find-column-name $stuff2 "NOPE!")
;;; (def $stuff3 (add-item $stuff2 ["Thing 1"]))
;;; (make-couchable $stuff3)
;;; (def $stuff4 (add-item $stuff3 ["Thing 2"]))
;;; (make-couchable $stuff4)
;;; (get-columns $stuff4)
;;; (get-items $stuff4)
;;; (column-at $stuff4 0)
;;; (item-at $stuff4 0)
;;; (item-at $stuff4 1)
;;; (max-column-index $stuff4)
;;; (max-item-index $stuff4)
;;; (def $stuff5 (add-column $stuff4 "Flavor"))
;;; (column-at $stuff5 0)
;;; (column-at $stuff5 1)
;;; (item-at $stuff5 0)
;;; (item-at $stuff4 1)
;;; (make-couchable $stuff5)
;;; (get-column-indexes $stuff5)
;;; (get-column-names $stuff5)

;;; ---------------------------------------------------------------------
;;; Couchbase List records
;;; ---------------------------------------------------------------------

(defn delectus-lists [user-id]
  (let [bucket (config/delectus-content-bucket)
        bucket-name (.name bucket)
        select-expression (cl-format nil "SELECT id,name from `~A` WHERE type = \"delectus_list\" AND `owner-id` =\"~A\""
                                     bucket-name user-id)
        results (.query bucket (N1qlQuery/simple select-expression))]
    (map #(to-map (.value %))
         results)))

;;; (time (delectus-lists (delectus-users/delectus-user-email->id "mikel@evins.net")))


(defn add-delectus-list! [owner-id list-name & {:keys [list-id]
                                                :or {list-id (makeid)}}]
  (when (not owner-id)
    (throw (ex-info "Missing owner id" {})))
  (let [bucket (config/delectus-content-bucket)
        already-list-document (couch-io/get-document bucket list-id)]
    (if already-list-document
      (throw (ex-info "A list with the supplied ID already exists" {:id list-id :bucket (.name bucket)}))
      (let [new-list-map (make-list :id list-id
                                    :name list-name
                                    :owner-id owner-id)
            new-list-document (to-json-document new-list-map list-id)]
        (.insert bucket new-list-document)
        list-id))))

;;; (def $things (add-delectus-list! (delectus-users/delectus-user-email->id "mikel@evins.net") "Things"))
