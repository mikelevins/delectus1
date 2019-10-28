(ns delectus-api-server.couchbase.delectus.items
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.marshal
             :refer [Couchable JsonDocumentable JsonObjectable make-couchable to-json-document to-json-object to-map]]
            [delectus-api-server.couchbase.delectus.deletable :refer [Deletable mark-deleted]]
            [delectus-api-server.couchbase.delectus.users :as delectus-users]
            [delectus-api-server.identifiers :refer [makeid]])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; Item
;;; ---------------------------------------------------------------------

(defn the-item-document-type [] "delectus_item")

;;; deleted: a Boolean indicating whether the item has been marked deleted
;;; fields: a map from integer to value

(defprotocol FieldBearing
  (add-field [data field-value])
  (field-at [data index])
  (get-fields [data])
  (max-field-index [data])
  (remove-field-at [data index])
  (update-fields [data new-fields])
  (upsert-field-at [data index new-field-value]))

(defrecord Item [deleted fields]

  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs))))

  Deletable
  (mark-deleted [data deleted?]
    (map->Item (merge data {:deleted deleted?})))

  FieldBearing
  (add-field [data field-value]
    (let [max-index (max-field-index data)
          new-index (if max-index (+ 1 max-index) 0)]
      (upsert-field-at data new-index field-value)))
  (get-fields [data] (:fields data))
  (field-at [data index](get (:fields data) index))
  (max-field-index [data]
    (if (empty? (get-fields data))
      nil
      (apply max (keys (get-fields data)))))
  (remove-field-at [data index]
    (update-fields data
                   (dissoc (get-fields data)
                           index)))
  (update-fields [data new-fields]
    (map->Item (merge data {:fields new-fields})))
  (upsert-field-at [data index new-field-value]
    (let [old-fields (get-fields data)
          new-fields (merge old-fields {index new-field-value})]
      (map->Item (merge data {:fields new-fields})))))

(defn make-item [& {:keys [deleted fields]
                   :or {deleted false
                        fields {}}}]
  (map->Item {:deleted deleted
              :fields fields}))

;;; (def $item (make-item :fields {}))
;;; (make-couchable $item)
;;; (def $item2 (mark-deleted $item true))
;;; (make-couchable $item2)

(defn values->item [& vals]
  (make-item :fields (zipmap (take (count vals) (range))
                             vals)))

;;; (def $item (values->item "A" "B" "C" "D"))
