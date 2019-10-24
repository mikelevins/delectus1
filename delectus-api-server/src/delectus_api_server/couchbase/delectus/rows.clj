(ns delectus-api-server.couchbase.delectus.rows
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.couchbase.marshal
             :refer [Couchable JsonDocumentable JsonObjectable
                     make-couchable to-json-document to-json-object to-map]]
            [delectus-api-server.couchbase.delectus.users :as delectus-users]
            [delectus-api-server.couchbase.delectus.deletable :refer [Deletable mark-deleted]])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; Row
;;; ---------------------------------------------------------------------

(defn the-row-document-type [] "delectus_row")

;;; deleted: a Boolean indicating wherther the row has been marked deleted
;;; fields: a map from integer to value

(defrecord Row [deleted fields]
  Deletable
  (mark-deleted [data deleted?]
    (map->Row (merge data {:deleted deleted?})))

  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs)))))

(defn make-row [& {:keys [deleted fields]
                   :or {deleted false
                        fields {}}}]
  (map->Row {:deleted deleted
             :fields fields}))

;;; (def $row (make-row :id 0 :fields {}))
;;; (make-couchable $row)
;;; (def $row2 (mark-deleted $row true))
;;; (make-couchable $row2)
