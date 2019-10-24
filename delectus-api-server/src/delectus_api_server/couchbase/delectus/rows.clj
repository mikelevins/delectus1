(ns delectus-api-server.couchbase.delectus.rows
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.couchbase.marshal
             :refer [Couchable JsonDocumentable JsonObjectable
                     make-couchable to-json-document to-json-object to-map]]
            [delectus-api-server.couchbase.delectus.users :as delectus-users])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; Row
;;; ---------------------------------------------------------------------

(defn the-row-document-type [] "delectus_row")

(defrecord Row [id fields]
  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs))))
  JsonObjectable
  (to-json-object [data] (JsonObject/from (make-couchable data)))
  JsonDocumentable
  (to-json-document [data id] (JsonDocument/create id (to-json-object data))))

(defn make-row [& {:keys [id fields]
                   :or {id (makeid)
                        fields {}}}]
  (map->Row {:id id
             :fields fields}))

;;; (def $row (make-row :id 0 :fields {}))
;;; (make-couchable $row)
