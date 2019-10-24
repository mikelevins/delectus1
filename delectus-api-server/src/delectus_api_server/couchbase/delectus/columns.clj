(ns delectus-api-server.couchbase.delectus.columns
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
;;; List
;;; ---------------------------------------------------------------------

(defn the-column-document-type [] "delectus_column")

(defrecord Column [id type label]
  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs))))
  JsonObjectable
  (to-json-object [data] (JsonObject/from (make-couchable data)))
  JsonDocumentable
  (to-json-document [data id] (JsonDocument/create id (to-json-object data))))

(defn make-column [& {:keys [id label]
                       :or {id (makeid)
                            label nil}}]
  (when (not label)
    (throw (ex-info ":label parameter missing" {})))
  (map->Column {:id id
                :type (the-column-document-type)
                :label label}))

;;; (def $col (make-column :label "Title"))
;;; (make-couchable $col)
