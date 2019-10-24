(ns delectus-api-server.couchbase.delectus.columns
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
;;; Column
;;; ---------------------------------------------------------------------

(defn the-column-document-type [] "delectus_column")

;;; id: an identifier created with makeid
;;; list: the id of the list that contains the row
;;; deleted: a Boolean indicating wherther the row has been marked deleted
;;; fields: a map from integer to value

(defrecord Column [deleted label]
  Deletable
  (mark-deleted [data deleted?]
    (map->Column (merge data {:deleted deleted?})))

  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs)))))

(defn make-column [& {:keys [deleted label]
                      :or {deleted false
                           label nil}}]
  (when (not label)
    (throw (ex-info ":label parameter missing" {})))
  (map->Column {:deleted deleted
                :label label}))

;;; (def $col (make-column :label "Title"))
;;; (make-couchable $col)
;;; (def $col2 (mark-deleted $col true))
;;; (make-couchable $col2)

