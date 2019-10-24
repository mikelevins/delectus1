(ns delectus-api-server.couchbase.delectus.lists
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

(defn the-list-document-type [] "delectus_list")

(defrecord List [id type name owner-id columns rows]
  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs))))
  JsonObjectable
  (to-json-object [data] (JsonObject/from (make-couchable data)))
  JsonDocumentable
  (to-json-document [data id] (JsonDocument/create id (to-json-object data))))

(defn make-list [& {:keys [id name owner-id columns rows]
                    :or {id (makeid)
                         name nil
                         owner-id nil
                         columns {}
                         rows {}}}]
  (when (not name)
    (throw (ex-info ":name parameter missing" {})))
  (when (not owner-id)
    (throw (ex-info ":owner-id parameter missing" {})))
  (map->List {:id id
              :type (the-list-document-type)
              :name name
              :owner-id owner-id
              :columns columns
              :rows {}}))

;;; (def $mikel-id (makeid))
;;; (def $stuffid (makeid))
;;; (def $stuff (make-list :id $stuffid :name "Stuff" :owner-id $mikel-id))
;;; (make-couchable $stuff)
