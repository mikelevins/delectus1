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
            [delectus-api-server.couchbase.delectus.typable :refer [Typable get-type]]
            [delectus-api-server.couchbase.delectus.nameable :refer [Nameable get-name rename]])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; Collection
;;; ---------------------------------------------------------------------

(defn the-collection-document-type [] "delectus_collection")

(defrecord Collection [id type name owner-id]
  Identifiable
  (get-id [data] (:id data))

  Typable
  (get-type [data] (:type data))

  Nameable
  (get-name [data] (:name data))
  (rename [data new-name] (map->Collection (merge data {:name new-name})))

  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs))))

  JsonObjectable
  (to-json-object [data] (JsonObject/from (make-couchable data)))

  JsonDocumentable
  (to-json-document [data id] (JsonDocument/create id (to-json-object data))))

(defn make-collection [& {:keys [id name owner-id]
                          :or {id (makeid)
                               name nil
                               owner-id nil}}]
  (when (not name)
    (throw (ex-info ":name parameter missing" {})))
  (when (not owner-id)
    (throw (ex-info ":owner-id parameter missing" {})))
  (map->Collection {:id id
                    :type (the-collection-document-type)
                    :name name
                    :owner-id owner-id}))

;;; (def $things-id (makeid))
;;; (def $mikel-id (makeid))
;;; (def $things (make-collection :id $things-id :name "Things" :owner-id $mikel-id))
;;; (make-couchable $things)
;;; (def $things2 (rename $things "My Things"))
;;; (make-couchable $things2)
