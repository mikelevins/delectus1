(ns delectus-api-server.couchbase.delectus.collections
  (:require [clojure.data.json :as json]
            [clojure.pprint :refer [cl-format]]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.identifiers :refer [makeid]]
            [delectus-api-server.couchbase.marshal
             :refer [Couchable JsonDocumentable JsonObjectable
                     make-couchable to-json-document to-json-object to-map]])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; User
;;; ---------------------------------------------------------------------

;; (defn user-roles [] ["user"])

(defn the-collection-document-type [] "delectus_collection")

(defrecord Collection [id type name owner-id access lists]
  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs))))
  JsonObjectable
  (to-json-object [data] (JsonObject/from (make-couchable data)))
  JsonDocumentable
  (to-json-document [data id] (JsonDocument/create id (to-json-object data))))

(defn make-collection [& {:keys [id name owner-id access lists]
                          :or {id (makeid)
                               name nil
                               owner-id nil
                               access {}
                               lists {}}}]
  (when (not name)
    (throw (ex-info ":name parameter missing" {})))
  (when (not owner-id)
    (throw (ex-info ":owner-id parameter missing" {})))
  (map->Collection {:id id
                    :type (the-collection-document-type)
                    :name name
                    :owner-id owner-id
                    :access access
                    :lists lists}))

;;; (def $things-id (makeid))
;;; (def $things (make-collection :id $things-id :name "Things"))
;;; (make-couchable $mikel)
;;; (to-json-object $mikel)
;;; (to-json-document $mikel $mikel-id)
