(ns delectus-api-server.couchbase.marshal
  (:require [clojure.data.json :as json]
            [delectus-api-server.identifiers :refer [makeid]])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)))

;;; ---------------------------------------------------------------------
;;; convert values to types that can be represented in Couchbase collections
;;; ---------------------------------------------------------------------

(defprotocol Couchable
  "Allows a type to be converted to and from CouchBase representations"
  (make-couchable [data]))

(extend-type nil
  Couchable
  (make-couchable [data] data))

;;; (make-couchable nil)

(extend-type java.lang.Boolean
  Couchable
  (make-couchable [data] data))

;;; (make-couchable true)

(extend-type java.lang.Number
  Couchable
  (make-couchable [data] data))

;;; (make-couchable 123.45)

(extend-type java.lang.String
  Couchable
  (make-couchable [data] data))

;;; (make-couchable "Hello...")

(extend-type clojure.lang.Keyword
  Couchable
  (make-couchable [data] (name data)))

;;; (make-couchable :foo)

(extend-type clojure.lang.Symbol
  Couchable
  (make-couchable [data] (name data)))

;;; (make-couchable 'foo)

(extend-type java.util.List
  Couchable
  (make-couchable [data] (java.util.ArrayList. (into [] (map make-couchable data)))))

;;; (class (make-couchable [0 1 :foo 3 4]))

(extend-type java.util.Map
  Couchable
  (make-couchable [data]
    (let [ks (map make-couchable (keys data))
          vs (map make-couchable (vals data))]
      (java.util.HashMap. (zipmap ks vs)))))

;;; (class (make-couchable {:a 1 :b 2 :c [3 "three" 3.0] :d {:val 4.0}}))


;;; ---------------------------------------------------------------------
;;; convert lists to JsonArray 
;;; ---------------------------------------------------------------------

(defprotocol JsonArrayable
  (to-json-array [data]))


(extend-type java.util.List
  JsonArrayable
  (to-json-array [data] (JsonArray/from (make-couchable data))))

;;; (to-json-array [0 1 :foo 3 4])

;;; ---------------------------------------------------------------------
;;; convert values to lists 
;;; ---------------------------------------------------------------------

(defprotocol Listable
  (to-list [data]))


(extend-type com.couchbase.client.java.document.json.JsonArray
  Listable
  (to-list [data](into [] (.toList data))))

;;; (to-list (to-json-array [0 1 :foo 3 4]))

;;; ---------------------------------------------------------------------
;;; convert maps to JsonObject 
;;; ---------------------------------------------------------------------

(defprotocol JsonObjectable
  (to-json-object [data]))


(extend-type java.util.Map
  JsonObjectable
  (to-json-object [data] (JsonObject/from (make-couchable data))))

;;; (to-json-object {:a 1 :b 2 :c [3 "three" 3.0] :d {:val 4.0}})

;;; ---------------------------------------------------------------------
;;; convert maps to JsonDocument 
;;; ---------------------------------------------------------------------

(defprotocol JsonDocumentable
  (to-json-document [data id]))

(extend-type java.util.Map
  JsonDocumentable
  (to-json-document [data id] (JsonDocument/create id (to-json-object data))))

;;; (to-json-document (make-couchable {:a 1 :b 2 :c [3 "three" 3.0] :d {:val 4.0}}) (makeid))

;;; ---------------------------------------------------------------------
;;; convert values to maps 
;;; ---------------------------------------------------------------------

(defprotocol Mappable
  (to-map [data]))

(extend-type com.couchbase.client.java.document.json.JsonObject
  Mappable
  (to-map [data](into {} (.toMap data))))

(extend-type com.couchbase.client.java.document.JsonDocument
  Mappable
  (to-map [data]
    (let [content-map (to-map (.content data))]
      (merge content-map
             {:document-id (.id data)}))))

;;; (to-map (to-json-document (make-couchable {:a 1 :b 2 :c [3 "three" 3.0] :d {:val 4.0}}) (makeid)))

