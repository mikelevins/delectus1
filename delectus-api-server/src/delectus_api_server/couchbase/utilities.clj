(ns delectus-api-server.couchbase.utilities
  (:require [clojure.data.json :as json])
  (:import
   (com.couchbase.client.java.document JsonDocument)
   (com.couchbase.client.java.document.json JsonArray JsonObject)))

;;; ---------------------------------------------------------------------
;;; converting values to and from CouchBase format
;;; ---------------------------------------------------------------------

(defmulti for-couchbase class)
(defmethod for-couchbase nil [v] v)
(defmethod for-couchbase java.lang.Boolean [b] b)
(defmethod for-couchbase java.lang.Number [n] n)
(defmethod for-couchbase java.lang.String [s] s)
(defmethod for-couchbase clojure.lang.Keyword [s] (name s))
(defmethod for-couchbase clojure.lang.Symbol [s] (name s))

(defmethod for-couchbase clojure.lang.PersistentVector [v]
  (map for-couchbase v))

(defmethod for-couchbase clojure.lang.PersistentArrayMap [m]
  (let [ks (map for-couchbase (keys m))
        vs (map for-couchbase (vals m))]
    (zipmap ks vs)))

;;; (for-couchbase false)
;;; (for-couchbase 1)
;;; (for-couchbase 1.2)
;;; (for-couchbase "hello")
;;; (for-couchbase 'hello)
;;; (for-couchbase :hello)
;;; (for-couchbase [0 "one" ['two :three] 4.0])
;;; (for-couchbase {})
;;; (for-couchbase {:a 1 :b 2})
;;; (for-couchbase {:a 1 :b 2 :c [3 "three" 3.0] :d {:val 4.0}})
;;; (for-couchbase {:name "Fred" :type "cartoon"})

(defn vector->JsonArray [vecdata] (JsonArray/from (for-couchbase vecdata)))

;;; (def $vec (vector->JsonArray []))
;;; (def $vec (vector->JsonArray [:zero 1 {"2" 2} ['three]]))

(defn map->JsonObject [mapdata] (JsonObject/from (for-couchbase mapdata)))

;;; (def $obj (map->JsonObject {}))
;;; (def $obj (map->JsonObject {:name "Fred" :age 32}))
;;; (def $obj (map->JsonObject {:name "Fred" :age 32 :friends ["Barney" "Betty"] :job {:company "Slate Gravel" :boss "Mr. Slate"}}))

(defn map->JsonDocument [docid mapdata]
  (let [obj (JsonObject/from (for-couchbase mapdata))]
    (JsonDocument/create docid obj)))

;;; (def $doc (map->JsonDocument "fred_flintstone" {:name "Fred" :age 32}))

(defn JsonObject->map [object]
  (if object
    (json/read-json (.toString object)
                    true)
    nil))

;;; (JsonObject->map (get-object (config/travel-sample-bucket) "airline_10"))
;;; (JsonObject->map (get-object (config/travel-sample-bucket) "NOPE!"))
;;; (JsonObject->map (.content (map->JsonDocument "fred_flintstone" {:name "Fred" :age 32})))

(defn JsonDocument->map [document]
  (if document
    (JsonObject->map (.content document))
    nil))

;;; (JsonDocument->map (get-document (config/travel-sample-bucket) "airline_10"))
;;; (JsonDocument->map (get-document (config/travel-sample-bucket) "NOPE!"))

;;; ---------------------------------------------------------------------
;;; Couchbase support functions
;;; ---------------------------------------------------------------------

(defn ensure-primary-index [bucket]
  ;; create a N1QL primary index, unless it already exists
  (.createN1qlPrimaryIndex (.bucketManager bucket) true false))
