(ns delectus-api-server.couchbase.utilities
  (:import
   (com.couchbase.client.java.document.json JsonArray JsonObject)))

;;; ---------------------------------------------------------------------
;;; Couchbase support functions
;;; ---------------------------------------------------------------------

(defn ensure-primary-index [bucket]
  ;; create a N1QL primary index, unless it already exists
  (.createN1qlPrimaryIndex (.bucketManager bucket) true false))

(defn map->JsonObject [mapdata] (JsonObject/from mapdata))
(defn vector->JsonArray [vecdata] (JsonArray/from vecdata))

;;; ---------------------------------------------------------------------
;;; converting values for storage in CouchBase
;;; ---------------------------------------------------------------------

(defmulti for-couchbase class)
(defmethod for-couchbase nil [v] v)
(defmethod for-couchbase java.lang.Boolean [b] b)
(defmethod for-couchbase java.lang.Number [n] n)
(defmethod for-couchbase java.lang.String [s] s)
(defmethod for-couchbase clojure.lang.Keyword [s] (name s))
(defmethod for-couchbase clojure.lang.Symbol [s] (name s))

(defmethod for-couchbase clojure.lang.PersistentVector [v]
  (java.util.ArrayList. (map for-couchbase v)))

(defmethod for-couchbase clojure.lang.PersistentArrayMap [m]
  (let [ks (map for-couchbase (keys m))
        vs (map for-couchbase (vals m))]
    (map->JsonObject (zipmap ks vs))))

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
