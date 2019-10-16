(ns delectus-api-server.couchbase.utilities
  (:import
   (com.couchbase.client.java.document.json JsonObject)))

;;; ---------------------------------------------------------------------
;;; Couchbase support functions
;;; ---------------------------------------------------------------------

(defn ensure-primary-index [bucket]
  ;; create a N1QL primary index, unless it already exists
  (.createN1qlPrimaryIndex (.bucketManager bucket) true false))

(defn ->JsonObject [mapdata] (JsonObject/from mapdata))
