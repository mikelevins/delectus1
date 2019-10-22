(ns delectus-api-server.couchbase.utilities
  (:require [clojure.data.json :as json]))

;;; ---------------------------------------------------------------------
;;; Couchbase support functions
;;; ---------------------------------------------------------------------

(defn ensure-primary-index [bucket]
  ;; create a N1QL primary index, unless it already exists
  (.createN1qlPrimaryIndex (.bucketManager bucket) true false))
