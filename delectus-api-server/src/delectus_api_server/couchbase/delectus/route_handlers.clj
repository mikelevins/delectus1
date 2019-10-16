(ns delectus-api-server.couchbase.delectus.route-handlers
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.utilities :as couch-utils]))

;;; ---------------------------------------------------------------------
;;; delectus handlers and support functions
;;; ---------------------------------------------------------------------

(defn delectus-users []
  (let [couch (config/couchbase-cluster)
        configuration (config/delectus-configuration)]
    (.authenticate couch
                   (:travel-sample-user configuration)
                   (:travel-sample-password configuration))
    (let [bucket-name (:delectus-main-bucket-name (config/delectus-configuration))
          bucket (.openBucket couch bucket-name)]
      (let [users-doc-id (:delectus-users-document-name (config/delectus-configuration))
            users-doc (.get bucket users-doc-id)]
        (or users-doc
            (let [new-users-doc (new com.couchbase.client.java.datastructures.collections.CouchbaseMap
                                     users-doc-id bucket {})]
              new-users-doc))))))

;;; (def $couch (config/couchbase-cluster))
;;; (def $conf (config/delectus-configuration))
;;; (.authenticate $couch (:delectus-admin-user $conf)(:delectus-admin-password $conf))
;;; (def $bucket (.openBucket $couch (:delectus-main-bucket-name (config/delectus-configuration))))
;;; (def $users (delectus-users))
