(ns delectus-api-server.couchbase.collection-test.route-handlers
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.utilities :as couch-utils]))

;;; ---------------------------------------------------------------------
;;; collection-test handlers and support functions
;;; ---------------------------------------------------------------------
;;; nofunctions defined; just test code in comments for interacting
;;; with "collection-test" bucket

;;; (def $couch (couchbase-cluster))
;;; (def $conf (delectus-configuration))
;;; (.authenticate $couch (:delectus-admin-user $conf)(:delectus-admin-password $conf))
;;; (def $bucket (.openBucket $couch "collection-test"))

;;; List
;;; (def $listid "test_list_1")
;;; (def $flavor-list (new com.couchbase.client.java.datastructures.collections.CouchbaseArrayList $listid $bucket))
;;; (.size $flavor-list)
;;; (.add $flavor-list "Apple")
;;; (.add $flavor-list "Banana")

;;; Map
;;; (def $mapid "test_map_1")
;;; (def $mapval { "Apple" "red", "Banana" "yellow", "Cherry" "red"})
;;; (def $flavor-map (new com.couchbase.client.java.datastructures.collections.CouchbaseMap $mapid $bucket $mapval))
;;; (def $flavor-map (new com.couchbase.client.java.datastructures.collections.CouchbaseMap $mapid $bucket))
;;; (.size $flavor-map)

