(ns delectus-api-server.couchbase.delectus.route-handlers
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clojure.data.json :as json]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.utilities :as couch-utils]
            [hiccup.core :refer :all]))

;;; ---------------------------------------------------------------------
;;; delectus handlers and support functions
;;; ---------------------------------------------------------------------

(defn delectus-users []
  (let [couch (config/couchbase-cluster)
        configuration (config/delectus-configuration)]
    (.authenticate couch
                   (:delectus-admin-user configuration)
                   (:delectus-admin-password configuration))
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

(defn make-userid [username]
  (str "user::" username))

(defn make-user-map [username]
  (couch-utils/->JsonObject
   {"username" username
    "collections" []
    "lists" []}))

;;; (make-user-map "mikel")
;;; (.get (make-user-map "mikel") "username")

(defn add-delectus-user [usermap]
  (let [couch (config/couchbase-cluster)
        configuration (config/delectus-configuration)]
    (.authenticate couch
                   (:delectus-admin-user configuration)
                   (:delectus-admin-password configuration))
    (let [bucket-name (:delectus-main-bucket-name (config/delectus-configuration))
          bucket (.openBucket couch bucket-name)
          users-doc-id (:delectus-users-document-name (config/delectus-configuration))
          users-couchmap (com.couchbase.client.java.datastructures.collections.CouchbaseMap. users-doc-id bucket)
          username (.get usermap "username")
          userid (make-userid username)]
      (.put users-couchmap userid usermap)
      userid)))

;;; (add-delectus-user (make-user-map "mikel"))

(defn root [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (html [:h1 "Delectus 2"]
                  [:p "version 0.1"])})


(defn users [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [found (delectus-users)
                  ;;users (json/read-json (.toString found))
                  ]
              (.toString (.content found)))})

