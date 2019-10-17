(ns delectus-api-server.couchbase.delectus.route-handlers
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clojure.data.json :as json]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.utilities :as couch-utils]
            [hiccup.core :refer :all])
  (:import
   (com.couchbase.client.java.datastructures.collections CouchbaseMap)))

;;; ---------------------------------------------------------------------
;;; delectus handlers and support functions
;;; ---------------------------------------------------------------------

(defn delectus-users []
  (let [couch (config/couchbase-cluster)
        configuration (config/delectus-configuration)
        bucket (config/delectus-bucket)
        users-doc-id (:delectus-users-document-name (config/delectus-configuration))
        users-doc (.get bucket users-doc-id)]
    (or users-doc
        (let [new-users-doc (new CouchbaseMap users-doc-id bucket {})]
          new-users-doc))))

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
    "password_hash" ""
    "collections" {}
    "lists" {}}))

;;; (make-user-map "mikel")
;;; (.get (make-user-map "mikel") "username")

(defn add-delectus-user [usermap]
  (let [bucket (config/delectus-bucket)
        users-doc-id (:delectus-users-document-name (config/delectus-configuration))
        users-couchmap (new CouchbaseMap users-doc-id bucket)
        username (.get usermap "username")
        userid (make-userid username)]
    (.put users-couchmap userid usermap)
    userid))

(defn remove-delectus-user [username]
  (let [bucket (config/delectus-bucket)
        users-doc-id (:delectus-users-document-name (config/delectus-configuration))
        users-couchmap (new CouchbaseMap users-doc-id bucket)
        userid (make-userid username)]
    (if (.containsKey users-couchmap userid)
      (do (.remove users-couchmap userid)
          userid)
      nil)))

;;; (add-delectus-user (make-user-map "mikel"))
;;; (remove-delectus-user "mikel")

(defn root [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (html [:h1 "Delectus 2"]
                  [:p "version 0.1"])})

(defn users [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [found (delectus-users)]
              (.toString (.content found)))})

