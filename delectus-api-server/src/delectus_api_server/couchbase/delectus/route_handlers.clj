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
;;; support functions
;;; ---------------------------------------------------------------------

(defn delectus-users []
  (let [couch (config/couchbase-cluster)
        configuration (config/delectus-configuration)
        bucket (config/delectus-bucket)
        users-doc-id config/+delectus-users-document-name+
        users-doc (.get bucket users-doc-id)]
    (or users-doc
        (let [new-users-doc (new CouchbaseMap users-doc-id bucket {})]
          (.get bucket users-doc-id)))))

;;; (def $users (delectus-users))

;;; BUG: the character set allowed in Couchbase map keys is
;;; restricted, which means the character set allowed in usernames is
;;; restricted, if we use usernames as keys in the users document.
;;; For example, we can't use a username like "mikel.evins" because
;;; documents in the database are JSON objects, and "mikel.evins"
;;; looks like a reference to the "evins" field of the "mikel" object
;;; in JSON, which causes key-path errors when creating the entry in
;;; the database.
;;; TODO:
;;;   1. figure out what the set of allowable characters is
;;;   2. determine whether there's a way to avoid overly-restrictive
;;;   rules for usernames
(defn make-userid [username]
  (str "user::" username))

(defn make-user-map [username]
  (couch-utils/->JsonObject
   {"username" username
    "password_hash" ""
    "collections" {}
    "lists" {}}))

;;; (make-user-map "mikelevins")
;;; (.get (make-user-map "mikelevins") "username")

(defn add-delectus-user [usermap]
  (let [bucket (config/delectus-bucket)
        users-doc-id config/+delectus-users-document-name+
        users-couchmap (new CouchbaseMap users-doc-id bucket)
        username (.get usermap "username")
        userid (make-userid username)]
    (.put users-couchmap userid usermap)
    userid))

(defn remove-delectus-user [username]
  (let [bucket (config/delectus-bucket)
        users-doc-id config/+delectus-users-document-name+
        users-couchmap (new CouchbaseMap users-doc-id bucket)
        userid (make-userid username)]
    (if (.containsKey users-couchmap userid)
      (do (.remove users-couchmap userid)
          userid)
      nil)))

;;; (add-delectus-user (make-user-map "mikelevins"))
;;; (remove-delectus-user "mikelevins")

;;; ---------------------------------------------------------------------
;;; delectus handlers
;;; ---------------------------------------------------------------------

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

