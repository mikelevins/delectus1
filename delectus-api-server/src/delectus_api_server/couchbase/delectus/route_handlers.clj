(ns delectus-api-server.couchbase.delectus.route-handlers
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [hiccup.core :refer :all]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.utilities :refer [for-couchbase map->JsonObject]]
            [delectus-api-server.couchbase.delectus.users :as users])
  (:import
   (com.couchbase.client.java.datastructures.collections CouchbaseMap)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; support functions
;;; ---------------------------------------------------------------------

;;; NOTES on size limits:
;;; The cap on RAM per document is 20MB
;;; A user document is currently around 256 bytes
;;; that means we can store around 81,920 users in
;;; a single user document
;; a lot, but maybe it's better to store one user per document.

(defn delectus-users  []
  (let [bucket (config/delectus-bucket)
        select-expression "SELECT * from `delectus` WHERE `type` = \"delectus-user\""
        result (.query bucket (N1qlQuery/simple select-expression))
        vals (distinct (map (fn [r] (.value r)) result))
        objs (map (fn [v](get (json/read-json (.toString v) false) "delectus")) vals)]
    objs))

;;; (def $users (delectus-users))
;;; (time (def $users (delectus-users)))

(defn find-delectus-username [username]
  (let [bucket (config/delectus-bucket)
        select-expression (pp/cl-format false "SELECT * from `delectus` WHERE `type` = \"delectus-user\" AND `username` = ~S" username)
        result (.query bucket (N1qlQuery/simple select-expression))
        vals (map #(.value %) result)
        objs (map #(get (json/read-json (.toString %) false) "delectus") vals)]
    objs))

;;; (def $u (find-delectus-username "mikelevins"))
;;; (def $u (find-delectus-username "NOPE!"))

(defn add-delectus-user [useraccount]
  (let [bucket (config/delectus-bucket)
        userid (get useraccount "id")
        userobject (zipmap (map for-couchbase (keys useraccount))
                           (map for-couchbase (vals useraccount)))
        couchmap (new CouchbaseMap userid bucket userobject)]
    couchmap))

(defn remove-delectus-user [userid]
  (let [bucket (config/delectus-bucket)]
    (if (.exists bucket userid)
      (do (.remove bucket userid)
          userid)
      nil)))

;;; (add-delectus-user (users/make-user-account "mikelevins"))
;;; (remove-delectus-user (get (find-delectus-username "mikelevins") "id"))

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
              (json/write-str found))})

