(ns delectus-api-server.couchbase.delectus.route-handlers
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [hiccup.core :refer :all]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.utilities :refer [map->JsonObject]]
            [delectus-api-server.couchbase.delectus.users :as users])
  (:import
   (com.couchbase.client.java.datastructures.collections CouchbaseMap)
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; support functions
;;; ---------------------------------------------------------------------

(defn delectus-users  []
  (let [bucket (config/delectus-bucket)
        select-expression "SELECT * from `delectus` WHERE `type` = \"delectus-user\""
        result (.query bucket (N1qlQuery/simple select-expression))
        vals (distinct (map (fn [r] (.value r)) result))
        objs (map (fn [v](:delectus (json/read-json (.toString v)))) vals)]
    objs))

;;; (def $users (delectus-users))
;;;

(defn add-delectus-user [useraccount]
  (let [bucket (config/delectus-bucket)
        userid (get useraccount "id")
        couchmap (new CouchbaseMap userid bucket useraccount)]
    couchmap))

(defn remove-delectus-user [username]
  (let [bucket (config/delectus-bucket)
        users-doc-id config/+delectus-users-document-name+
        users-couchmap (new CouchbaseMap users-doc-id bucket)
        userid username]
    (if (.containsKey users-couchmap userid)
      (do (.remove users-couchmap userid)
          userid)
      nil)))

;;; (add-delectus-user (users/make-user-account "mikelevins"))
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
              (json/write-str found))})

