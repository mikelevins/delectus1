(ns delectus-api-server.core
  (:require [org.httpkit.server :as server]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer :all]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.data.json :as json]
            )
  (:gen-class))

;;; ---------------------------------------------------------------------
;;; the Delectus 2 API
;;; ---------------------------------------------------------------------
;;;
;;; engine
;;; ---------------------------------------------------------------------
;;; version
;;;
;;; collections
;;; ---------------------------------------------------------------------
;;; find_collection_by_id
;;; find_collection_by_name
;;; create_collection
;;; rename_collection
;;; delete_collection
;;; purge_collection
;;;
;;; lists
;;; ---------------------------------------------------------------------
;;; find_list_by_id
;;; find_list_by_name
;;; create_list
;;; rename_list
;;; delete_list
;;; purge_list
;;; include_deleted
;;; has_deleted
;;; add_row
;;; add_column
;;;
;;; columns
;;; ---------------------------------------------------------------------
;;; count_columns
;;; count_deleted_columns
;;; find_column_by_id
;;; find_column_by_label
;;; find_column_by_index
;;; is_duplicate_label
;;; relabel_column
;;; is_column_deleted
;;; mark_column_deleted
;;;
;;; rows
;;; ---------------------------------------------------------------------
;;; count_rows
;;; count_deleted_rows
;;; value_at
;;; put_value_at
;;; rename_column
;;; is_row_deleted
;;; mark_row_deleted


;;; ---------------------------------------------------------------------
;;; handlers
;;; ---------------------------------------------------------------------

(defn landing-page [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "Delectus 2 API Server, v 0.1"})

(defn hello-name [req]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body (->
          (pp/pprint req)
          (str "Hello, " (:name (:params req))))})

(defn status [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (com.couchbase.client.java.CouchbaseCluster/create ["mars.local"])]
           (.authenticate couch "admin" "password")
           (let [mgr (.clusterManager couch)
                 info (.raw (.info mgr))]
             (.toString info)))})

;;; returns: ("airline" "airport" "hotel" "landmark" "route")
(defn travel-entity-types [req]
  {:status 200
   :headers {"Content-type" "text/plain"}
   :body (let [couch (com.couchbase.client.java.CouchbaseCluster/create ["mars.local"])]
           (.authenticate couch "admin" "password")
           (let [bucket (.openBucket couch "travel-sample")]
             (.createN1qlPrimaryIndex (.bucketManager bucket)
                                      true false)
             (let [result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple
                                          "SELECT * FROM `travel-sample` WHERE type IS NOT MISSING"))
                   vals (map (fn [r] (.value r)) result)
                   objs (map (fn [v](json/read-json (.toString v))) vals)
                   types (distinct (map (fn [o] (:type (:travel-sample o))) objs))
                   ]
               (pp/cl-format nil "~s" types))))})

(defn airlines [req]
  {:status 200
   :headers {"Content-type" "text/plain"}
   :body (let [couch (com.couchbase.client.java.CouchbaseCluster/create ["mars.local"])]
           (.authenticate couch "admin" "password")
           (let [bucket (.openBucket couch "travel-sample")]
             (.createN1qlPrimaryIndex (.bucketManager bucket)
                                      true false)
             (let [result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple
                                          "SELECT name FROM `travel-sample` WHERE type = \"airline\""))
                   vals (map (fn [r]
                               (:name (json/read-json (.toString (.value r)))))
                             result)]
               (pp/cl-format nil "~s" vals))))})

(defn hotels [req]
  {:status 200
   :headers {"Content-type" "text/plain"}
   :body (let [couch (com.couchbase.client.java.CouchbaseCluster/create ["mars.local"])]
           (.authenticate couch "admin" "password")
           (let [bucket (.openBucket couch "travel-sample")]
             (.createN1qlPrimaryIndex (.bucketManager bucket)
                                      true false)
             (let [result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple
                                          "SELECT name FROM `travel-sample` WHERE type = \"hotel\""))
                   vals (map (fn [r]
                               (:name (json/read-json (.toString (.value r)))))
                             result)]
               (pp/cl-format nil "~s" vals))))})

;;; ---------------------------------------------------------------------
;;; routes
;;; ---------------------------------------------------------------------

(defroutes app-routes
  (GET "/" [] landing-page)
  (GET "/hello" [] hello-name)
  (GET "/status" [] status)
  (GET "/travel-types" [] travel-entity-types)
  (GET "/airlines" [] airlines)
  (GET "/hotels" [] hotels)
  ;; default route
  (route/not-found "Error, page not found!"))

;;; ---------------------------------------------------------------------
;;; main server program
;;; ---------------------------------------------------------------------

(defn -main
  "This is our main entry point"
  [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "9000"))]
    ;; Run the server with Ring.defaults middleware
    (server/run-server (wrap-defaults #'app-routes site-defaults) {:port port})
    ;; Run the server without ring defaults
    ;;(server/run-server #'app-routes {:port port})
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))
