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
;;; test handlers
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
(defn travel-types [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (com.couchbase.client.java.CouchbaseCluster/create ["mars.local"])]
           (.authenticate couch "admin" "password")
           (let [bucket (.openBucket couch "travel-sample")]
             (.createN1qlPrimaryIndex (.bucketManager bucket) true false)
             (let [result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple
                                          "SELECT type FROM `travel-sample` WHERE type IS NOT MISSING"))
                   vals (distinct (map (fn [r] (.value r)) result))
                   objs (map (fn [v](:type (json/read-json (.toString v)))) vals)]
               (json/write-str objs))))})

(defn airlines [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (com.couchbase.client.java.CouchbaseCluster/create ["mars.local"])]
           (.authenticate couch "admin" "password")
           (let [bucket (.openBucket couch "travel-sample")]
             (.createN1qlPrimaryIndex (.bucketManager bucket)
                                      true false)
             (let [limit (or (:limit (:params req))
                             50)
                   result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple
                                          (pp/cl-format nil
                                                        "SELECT * FROM `travel-sample` WHERE type = \"airline\" LIMIT ~A"
                                                        limit)))
                   vals (map (fn [r](:travel-sample (json/read-json (.toString (.value r)))))
                             result)]
               (json/write-str vals))))})

(defn airports [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (com.couchbase.client.java.CouchbaseCluster/create ["mars.local"])]
           (.authenticate couch "admin" "password")
           (let [bucket (.openBucket couch "travel-sample")]
             (.createN1qlPrimaryIndex (.bucketManager bucket)
                                      true false)
             (let [limit (or (:limit (:params req))
                             50)
                   result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple
                                          (pp/cl-format nil
                                                        "SELECT * FROM `travel-sample` WHERE type = \"airport\" LIMIT ~A"
                                                        limit)))
                   vals (map (fn [r](:travel-sample (json/read-json (.toString (.value r)))))
                             result)]
               (json/write-str vals))))})

(defn hotels [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (com.couchbase.client.java.CouchbaseCluster/create ["mars.local"])]
           (.authenticate couch "admin" "password")
           (let [bucket (.openBucket couch "travel-sample")]
             (.createN1qlPrimaryIndex (.bucketManager bucket)
                                      true false)
             (let [limit (or (:limit (:params req))
                             50)
                   result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple
                                          (pp/cl-format nil
                                                        "SELECT * FROM `travel-sample` WHERE type = \"hotel\" LIMIT ~A"
                                                        limit)))
                   vals (map (fn [r](:travel-sample (json/read-json (.toString (.value r)))))
                             result)]
               (json/write-str vals))))})

;;; ---------------------------------------------------------------------
;;; routes
;;; ---------------------------------------------------------------------

(defroutes app-routes
  ;; landing page
  (GET "/" [] landing-page)
  ;; testing routes
  (GET "/hello" [] hello-name)
  (GET "/status" [] status)
  (GET "/travel-types" [] travel-types)
  (GET "/airlines" [] airlines)
  (GET "/airports" [] airports)
  (GET "/hotels" [] hotels)
  ;; Delectus API routes
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
