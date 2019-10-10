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
;;; 
;;; ---------------------------------------------------------------------

(defn landing-page [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "Delectus 2 API Server, v 0.1"})

(defn hello-name [req]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body (->
          (clojure.pprint/pprint req)
          (str "Hello, " (:name (:params req))))})

(defn couch-page [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (com.couchbase.client.java.CouchbaseCluster/create ["mars.local"])
               diagnostics (.diagnostics couch)]
           (.authenticate couch "admin" "password")
           (.exportToJson diagnostics))})


(defn travel-page [req]
  {:status 200
   :headers {"Content-type" "text/plain"}
   :body (let [couch (com.couchbase.client.java.CouchbaseCluster/create ["mars.local"])]
           (.authenticate couch "admin" "password")
           (let [bucket (.openBucket couch "travel-sample")]
             (.createN1qlPrimaryIndex (.bucketManager bucket)
                                      true false)
             (let [result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple
                                          "SELECT id FROM `travel-sample`"))]
               (clojure.pprint/cl-format nil "~S" result))))})

;;; ---------------------------------------------------------------------
;;; routes
;;; ---------------------------------------------------------------------

(defroutes app-routes
  (GET "/" [] landing-page)
  (GET "/hello" [] hello-name)
  (GET "/couch" [] couch-page)
  (GET "/travel" [] travel-page)
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
