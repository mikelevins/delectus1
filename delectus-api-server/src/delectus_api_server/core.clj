(ns delectus-api-server.core
  (:require [org.httpkit.server :as server]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer :all]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [aero.core :as aero]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.utilities :as utils]
            [delectus-api-server.route-handlers :as handlers]
            [delectus-api-server.couchbase.utilities :as couch-utils])
  (:gen-class))


;;; ---------------------------------------------------------------------
;;; Couchbase handlers
;;; ---------------------------------------------------------------------

(defn status [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (config/couchbase-cluster)
               configuration (config/delectus-configuration)]
           (.authenticate couch
                          (:travel-sample-user configuration)
                          (:travel-sample-password configuration))
           (let [mgr (.clusterManager couch)
                 info (.raw (.info mgr))]
             (.toString info)))})

;;; ---------------------------------------------------------------------
;;; travel-sample handlers and support functions
;;; ---------------------------------------------------------------------

;;; returns: ("airline" "airport" "hotel" "landmark" "route")
(defn document-types [req bucket-name]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (config/couchbase-cluster)
               configuration (config/delectus-configuration)]
           (.authenticate couch
                          (:travel-sample-user configuration)
                          (:travel-sample-password configuration))
           (let [bucket (.openBucket couch bucket-name)]
             (couch-utils/ensure-primary-index bucket)
             (let [select-expression (pp/cl-format nil
                                                   "SELECT type FROM `~A` WHERE type IS NOT MISSING"
                                                   bucket-name)
                   result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple select-expression))
                   vals (distinct (map (fn [r] (.value r)) result))
                   objs (map (fn [v](:type (json/read-json (.toString v)))) vals)]
               (json/write-str objs))))})

(defn objects-of-type [req bucket-name type-name]
  (let [couch (config/couchbase-cluster)
        configuration (config/delectus-configuration)]
    (.authenticate couch
                   (:travel-sample-user configuration)
                   (:travel-sample-password configuration))
    (let [bucket (.openBucket couch bucket-name)]
      (couch-utils/ensure-primary-index bucket)
      (let [limit (or (:limit (:params req)) 10)
            offset (or (:offset (:params req)) 0)
            select-expr (pp/cl-format nil
                                      "SELECT * FROM `~A` WHERE type = \"~A\" LIMIT ~A OFFSET ~A"
                                      bucket-name type-name limit offset)
            result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple select-expr))
            vals (map (fn [r](get (json/read-json (.toString (.value r))) (keyword bucket-name)))
                      result)]
        (json/write-str vals)))))

(defn airlines [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (config/delectus-configuration)) "airline")})

(defn airports [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (config/delectus-configuration)) "airport")})

(defn hotels [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (config/delectus-configuration)) "hotel")})

(defn landmarks [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (config/delectus-configuration)) "landmark")})

(defn travel-routes [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (config/delectus-configuration)) "route")})

;;; ---------------------------------------------------------------------
;;; collection-test handlers and support functions
;;; ---------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------
;;; delectus handlers and support functions
;;; ---------------------------------------------------------------------

;;; (def $couch (couchbase-cluster))
;;; (def $conf (delectus-configuration))
;;; (.authenticate $couch (:delectus-admin-user $conf)(:delectus-admin-password $conf))
;;; (def $bucket (.openBucket $couch (:delectus-main-bucket-name (delectus-configuration))))

;;; should be needed exactly once: to create the "delectus-users" document
(defn create-delectus-users []
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

;;; (def $users (create-delectus-users))

;;; ---------------------------------------------------------------------
;;; routes
;;; ---------------------------------------------------------------------

(defroutes app-routes
  ;; landing page
  ;; ------------
  (GET "/" [] handlers/landing-page)
  ;; general test routes
  ;; -------------------
  (GET "/hello" [] handlers/hello-name)
  (GET "/status" [] status)
  ;; travel-sample test routes
  ;; -------------------
  (GET "/document-types" [] (fn [req] (document-types req (:travel-sample-bucket-name (config/delectus-configuration)))))
  (GET "/airlines" [] airlines)
  (GET "/airports" [] airports)
  (GET "/hotels" [] hotels)
  (GET "/landmarks" [] landmarks)
  (GET "/routes" [] travel-routes)
  ;; Delectus API routes
  ;; -------------------
  
  ;; default ("Page not found") route
  ;; --------------------------------
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
