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
            [delectus-api-server.couchbase.utilities :as couch-utils]
            [delectus-api-server.couchbase.route-handlers :as couch-handlers]
            [delectus-api-server.couchbase.travel-sample.route-handlers :as travel-handlers])
  (:gen-class))


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
  (GET "/status" [] couch-handlers/status)
  ;; travel-sample test routes
  ;; -------------------
  (GET "/document-types" [] (fn [req]
                              (travel-handlers/document-types
                               req
                               (:travel-sample-bucket-name (config/delectus-configuration)))))
  (GET "/airlines" [] travel-handlers/airlines)
  (GET "/airports" [] travel-handlers/airports)
  (GET "/hotels" [] travel-handlers/hotels)
  (GET "/landmarks" [] travel-handlers/landmarks)
  (GET "/routes" [] travel-handlers/travel-routes)
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
