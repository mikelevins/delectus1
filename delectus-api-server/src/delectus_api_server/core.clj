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
            [delectus-api-server.couchbase.travel-sample.route-handlers :as travel-handlers]
            [delectus-api-server.couchbase.delectus.route-handlers :as delectus-handlers])
  (:gen-class))

;;; ---------------------------------------------------------------------
;;; routes
;;; ---------------------------------------------------------------------

(defroutes app-routes
  (GET "/" [] handlers/landing-page)

  ;; general test routes
  ;; -------------------
  (GET "/hello" [] handlers/hello-name)
  (GET "/status" [] couch-handlers/status)

  ;; travel-sample test routes
  ;; -------------------
  (GET "/document-types" [] travel-handlers/document-types)
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
