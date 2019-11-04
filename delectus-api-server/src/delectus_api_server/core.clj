(ns delectus-api-server.core
  (:require
   [buddy.auth :refer [authenticated? throw-unauthorized]]
   [buddy.auth.backends :as backends]
   [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]
   [buddy.sign.jwt :as jwt]
   [clojure.java.io :as io]
   [compojure.core :refer :all]
   [compojure.response :refer [render]]
   [compojure.route :as route]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.couchbase.delectus.route-handlers :as delectus-handlers]
   [delectus-api-server.couchbase.delectus.users :as delectus-users]
   [delectus-api-server.couchbase.route-handlers :as couch-handlers]
   [delectus-api-server.couchbase.travel-sample.route-handlers :as travel-handlers]
   [delectus-api-server.route-handlers :as handlers]
   [org.httpkit.server :as server]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.middleware.defaults :refer :all]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.util.response :refer [response redirect content-type]])
  (:gen-class))
          

;;; ---------------------------------------------------------------------
;;; routes
;;; ---------------------------------------------------------------------

(defroutes app-routes
  (GET "/" [] handlers/landing-page)

  ;; general test routes
  ;; -------------------
  (GET "/echo" [] handlers/echo)
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

  (GET "/delectus/login" [] delectus-handlers/login) 
  (GET "/delectus/userid" [] delectus-handlers/userid)
  (GET "/delectus/collections" [] delectus-handlers/collections)
  (GET "/delectus/lists" [] delectus-handlers/lists)
  (GET "/delectus/collection_named" [] delectus-handlers/collection-named)
  (GET "/delectus/collection_with_id" [] delectus-handlers/collection-with-id)
  (GET "/delectus/list_named" [] delectus-handlers/list-named)
  (GET "/delectus/list_with_id" [] delectus-handlers/list-with-id)
  
  ;; default ("Page not found") route
  ;; --------------------------------
  (route/not-found "Error, page not found!"))

;;; ---------------------------------------------------------------------
;;; main server program
;;; ---------------------------------------------------------------------

;;; hold a reference to a function that stops the running server
(defonce server (atom nil))

(defn -main [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "9000"))]
    ;; Run the server with Ring.defaults middleware
    (reset! server
            (server/run-server
             (wrap-authorization
              (wrap-authentication
               (wrap-defaults
                (wrap-cors #'app-routes
                           :access-control-allow-origin [#"http://localhost:5000" #"http://mars.local:5000"]
                           :access-control-allow-methods [:get :put :post :delete])
                site-defaults)
               (backends/jws {:secret (config/delectus-users-signing-secret)
                              :options {:alg :hs512}}))
              (backends/jws {:secret (config/delectus-users-signing-secret)
                             :options {:alg :hs512}}))
             {:port port}))
    ;; Run the server without ring defaults
    ;;(server/run-server #'app-routes {:port port})
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))

;;; (stop-server)
