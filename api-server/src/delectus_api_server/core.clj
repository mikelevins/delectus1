(ns delectus-api-server.core
  (:require
   [compojure.core :refer :all]
   [delectus-api-server.logging :refer [disable-logging enable-logging logging-enabled? wrap-logger]]
   [delectus-api-server.routes :refer [app-routes]]
   [org.httpkit.server :as server]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.middleware.defaults :refer :all]
   [ring.middleware.params :refer :all]
   [ring.middleware.session :refer :all])
  (:gen-class))

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
             (wrap-logger
              (wrap-session
              (wrap-params
               (wrap-defaults
                (wrap-cors #'app-routes
                           ;;:access-control-allow-origin [#"http://localhost:5000" #"http://mars.local:5000"]
                           :access-control-allow-origin [#".*"]
                           :access-control-allow-methods [:get :put :post :delete])
                site-defaults))
              ;; sessions last 1 hour
              {:cookie-attrs {:max-age 3600}}))
             {:port port}))
    ;; Run the server without ring defaults
    ;;(server/run-server #'app-routes {:port port})
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))

;;; (-main)

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))

;;; (-main)
;;; (stop-server)
