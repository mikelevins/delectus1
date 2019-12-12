(ns delectus-api-server.core
  (:require
   ;;[compojure.core :refer :all]
   [delectus-api-server.routes :refer [router]]
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
  (let [port (Integer/parseInt (or (System/getenv "PORT") "9000"))
        delectus-security (assoc (:security site-defaults)
                                 :anti-forgery false)
        delectus-site-defaults (assoc site-defaults :security delectus-security)]
    ;; Run the server with Ring.defaults middleware
    (reset! server
            (server/run-server
             (wrap-defaults
              (wrap-cors #'router
                         :access-control-allow-origin [#".*"]
                         :access-control-allow-methods [:get :put :post :delete])
              ;; disable anti-forgery until api routes work
              delectus-site-defaults)
             {:port port}))
    ;; Run the server without ring defaults
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

