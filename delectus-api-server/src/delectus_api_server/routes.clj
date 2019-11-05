(ns delectus-api-server.routes
  (:require
   [compojure.core :refer :all]
   [compojure.route :as route]
   [delectus-api-server.handlers :as handlers])
  (:gen-class))

;;; ---------------------------------------------------------------------
;;; routes
;;; ---------------------------------------------------------------------

(defroutes app-routes

  ;; general test routes
  ;; -------------------
  (GET "/echo" [] handlers/echo)
  (GET "/status" [] handlers/status)

  ;; Delectus API routes
  ;; -------------------

  (GET "/delectus/login" [] handlers/login) 
  (GET "/delectus/userid" [] handlers/userid)
  (GET "/delectus/collections" [] handlers/collections)
  (GET "/delectus/lists" [] handlers/lists)
  (GET "/delectus/collection_named" [] handlers/collection-named)
  (GET "/delectus/collection_with_id" [] handlers/collection-with-id)
  (GET "/delectus/list_named" [] handlers/list-named)
  (GET "/delectus/list_with_id" [] handlers/list-with-id)
  
  ;; default ("Page not found") route
  ;; --------------------------------
  (route/not-found "Error, page not found!"))
