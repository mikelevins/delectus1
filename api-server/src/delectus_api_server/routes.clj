(ns delectus-api-server.routes
  (:require
   [compojure.api.sweet :refer :all]
   ;; [compojure.core :refer :all]
   [compojure.route :as route]
   [delectus-api-server.handlers :as handlers]
   [ring.handler.dump :refer [handle-dump]]
   [ring.middleware.resource :refer [wrap-resource]]
   [ring.middleware.content-type :refer [wrap-content-type]]
   [ring.util.response :refer [redirect]]
   [schema.core :as s])
  (:gen-class))

;;; ---------------------------------------------------------------------
;;; routes
;;; ---------------------------------------------------------------------

;;; TODO: enable authentication for the API
;;; so random people can't use the API to modify db data!
(def api-routes
  (api
   {:swagger
    {:ui "/api"
     :spec "/api/swagger.json"
     :data {:info {:title "Delectus 2"
                   :description "The REST API"} 
            :tags [{:name "api", :description "api endpoints"}]}}}

   (context "/api" []
     :tags ["api"]

     (GET "/echo" req
       :return s/Str
       :summary "echoes the request"
       (handle-dump req))

     )))

(defroutes app-routes
  ;; default landing
  (GET "/" [] (redirect "index.html"))

  ;; sessions
  ;; -------------------  

  (GET "/delectus/login" [] handlers/login)
  (GET "/delectus/logout" [] handlers/logout)
  (GET "/delectus/userid" [] handlers/userid)
  (GET "/delectus/user" [] handlers/user)

  ;; collections
  ;; -------------------  

  (GET "/delectus/collections" [] handlers/collections)
  (GET "/delectus/collection_with_id" [] handlers/collection-with-id)
  (GET "/delectus/collection_name" [] handlers/collection-name)
  (GET "/delectus/collection_named" [] handlers/collection-named)
  (GET "/delectus/rename_collection" [] handlers/rename-collection)
  (GET "/delectus/new_collection" [] handlers/new-collection)
  (GET "/delectus/delete_collection" [] handlers/delete-collection)        
  (GET "/delectus/undelete_collection" [] handlers/undelete-collection)    
  (GET "/delectus/collection_deleted" [] handlers/collection-deleted?)        
  (GET "/delectus/collection_lists" [] handlers/collection-lists)          
  (GET "/delectus/collection_add_list" [] handlers/collection-add-list)
  (GET "/delectus/collection_remove_list" [] handlers/collection-remove-list)

  ;; lists
  ;; -------------------  

  (GET "/delectus/lists" [] handlers/lists)                                  
  (GET "/delectus/list_with_id" [] handlers/list-with-id)                    
  (GET "/delectus/list_name" [] handlers/list-name)
  (GET "/delectus/list_named" [] handlers/list-named)                        
  (GET "/delectus/rename_list" [] handlers/rename-list)
  (GET "/delectus/new_list" [] handlers/new-list)                         
  (GET "/delectus/delete_list" [] handlers/delete-list)                   
  (GET "/delectus/undelete_list" [] handlers/undelete-list)               
  (GET "/delectus/list_deleted" [] handlers/list-deleted?)                   
  (GET "/delectus/list_columns" [] handlers/list-columns)
  (GET "/delectus/new_column" [] handlers/new-column)
  (GET "/delectus/column_with_id" [] handlers/column-with-id)
  (GET "/delectus/column_name" [] handlers/column-name)
  (GET "/delectus/column_named" [] handlers/column-named)
  (GET "/delectus/column_deleted" [] handlers/column-deleted?)
  (GET "/delectus/delete_column" [] handlers/delete-column)
  (GET "/delectus/undelete_column" [] handlers/undelete-column)
  (GET "/delectus/rename_column" [] handlers/rename-column)
  (GET "/delectus/list_items" [] handlers/list-items)
  (GET "/delectus/item_with_id" [] handlers/item-with-id)
  (GET "/delectus/new_item" [] handlers/new-item)
  (GET "/delectus/delete_item" [] handlers/delete-item)
  (GET "/delectus/undelete_item" [] handlers/undelete-item)
  (GET "/delectus/item_deleted" [] handlers/item-deleted?)
  (GET "/delectus/item_column_value" [] handlers/item-column-value)
  (GET "/delectus/set_item_column_value" [] handlers/set-item-column-value)
  
  ;; default ("Page not found") route
  ;; --------------------------------
  (route/not-found "Error, page not found!"))

(def router
  (routes
   api-routes
   (wrap-content-type
    (wrap-resource app-routes "public"))))
