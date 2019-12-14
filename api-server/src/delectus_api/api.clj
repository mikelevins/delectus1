(ns delectus-api.api
  (:require
   [compojure.api.sweet :refer :all]
   [delectus-api.handlers :as handlers]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.errors :as errors]
   [delectus-api.schema :as schema]
   [ring.handler.dump :refer [handle-dump]]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
   [tick.alpha.api :as t]
   ))

;;; TODO: change API endpoints to use user ID strings instead of email addresses

(def app
  (api
   {:swagger
    {:ui "/"
     :spec "/swagger.json"
     :data {:info {:title "Delectus-api"
                   :description "The Delectus 2 Database API"}
            :tags [{:name "api", :description "Delectus endpoints"}]}}}

   (context "/api" []
            :tags ["api"]

            ;; diagnostics
            ;; -------------------  
            
            (GET "/echo" req
                 :return s/Str
                 :summary "echoes the request"
                 (handle-dump req))
            
            ;; users
            ;; -------------------  

            (POST "/login" req
                  :body [{:keys [email password]} schema/LoginRequest]
                  :return {:token s/Str}
                  :summary "authenticates a Delectus user"
                  (handlers/login email password))

            (GET "/userid/:email" req
                 :path-params [email :- s/Str]
                 :return s/Str
                 :summary "Returns the userid"
                 (handlers/userid email))

            (GET "/userdata/:id" req
                 :path-params [id :- s/Str]
                 :return schema/UserData
                 :summary "Returns information avbout the user"
                 (handlers/userdata id))

            ;; collections
            ;; -------------------  

            (GET "/collections/:email" req
                 :path-params [email :- s/Str]
                 :return [{s/Str s/Str}]
                 :summary "Returns the collections that belong to the user"
                 (handlers/collections email))

            (GET "/collection_with_id/:email/:id" req
                 :path-params [email :- s/Str id :- s/Str]
                 :return {s/Str s/Str}
                 :summary "Returns the identified collection belonging to the user"
                 (handlers/collection-with-id email id))

            (GET "/collection_name/:email/:id" req
                 :path-params [email :- s/Str id :- s/Str]
                 :return s/Str
                 :summary "Returns the name of the collection"
                 (handlers/collection-name email id))

            (GET "/collection_named/:email/:name" req
                 :path-params [email :- s/Str name :- s/Str]
                 :return {s/Str s/Str}
                 :summary "Returns the named collection belonging to the user"
                 (handlers/collection-named email name))
            
            (POST "/rename_collection" req
                  :body [{:keys [email collectionid newname]} schema/CollectionRenameRequest]
                  :return s/Str
                  :summary "Renames the collection"
                  (handlers/rename-collection email collectionid newname))
            
            (POST "/new_collection" req
                  :body [{:keys [email name]} schema/NewCollectionRequest]
                  :return s/Str
                  :summary "Creates a new collection with the supplied name"
                  (handlers/new-collection email name))

            ;; (GET "/delectus/delete_collection" [] handlers/delete-collection)        
            ;; (GET "/delectus/undelete_collection" [] handlers/undelete-collection)    
            ;; (GET "/delectus/collection_deleted" [] handlers/collection-deleted?)        
            ;; (GET "/delectus/collection_lists" [] handlers/collection-lists)          
            ;; (GET "/delectus/collection_add_list" [] handlers/collection-add-list)
            ;; (GET "/delectus/collection_remove_list" [] handlers/collection-remove-list)

            ;; lists
            ;; -------------------  

            ;; (GET "/delectus/lists" [] handlers/lists)                                  
            ;; (GET "/delectus/list_with_id" [] handlers/list-with-id)                    
            ;; (GET "/delectus/list_name" [] handlers/list-name)
            ;; (GET "/delectus/list_named" [] handlers/list-named)                        
            ;; (GET "/delectus/rename_list" [] handlers/rename-list)
            ;; (GET "/delectus/new_list" [] handlers/new-list)                         
            ;; (GET "/delectus/delete_list" [] handlers/delete-list)                   
            ;; (GET "/delectus/undelete_list" [] handlers/undelete-list)               
            ;; (GET "/delectus/list_deleted" [] handlers/list-deleted?)                   
            ;; (GET "/delectus/list_columns" [] handlers/list-columns)
            ;; (GET "/delectus/new_column" [] handlers/new-column)
            ;; (GET "/delectus/column_with_id" [] handlers/column-with-id)
            ;; (GET "/delectus/column_name" [] handlers/column-name)
            ;; (GET "/delectus/column_named" [] handlers/column-named)
            ;; (GET "/delectus/column_deleted" [] handlers/column-deleted?)
            ;; (GET "/delectus/delete_column" [] handlers/delete-column)
            ;; (GET "/delectus/undelete_column" [] handlers/undelete-column)
            ;; (GET "/delectus/rename_column" [] handlers/rename-column)
            ;; (GET "/delectus/list_items" [] handlers/list-items)
            ;; (GET "/delectus/item_with_id" [] handlers/item-with-id)
            ;; (GET "/delectus/new_item" [] handlers/new-item)
            ;; (GET "/delectus/delete_item" [] handlers/delete-item)
            ;; (GET "/delectus/undelete_item" [] handlers/undelete-item)
            ;; (GET "/delectus/item_deleted" [] handlers/item-deleted?)
            ;; (GET "/delectus/item_column_value" [] handlers/item-column-value)
            ;; (GET "/delectus/set_item_column_value" [] handlers/set-item-column-value)

            )))
