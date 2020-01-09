(ns delectus-api.routes
  (:require
   [clj-time.core :as t]
   [compojure.api.sweet :refer :all]
   [delectus-api.handlers :as handlers]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.errors :as errors]
   [delectus-api.schema :as schema]
   [ring.handler.dump :refer [handle-dump]]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
   ))

;;; TODO:
;;; 1. auth tokens
;;; Consider using an authentication token to authorize each API
;;; endpoint except the one used to obtain the token
;;;
;;; 2. REST verbs
;;; Review the API endpoints; some may be better expressed as
;;; different REST verbs

(def router
  (api
   {:swagger
    {:ui "/"
     :spec "/swagger.json"
     :data {:info {:version "0.0.2"
                   :title "Delectus-api"
                   :description "The Delectus 2 Database API"}
            :tags [{:name "api/diagnostic", :description "Information about the server"}
                   {:name "api/user", :description "Operations on user accounts"}
                   {:name "api/collection", :description "Operations on collections"}
                   {:name "api/list", :description "Operations on lists"}]}}}

   (context "/api/diagnostic" [] :tags ["api/diagnostic"]

            (GET "/echo" req
                 :return s/Str
                 :summary "echoes the request"
                 (handle-dump req))

            (GET "/motd" req
                 :return s/Str
                 :summary "returns the API server's message of the day"
                 (handlers/motd)))
   
   (context "/api/user" [] :tags ["api/user"]

            (POST "/authenticate" req
                  :body [{:keys [userid password]} schema/AuthenticationRequest]
                  :return {:token s/Str}
                  :summary "authenticates a Delectus user"
                  (handlers/authenticate userid password))

            (POST "/login" req
                  :body [{:keys [email password]} schema/LoginRequest]
                  :return {:token s/Str}
                  :summary "Logs in a user by email address"
                  (handlers/login email password))

            (GET "/userid/:email" req
                 :path-params [email :- s/Str]
                 :return s/Str
                 :summary "Returns the userid associated with the email address"
                 (handlers/userid email))

            (GET "/userdata/:userid" req
                 :path-params [userid :- s/Str]
                 :return schema/UserData
                 :summary "Returns information about the identified user"
                 (handlers/userdata userid)))

   (context "/api/collection" [] :tags ["api/collection"]

            (GET "/collections/:userid" req
                  :path-params [userid :- s/Str]
                  :return [schema/CollectionMap]
                  :summary "Returns the user's collections"
                  (handlers/collections userid []))

            (POST "/collections" req
                  :body-params [userid :- s/Str
                                {fields :- [s/Str] []}]
                  :return [schema/CollectionMap]
                  :summary "Returns selected fields of the user's collections"
                  (handlers/collections userid fields))

            (GET "/collection_with_id/:userid/:collectionid" req
                 :path-params [userid :- s/Str
                               collectionid :- s/Str]
                 :return schema/CollectionMap
                 :summary "Returns the identified collection"
                 (handlers/collection-with-id userid collectionid []))

            (POST "/collection_with_id" req
                  :body-params [userid :- s/Str
                                collectionid :- s/Str
                                {fields :- [s/Str] []}]
                 :return schema/CollectionMap
                 :summary "Returns selected fields of the identified collection"
                 (handlers/collection-with-id userid collectionid fields))

            (GET "/collection_name/:userid/:collectionid" req
                 :path-params [userid :- s/Str collectionid :- s/Str]
                 :return s/Str
                 :summary "Returns the name of the collection"
                 (handlers/collection-name userid collectionid))

            (GET "/find_collections_with_name/:userid/:name" req
                 :path-params [userid :- s/Str
                               name :- s/Str]
                 :return [schema/CollectionMap]
                 :summary "Returns the named collections"
                 (handlers/find-collections-with-name userid name []))

            (POST "/find_collections_with_name" req
                  :body-params [userid :- s/Str
                                name :- s/Str
                                {fields :- [s/Str] []}]
                  :return [schema/CollectionMap]
                  :summary "Returns selected fields of the named collections"
                  (handlers/find-collections-with-name userid name fields))
            
            (POST "/rename_collection" req
                  :body [{:keys [userid collectionid newname]} schema/CollectionRenameRequest]
                  :return s/Str
                  :summary "Renames the collection"
                  (handlers/rename-collection userid collectionid newname))
            
            (POST "/new_collection" req
                  :body [{:keys [userid name]} schema/NewCollectionRequest]
                  :return s/Str
                  :summary "Creates a new collection with the supplied name"
                  (handlers/new-collection userid name))

            (POST "/delete_collection" req
                  :body [{:keys [userid collectionid]} schema/DeleteCollectionRequest]
                  :return s/Str
                  :summary "Marks a collection deleted"
                  (handlers/delete-collection userid collectionid))

            (POST "/undelete_collection" req
                  :body [{:keys [userid collectionid]} schema/DeleteCollectionRequest]
                  :return s/Str
                  :summary "Marks a collection not deleted"
                  (handlers/undelete-collection userid collectionid))

            (GET "/collection_deleted/:userid/:collectionid" req
                 :path-params [userid :- s/Str collectionid :- s/Str]
                 :return s/Bool
                 :summary "Returns true if the collection is marked deleted, and false otherwise"
                 (handlers/collection-deleted? userid collectionid))
            
            (GET "/collection_lists/:userid/:collectionid" req
                 :path-params [userid :- s/Str
                               collectionid :- s/Str]
                 :return [schema/ListMap]
                 :summary "Returns the collection's lists"
                 (handlers/collection-lists userid collectionid []))

            (POST "/collection_lists" req
                  :body-params [userid :- s/Str
                                collectionid :- s/Str
                                {fields :- [s/Str] []}]
                 :return [schema/ListMap]
                 :summary "Returns selected fields of the collection's lists"
                 (handlers/collection-lists userid collectionid fields)))

   (context "/api/list" [] :tags ["api/list"]

            (GET "/lists/:userid" req
                 :path-params [userid :- s/Str]
                 :return [schema/ListMap]
                 :summary "Returns the user's lists"
                 (handlers/lists userid []))

            (POST "/lists" req
                  :body-params [userid :- s/Str
                                {fields :- [s/Str] []}]
                  :return [schema/ListMap]
                  :summary "Returns selected fields of the user's lists"
                  (handlers/lists userid fields))

            (POST "/move_list_to_collection" req
                  :body [{:keys [userid listid collectionid]} schema/ListMoveToCollectionRequest]
                  :return s/Str
                  :summary "Makes the list a member of the collection"
                  (handlers/move-list-to-collection userid listid collectionid))

            (POST "/make_list_uncollected" req
                  :body [{:keys [userid listid]} schema/ListMakeUncollectedRequest]
                  :return nil
                  :summary "Makes the list a member of no collection"
                  (handlers/make-list-uncollected userid listid))
            
            (GET "/list_with_id/:userid/:listid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str]
                 :return schema/ListMap
                 :summary "Returns the identified list"
                 (handlers/list-with-id userid listid []))
            
            (POST "/list_with_id" req
                  :body-params [userid :- s/Str
                                listid :- s/Str
                                {fields :- [s/Str] []}]
                  :return schema/ListMap
                  :summary "Returns selected fields of the identified list"
                  (handlers/list-with-id userid listid fields))

            (GET "/list_name/:userid/:listid" req
                 :path-params [userid :- s/Str listid :- s/Str]
                 :return s/Str
                 :summary "Returns the name of the identified list"
                 (handlers/list-name userid listid))

            (GET "/find_lists_with_name/:userid/:name" req
                 :path-params [userid :- s/Str
                               name :- s/Str]
                 :return [schema/ListMap]
                 :summary "Returns the named lists"
                 (handlers/find-lists-with-name userid name []))

            (POST "/find_lists_with_name" req
                  :body-params [userid :- s/Str
                                name :- s/Str
                                {fields :- [s/Str] []}]
                  :return [schema/ListMap]
                  :summary "Returns selected fields of the named lists"
                  (handlers/find-lists-with-name userid name fields))
            
            (POST "/rename_list" req
                  :body [{:keys [userid listid newname]} schema/ListRenameRequest]
                  :return s/Str
                  :summary "Renames the list"
                  (handlers/rename-list userid listid newname))
            
            (POST "/new_list" req
                  :body [{:keys [userid name]} schema/NewListRequest]
                  :return s/Str
                  :summary "Creates a new list with the supplied name"
                  (handlers/new-list userid name))

            (POST "/delete_list" req
                  :body [{:keys [userid listid]} schema/DeleteListRequest]
                  :return s/Str
                  :summary "Marks a list deleted"
                  (handlers/delete-list userid listid))

            (POST "/undelete_list" req
                  :body [{:keys [userid listid]} schema/DeleteListRequest]
                  :return s/Str
                  :summary "Marks a list not deleted"
                  (handlers/undelete-list userid listid))

            (GET "/list_deleted/:userid/:listid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str]
                 :return s/Bool
                 :summary "Returns true if the list is marked deleted, and false otherwise"
                 (handlers/list-deleted? userid listid))

            (GET "/list_columns/:userid/:listid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str]
                 :return {s/Str s/Any}
                 :summary "Returns the list's columns"
                 (handlers/list-columns userid listid))

            (POST "/new_column" req
                  :body-params [userid :- s/Str
                                listid :- s/Str
                                name :- s/Str]
                  :return s/Str
                  :summary "Creates a new column with the supplied name"
                  (handlers/new-column userid listid name))

            (GET "/column_with_id/:userid/:listid/:columnid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str
                               columnid :- s/Str]
                 :return (s/maybe {s/Str s/Any})
                 :summary "Returns the identified column"
                 (handlers/column-with-id userid listid columnid))

            (GET "/column_name/:userid/:listid/:columnid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str
                               columnid :- s/Str]
                 :return (s/maybe s/Str)
                 :summary "Returns the name of the identified column"
                 (handlers/column-name userid listid columnid))

            (GET "/column_named/:userid/:listid/:name" req
                 :path-params [userid :- s/Str
                               listid :- s/Str
                               name :- s/Str]
                 :return (s/maybe {s/Str s/Any})
                 :summary "Returns the named column"
                 (handlers/column-named userid listid name))

            (GET "/column_deleted/:userid/:listid/:columnid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str
                               columnid :- s/Str]
                 :return (s/maybe s/Bool)
                 :summary "Returns true if the column is marked deleted, and false otherwise"
                 (handlers/column-deleted userid listid columnid))

            (POST "/delete_column" req
                  :body-params [userid :- s/Str
                                listid :- s/Str
                                columnid :- s/Str]
                  :return s/Str
                  :summary "Marks the identified column deleted"
                  (handlers/delete-column userid listid columnid))            

            (POST "/undelete_column" req
                  :body-params [userid :- s/Str
                                listid :- s/Str
                                columnid :- s/Str]
                  :return s/Str
                  :summary "Marks the identified column not deleted"
                  (handlers/undelete-column userid listid columnid))            

            (POST "/delete_column" req
                  :body-params [userid :- s/Str
                                listid :- s/Str
                                columnid :- s/Str]
                  :return s/Str
                  :summary "Marks the identified column deleted"
                  (handlers/delete-column userid listid columnid))            

            (POST "/rename_column" req
                  :body-params [userid :- s/Str
                                listid :- s/Str
                                columnid :- s/Str
                                name :- s/Str]
                  :return s/Str
                  :summary "Renames the identified column"
                  (handlers/rename-column userid listid columnid name))            

            (POST "/list_items" req
                  :body-params [userid :- s/Str
                                listid :- s/Str
                                {offset :- s/Int 0}
                                {limit :- s/Int 100}]
                  :return [{s/Str s/Any}]
                  :summary "Returns items from the identified list"
                  (handlers/list-items userid listid :offset offset :limit limit))            

            (GET "/list_item_with_id/:userid/:listid/:itemid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str
                               itemid :- s/Str]
                 :return (s/maybe {s/Str s/Any})
                 :summary "Returns the identified item"
                 (handlers/list-item-with-id userid listid itemid))

            (POST "/new_list_item" req
                  :body-params [userid :- s/Str
                                listid :- s/Str]
                  :return s/Str
                  :summary "Adds a new item to the list"
                  (handlers/new-list-item userid listid))

            
            (POST "/delete_list_item/:userid/:listid/:itemid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str
                               itemid :- s/Str]
                 :return s/Str
                 :summary "Marks the identified item deleted"
                 (handlers/delete-list-item userid listid itemid))
            
            (POST "/undelete_list_item/:userid/:listid/:itemid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str
                               itemid :- s/Str]
                 :return s/Str
                 :summary "Marks the identified item not deleted"
                 (handlers/undelete-list-item userid listid itemid))

            (GET "/list_item_deleted/:userid/:listid/:itemid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str
                               itemid :- s/Str]
                 :return s/Bool
                 :summary "Returns true if the identified item is marked deleted, and false otherwise"
                 (handlers/list-item-deleted userid listid itemid))

            (GET "/item_column_value/:userid/:listid/:itemid/:columnid" req
                 :path-params [userid :- s/Str
                               listid :- s/Str
                               itemid :- s/Str
                               columnid :- s/Int]
                 :return (s/maybe s/Str)
                 :summary "Returns the value from the column of the item"
                 (handlers/item-column-value userid listid itemid columnid))

            (POST "/set_item_column_value" req
                  :body-params [userid :- s/Str
                                listid :- s/Str
                                itemid :- s/Str
                                columnid :- s/Int
                                value :- s/Str]
                  :return (s/maybe s/Str)
                  :summary "Updates the value in the column of the item"
                  (handlers/set-item-column-value! userid listid itemid columnid value))
            
            )))

(def app
  (wrap-cors router
             :access-control-allow-origin [#".*"]
             :access-control-allow-methods [:get :put :post :delete]))
