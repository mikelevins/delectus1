(ns delectus-api.routes
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

;;; TODO:
;;; Consider using an authentication token to authorize each API
;;; endpoint except the one used to obtain the token

(def app
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
                 (handle-dump req)))

   
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
                 :summary "Returns the userid"
                 (handlers/userid email))

            (GET "/userdata/:userid" req
                 :path-params [userid :- s/Str]
                 :return schema/UserData
                 :summary "Returns information about the user"
                 (handlers/userdata userid)))

   (context "/api/collection" [] :tags ["api/collection"]

            (POST "/collections" req
                  :body-params [userid :- s/Str
                                {fields :- [s/Str] []}]
                  :return [schema/CollectionMap]
                  :summary "Returns the collections that belong to the user"
                  (handlers/collections userid fields))

            (POST "/collection_with_id" req
                  :body-params [userid :- s/Str
                                collectionid :- s/Str
                                {fields :- [s/Str] []}]
                 :return schema/CollectionMap
                 :summary "Returns the identified collection belonging to the user"
                 (handlers/collection-with-id userid collectionid fields))

            (GET "/collection_name/:userid/:collectionid" req
                 :path-params [userid :- s/Str collectionid :- s/Str]
                 :return s/Str
                 :summary "Returns the name of the collection"
                 (handlers/collection-name userid collectionid))

            (GET "/find_collection_with_name/:userid/:name" req
                 :path-params [userid :- s/Str name :- s/Str]
                 :return [s/Str]
                 :summary "Returns the named collection belonging to the user"
                 (handlers/find-collection-with-name userid name))
            
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
                 :summary "Returns true if the collection has been marked deleted, and false otherwise"
                 (handlers/collection-deleted? userid collectionid))

            (GET "/collection_lists/:userid/:collectionid" req
                 :path-params [userid :- s/Str collectionid :- s/Str]
                 :return [{s/Str s/Str}]
                 :summary "Returns the collection's lists"
                 (handlers/collection-lists userid collectionid)))

   (context "/api/list" [] :tags ["api/list"]

            (GET "/lists/:userid" req
                 :path-params [userid :- s/Str]
                 :return [{(s/required-key "name") s/Str
                           (s/required-key "id") s/Str
                           (s/required-key "collection") (s/maybe s/Str)
                           (s/required-key "deleted") (s/maybe s/Bool)}]
                 :summary "Returns the lists that belong to the user"
                 (handlers/lists userid))

            (POST "/move_list_to_collection" req
                  :body [{:keys [userid listid collectionid]} schema/ListMoveToCollectionRequest]
                  :return s/Str
                  :summary "Adds the list to the collection"
                  (handlers/move-list-to-collection userid listid collectionid))

            (POST "/make_list_uncollected" req
                  :body [{:keys [userid listid]} schema/ListMakeUncollectedRequest]
                  :return nil
                  :summary "Moves the list to no collection"
                  (handlers/make-list-uncollected userid listid))

            (GET "/list_with_id/:userid/:listid" req
                 :path-params [userid :- s/Str listid :- s/Str]
                 :return {s/Str s/Str}
                 :summary "Returns the identified list belonging to the user"
                 (handlers/list-with-id userid listid))

            (GET "/list_name/:userid/:listid" req
                 :path-params [userid :- s/Str listid :- s/Str]
                 :return s/Str
                 :summary "Returns the name of identified list belonging to the user"
                 (handlers/list-name userid listid))

            (GET "/find_list_with_name/:userid/:name" req
                 :path-params [userid :- s/Str name :- s/Str]
                 :return {s/Str s/Str}
                 :summary "Returns the named list belonging to the user"
                 (handlers/find-list-with-name userid name))
            
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
                 :path-params [userid :- s/Str listid :- s/Str]
                 :return s/Bool
                 :summary "Returns true if the list has been marked deleted, and false otherwise"
                 (handlers/list-deleted? userid listid))

            (GET "/list_columns/:userid/:listid" req
                 :path-params [userid :- s/Str listid :- s/Str]
                 :return [{s/Str s/Str}]
                 :summary "Returns the list's columns"
                 (handlers/list-columns userid listid))
            
            ;; (POST "/new_column" req
            ;;       :body [{:keys [userid listid name]} schema/NewColumnRequest]
            ;;       :return s/Str
            ;;       :summary "Creates a new column with the supplied name"
            ;;       (handlers/new-column userid listid name))

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

