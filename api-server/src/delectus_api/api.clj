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

            (GET "/echo" req
                 :return s/Str
                 :summary "echoes the request"
                 (handle-dump req))
            
            (POST "/login" req
                  :body [{:keys [email password]} schema/LoginRequest]
                  :return {:token s/Str}
                  :summary "authenticates a Delectus user"
                  (handlers/login email password))

            (GET "/userid/:email" req
                 :path-params [email :- s/Str]
                 :return s/Str
                 :summary "Returns the userid for the email address"
                 (handlers/userid email))

            (GET "/userdata/:id" req
                 :path-params [id :- s/Str]
                 :return schema/UserData
                 :summary "Returns name and email of the user with the id"
                 (handlers/userdata id))

            (GET "/collections/:email" req
                 :path-params [email :- s/Str]
                 :return [{s/Str s/Str}]
                 :summary "Returns the names and ids of collections that belong to the email address"
                 (handlers/collections email))

            (GET "/collection_with_id/:email/:id" req
                 :path-params [email :- s/Str id :- s/Str]
                 :return {s/Str s/Str}
                 :summary "Returns the name and id of the collection with id, if it belongs to the user"
                 (handlers/collection-with-id email id))

            (GET "/collection_name/:email/:id" req
                 :path-params [email :- s/Str id :- s/Str]
                 :return s/Str
                 :summary "Returns the name of the collection with id, if it belongs to the user"
                 (handlers/collection-name email id))

            (GET "/collection_named/:email/:name" req
                 :path-params [email :- s/Str name :- s/Str]
                 :return {s/Str s/Str}
                 :summary "Returns the collection named name, if it belongs to the user"
                 (handlers/collection-named email name))

            (POST "/rename_collection" req
                  :body [{:keys [email collectionid newname]} schema/CollectionRenameRequest]
                  :return s/Str
                  :summary "Renames the collection to the supplied name, if it belongs to the user"
                  (handlers/rename-collection email collectionid newname)))))
