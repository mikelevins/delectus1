(ns delectus-api.handler
  (:require
   [compojure.api.sweet :refer :all]
   [delectus-api.api :as api]
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
            :tags [{:name "api", :description "api endpoints"}]}}}

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
                  (api/login email password))

            (GET "/userid/:email" req
                 :path-params [email :- s/Str]
                 :return s/Str
                 :summary "Returns the userid for the email address"
                 (api/userid email))

            (GET "/userdata/:id" req
                 :path-params [id :- s/Str]
                 :return schema/UserData
                 :summary "Returns name and email of the user with the id"
                 (api/userdata id))

            (GET "/collections/:email" req
                 :path-params [email :- s/Str]
                 :return [{s/Str s/Str}]
                 :summary "Returns the names and ids of collections that belong to the email address"
                 (api/collections email))

            (GET "/collection_with_id/:email/:id" req
                 :path-params [email :- s/Str id :- s/Str]
                 :return {s/Str s/Str}
                 :summary "Returns the name and id of the collection with id, if it belongs to the user"
                 (api/collection-with-id email id))

            (GET "/collection_name/:email/:id" req
                 :path-params [email :- s/Str id :- s/Str]
                 :return s/Str
                 :summary "Returns the name of the collection with id, if it belongs to the user"
                 (api/collection-name email id)))))
