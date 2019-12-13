(ns delectus-api.handler
  (:require
   [compojure.api.sweet :refer :all]
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


;;; ---------------------------------------------------------------------
;;; the api
;;; ---------------------------------------------------------------------

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
                  (let [remote-addr (:remote-addr req)
                        maybe-auth (authenticate-user email password)]
                    (if maybe-auth
                      (ok {:token (make-auth-token maybe-auth remote-addr)})
                      (unauthorized "Login failed"))))

            (GET "/userid/:email" req
                 :path-params [email :- s/Str]
                 :return s/Str
                 :summary "Returns the userid for the email address"
                 (let [found-user (couchio/email->user email)]
                   (if found-user
                     (ok (.get found-user +id-attribute+))
                     (not-found "No such user"))))

            (GET "/userdata/:id" req
                 :path-params [id :- s/Str]
                 :return schema/UserData
                 :summary "Returns name and email of the user with the id"
                 (let [found-user (couchio/id->user id)]
                   (if found-user
                     (ok {:id id
                          :name (.get found-user +name-attribute+)
                          :email (.get found-user +email-attribute+)})
                     (not-found "No such user"))))

            (GET "/collections/:email" req
                 :path-params [email :- s/Str]
                 :return [{s/Str s/Str}]
                 :summary "Returns the names and ids of collections that belong to the email address"
                 (let [userid (couchio/email->userid email)]
                   (if userid
                     (let [collections (couchio/find-objects
                                        (config/delectus-content-bucket) []
                                        {"type" +collection-type+
                                         "owner-id" userid})]
                       (if collections
                         (let [collection-maps (map #(.toMap %) collections)
                               descriptions (map #(select-keys % ["name" "id"]) collection-maps)]
                           (ok descriptions))
                         (ok [])))
                     (not-found "No such user"))))

            (GET "/collection_with_id/:email/:id" req
                 :path-params [email :- s/Str id :- s/Str]
                 :return [{s/Str s/Str}]
                 :summary "Returns the name and id of the collection with id, if it belongs to the user"
                 (let [userid (couchio/email->userid email)]
                   (if userid
                     (let [collections (couchio/find-objects
                                        (config/delectus-content-bucket) []
                                        {"type" +collection-type+
                                         "owner-id" userid
                                         "id" id})]
                       (if collections
                         (let [collection-maps (map #(.toMap %) collections)
                               descriptions (map #(select-keys % ["name" "id"]) collection-maps)]
                           (ok descriptions))
                         (ok [])))
                     (not-found "No such user"))))

            (GET "/collection_name/:email/:id" req
                 :path-params [email :- s/Str id :- s/Str]
                 :return s/Str
                 :summary "Returns the name of the collection with id, if it belongs to the user"
                 (let [userid (couchio/email->userid email)]
                   (if userid
                     (let [collections (couchio/find-objects
                                        (config/delectus-content-bucket) []
                                        {"type" +collection-type+
                                         "owner-id" userid
                                         "id" id})]
                       (if collections
                         (let [collection (first collections)
                               name (.get collection +name-attribute+)]
                           (ok name))
                         (not-found "No such collection")))
                     (not-found "No such user")))))))

;;; (def $userid (couchio/email->userid "mikel@evins.net"))
;;; (def $collections (couchio/find-objects (config/delectus-content-bucket) [] {"type" +collection-type+}))
