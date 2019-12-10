(ns delectus-api.handler
  (:require
   [buddy.hashers :as hashers]
   [compojure.api.sweet :refer :all]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
   ))

;;; (config/delectus-configuration)
;;; (config/delectus-users-signing-secret)
;;; (config/couchbase-cluster)
;;; (config/delectus-users-bucket)
;;; (config/delectus-content-bucket)

;;; temporary in-memory table of authorized users
(def +userdb+
  {"mikel@evins.net" true
   "greer@evins.net" true
   "granny@evins.net" true})


(defn email->user [email]
  (let [found (couchio/find-objects
               (config/delectus-users-bucket) []
               {+type-attribute+ +user-type+
                +email-attribute+ email})]
    (if (empty? found)
      nil
      (first found))))

;;; (email->user "mikel@evins.net")
;;; (email->user "greer@evins.net")
;;; (email->user "nobody@nowhere.net")

(defn make-auth-token [user-record]
  (str (.get user-record +email-attribute+) " logged in"))

(s/defschema LoginRequest
  {:email s/Str
   :password s/Str})

(defn authenticate-user [email password]
  (let [found-user (email->user email)]
    (if found-user
      (if (hashers/check password (.get found-user "password-hash"))
        {:token (make-auth-token found-user)}
        nil)
      nil)))

;;; (authenticate-user "mikel@evins.net" "foo")
;;; (authenticate-user "nobody@evins.net" "foo")

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

     (POST "/login" []
       :body [{:keys [email password]} LoginRequest]
       :return {:token s/Str}
       :summary "authenticates a Delectus user"
       (let [maybe-auth (authenticate-user email password)]
         (if maybe-auth
           (ok maybe-auth)
           (unauthorized "Login failed"))))

     )))


