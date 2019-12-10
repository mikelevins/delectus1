(ns delectus-api.handler
  (:require
   [buddy.core.bytes :as bytes]
   [buddy.core.nonce :as nonce]
   [buddy.hashers :as hashers]
   [buddy.sign.jws :as jws]
   [buddy.sign.jwt :as jwt]
   [compojure.api.sweet :refer :all]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
   [tick.alpha.api :as t]))


(s/defschema LoginRequest
  {:email s/Str
   :password s/Str})

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
  (let [userid (.get user-record "id")]
    {:email (.get user-record +email-attribute+)
     :userid userid
     :timestamp (str (t/now))
     ;; in seconds; default 1 hour
     :expiration 3600}))

;;; tokens
;;; ---------------------------------------------------------------------
;;; the token works right if $auth-map-2 equals $auth-map-1
;;;
;;; (def $auth-map-1 (make-auth-token (email->user "mikel@evins.net")))
;;; (def $jwt-key (nonce/random-bytes 32))
;;; (def $token (jwt/encrypt $auth-map-1 $jwt-key))
;;; (def $auth-map-2 (jwt/decrypt $token $jwt-key))
;;; (= $auth-map-1 $auth-map-2)

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


