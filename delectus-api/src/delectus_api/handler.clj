(ns delectus-api.handler
  (:require
   [buddy.auth :refer [authenticated?]]
   [buddy.auth.backends.token :refer [jwe-backend]]
   [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]
   [buddy.core.bytes :as bytes]
   [buddy.core.nonce :as nonce]
   [buddy.hashers :as hashers]
   [buddy.sign.jwt :as jwt]
   [clj-time.core :as time]
   [clojure.pprint :refer [cl-format]]
   [clojure.data.json :as json]
   [compojure.api.sweet :refer :all] 
   [compojure.response :refer [render]]
   [compojure.route :as route]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [ring.adapter.jetty :as jetty]
   [ring.handler.dump :refer [handle-dump]]
   [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
   [tick.alpha.api :as t]
   ))

(def secret (nonce/random-bytes 32))

;;; ---------------------------------------------------------------------
;;; schemata
;;; ---------------------------------------------------------------------

(s/defschema LoginRequest
  {:email s/Str
   :password s/Str})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Controllers                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global var that stores valid users with their
;; respective passwords.

(def authdata {:admin "secret"
               :test "secret"})

(def auth-backend (jwe-backend {:secret secret
                                :options {:alg :a256kw :enc :a128gcm}}))

(def userdata {"admin" "12345"
               "test" "98765"})

;;; (get userdata "admin")

;;; ---------------------------------------------------------------------
;;; the api
;;; ---------------------------------------------------------------------


(def app-routes
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
       :body [{:keys [email password]} LoginRequest]
       :return {:token s/Str}
       :summary "authenticates a Delectus user"
       (let [valid? (some-> authdata
                            (get (keyword email))
                            (= password))]
         (do (cl-format true "~%email: ~S~%password: ~S~%authdata: ~S"
                        email password authdata)
             (if valid?
               (let [claims {:user (keyword email)
                             :exp (time/plus (time/now) (time/seconds 3600))}
                     token (jwt/encrypt claims secret {:alg :a256kw :enc :a128gcm})]
                 (ok {:token token}))
               (unauthorized)))))

     (GET "/userid/:email" req
       :path-params [email :- s/Str]
       :header-params [authorization :- s/Str]
       :return s/Str
       :summary "fetches the userid for the offered email address"
       (if-not (authenticated? req)
         (unauthorized (str "Unauthorized user: " email))
         (ok (get userdata email))))

     )))


(def app
  (as-> app-routes $
    (wrap-authorization $ auth-backend)
    (wrap-authentication $ auth-backend)))
