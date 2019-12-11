(ns delectus-api.handler
  (:require
   [buddy.core.bytes :as bytes]
   [buddy.core.nonce :as nonce]
   [buddy.hashers :as hashers]
   [buddy.sign.jws :as jws]
   [buddy.sign.jwt :as jwt]
   [clojure.data.json :as json]
   [clojure.pprint :as pp]
   [compojure.api.sweet :refer :all]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [ring.handler.dump :refer [handle-dump]]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
   [tick.alpha.api :as t]))

;;; ---------------------------------------------------------------------
;;; schemata
;;; ---------------------------------------------------------------------

(s/defschema LoginRequest
  {:email s/Str
   :password s/Str})

;;; ---------------------------------------------------------------------
;;; finding registered users
;;; ---------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------
;;; auth
;;; ---------------------------------------------------------------------

;;; tokens
;;; ---------------------------------------------------------------------
;;; the token works right if $auth-map-2 equals $auth-map-1
;;;
;;; (def $auth-map-1 (make-auth-token (email->user "mikel@evins.net")))
;;; (def $jwt-key (nonce/random-bytes 32))
;;; (def $token (jwt/encrypt $auth-map-1 $jwt-key))
;;; (def $auth-map-2 (jwt/decrypt $token $jwt-key))
;;; (= $auth-map-1 $auth-map-2)

(defn make-auth-map [user-record remote-addr]
  (let [userid (.get user-record "id")]
    {;; identifies the logged-in account
     :userid userid
     ;; identifies the account the client thinks it's authenticating
     :email (.get user-record +email-attribute+)
     ;; used to prevent a different device from hijacking a token
     :remote-addr remote-addr
     ;; designates when the authentication became valid
     :timestamp (str (t/now))
     ;; specifies how long it lasts; in seconds; default 1 hour
     :expiration 3600}))

(defonce +jwt-secret+ (nonce/random-bytes 32))

(defn auth-map->token [auth-map]
  (jwt/encrypt auth-map +jwt-secret+))

(defn make-auth-token [user-record remote-addr]
  (auth-map->token
   (make-auth-map user-record remote-addr)))

(defn decode-auth-token [token]
  (jwt/decrypt token +jwt-secret+))

;;; (def $token (make-auth-token (email->user "mikel@evins.net") "127.0.0.1"))
;;; (def $token-map (decode-auth-token $token))
;;; (decode-auth-token "foo")

(defn authenticate-user [email password]
  (let [found-user (email->user email)]
    (if found-user
      (if (hashers/check password (.get found-user +password-hash-attribute+))
        found-user
        nil)
      nil)))

;;; (authenticate-user "mikel@evins.net" "foo")
;;; (authenticate-user "nobody@evins.net" "foo")

(defn authorized? [req]
  (let [headers (:headers req)
        authorization (get headers "authorization")]
    (pp/cl-format true "~%Authorization: ~S~%" authorization)
    true))

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
       :body [{:keys [email password]} LoginRequest]
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
       :summary "Returns the userid for the offered email address"
       (if (authorized? req)
         (let [found-user (email->user email)]
           (if found-user
             (ok (.get found-user +id-attribute+))
             (not-found (str "No such user: " email))))
         (unauthorized))))))

