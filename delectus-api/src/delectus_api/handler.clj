(ns delectus-api.handler
  (:require
   [buddy.core.nonce :as nonce]
   [buddy.hashers :as hashers]
   [buddy.sign.jwt :as jwt]
   [compojure.api.sweet :refer :all]
   [delectus-api.configuration :as config]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.errors :as errors]
   [ring.handler.dump :refer [handle-dump]]
   [ring.util.http-response :refer :all]
   [schema.core :as s]
   [tick.alpha.api :as t]
   ))

;;; ---------------------------------------------------------------------
;;; schemata
;;; ---------------------------------------------------------------------

(s/defschema LoginRequest
  {:email s/Str
   :password s/Str})

(s/defschema UserData
  {:id s/Str
   :email s/Str
   :name s/Str})

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


(defn email->userid [email]
  (let [found (couchio/find-objects
               (config/delectus-users-bucket) []
               {+type-attribute+ +user-type+
                +email-attribute+ email})]
    (if (empty? found)
      nil
      (.get (first found) +id-attribute+))))

;;; (email->userid "mikel@evins.net")
;;; (email->userid "nobody@evins.net")

(defn id->user [userid]
  (couchio/get-user userid))

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
                 :summary "Returns the userid for the email address"
                 (let [found-user (email->user email)]
                   (if found-user
                     (ok (.get found-user +id-attribute+))
                     (not-found "No such user"))))

            (GET "/userdata/:id" req
                 :path-params [id :- s/Str]
                 :return UserData
                 :summary "Returns name and email of the user with the id"
                 (let [found-user (id->user id)]
                   (if found-user
                     (ok {:id id
                          :name (.get found-user +name-attribute+)
                          :email (.get found-user +email-attribute+)})
                     (not-found "No such user"))))

            (GET "/collections/:email" req
                 :path-params [email :- s/Str]
                 :return [{s/Str s/Str}]
                 :summary "Returns the names and ids of collections that belong to the email address"
                 (let [userid (email->userid email)]
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
                     (not-found "No such user")))))))

;;; (def $userid (email->userid "mikel@evins.net"))
;;; (def $collections (couchio/find-objects (config/delectus-content-bucket) [] {"type" +collection-type+}))
