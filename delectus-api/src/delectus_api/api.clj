(ns delectus-api.api
  (:require
   [compojure.api.sweet :refer :all]
   [delectus-api.auth :as auth]
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

(defn login [req email password]
  (let [remote-addr (:remote-addr req)
        maybe-auth (auth/authenticate-user email password)]
    (if maybe-auth
      (ok {:token (auth/make-auth-token maybe-auth remote-addr)})
      (unauthorized "Login failed"))))

(defn userid [req email]
  (let [found-user (couchio/email->user email)]
    (if found-user
      (ok (.get found-user +id-attribute+))
      (not-found "No such user"))))

(defn userdata [req userid]
  (let [found-user (couchio/id->user userid)]
    (if found-user
      (ok {:id userid
           :name (.get found-user +name-attribute+)
           :email (.get found-user +email-attribute+)})
      (not-found "No such user"))))

;;; (couchio/id->user "6235e7b7-eb83-47d9-a8ef-ac129601e810")
