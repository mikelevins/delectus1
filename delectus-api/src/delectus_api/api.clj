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
