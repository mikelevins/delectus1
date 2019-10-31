(ns delectus-api-server.couchbase.delectus.route-handlers
  (:require [buddy.auth.backends :as backends]
            [buddy.auth.middleware :refer [wrap-authentication]]
            [buddy.hashers :as hashers]
            [buddy.sign.jwt :as jwt]
            [clojure.data.json :as json]
            [hiccup.core :refer :all]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.delectus.users :as users]
            [delectus-api-server.couchbase.delectus.api :as api]))

;;; ---------------------------------------------------------------------
;;; delectus handlers
;;; ---------------------------------------------------------------------

(defn root [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (html [:h1 "Delectus 2"]
                  [:p "version 0.1"])})


(defn login-user [email password]
  (let [found-user (users/user-from-email email)]
    (if found-user
      (if (hashers/check password (:password-hash found-user))
        found-user
        false)
      false)))

(defn login [req]
  (let [params (:params req)
        supplied-email (:email params)
        supplied-password (:password params)
        found-user (login-user supplied-email supplied-password)]
    (if found-user
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body (let [secret (config/delectus-users-signing-secret)
                   token (jwt/sign {:user (:email found-user)} secret)]
               (json/write-str token))}
      {:status  401
       :headers {"Content-Type" "text/html"}
       :body    (html [:h1 "Delectus 2"]
                      [:p "Login failed"])})))


(defn userid [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))]
              (json/write-str (api/email->user-id email)))})

(defn lists [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [userid (:userid (:params req))]
              (json/write-str (api/list-lists userid)))})

