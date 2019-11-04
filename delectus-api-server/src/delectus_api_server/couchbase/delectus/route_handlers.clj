(ns delectus-api-server.couchbase.delectus.route-handlers
  (:require [buddy.auth :refer [authenticated? throw-unauthorized]]
            [buddy.auth.backends :as backends]
            [buddy.auth.middleware :refer [wrap-authentication]]
            [buddy.hashers :as hashers]
            [buddy.sign.jwt :as jwt]
            [clj-time.core :as time]
            [clojure.data.json :as json]
            [hiccup.core :refer :all]
            [hiccup.form :refer :all]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.delectus.users :as users]
            [delectus-api-server.couchbase.delectus.api :as api]))

;;; ---------------------------------------------------------------------
;;; support functions
;;; ---------------------------------------------------------------------

(defn login-user [email password]
  (let [found-user (users/user-from-email email)]
    (if found-user
      (if (hashers/check password (:password-hash found-user))
        found-user
        false)
      false)))

;;; ---------------------------------------------------------------------
;;; delectus handlers
;;; ---------------------------------------------------------------------

(defn root [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (html [:h1 "Delectus 2"]
                  [:p "version 0.1"])})

(defn login [req]
  (let [params (:params req)
        supplied-email (:email params)
        supplied-password (:password params)
        found-user (login-user supplied-email supplied-password)]
    (if found-user
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body (let [secret (config/delectus-users-signing-secret)
                   claims {:user (:email found-user)
                           :exp (time/plus (time/now) (time/seconds 3600))}
                   token (jwt/sign claims secret)]
               (json/write-str {:token token}))}
      {:status  401
       :headers {"Content-Type" "text/html"}
       :body    (html [:h1 "Delectus 2"]
                      [:p "Login failed"])})))


(defn userid [request]
  (if-not (authenticated? request)
    (throw-unauthorized)
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (let [email (:email (:params request))]
                (json/write-str (api/email->user-id email)))}))

(defn lists [req]
  (if-not (authenticated? req)
    (throw-unauthorized)
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (let [userid (:userid (:params req))]
                (json/write-str (api/list-lists userid)))}))

(defn collections [req]
  (if-not (authenticated? req)
    (throw-unauthorized)
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (let [userid (:userid (:params req))]
                (json/write-str (api/list-collections userid)))}))

