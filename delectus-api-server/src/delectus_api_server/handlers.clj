(ns delectus-api-server.handlers
  (:require
   [buddy.hashers :as hashers]
   [clj-time.core :as time]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [compojure.core :refer :all]
   [compojure.response :refer [render]]
   [compojure.route :as route]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.couchbase.delectus.api :as api]
   [delectus-api-server.couchbase.delectus.users :as delectus-users]
   [delectus-api-server.couchbase.marshal :as marshal]
   [hiccup.core :refer :all]
   [org.httpkit.server :as server]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.middleware.defaults :refer :all]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.util.response :refer [response redirect content-type]]))


;;; ---------------------------------------------------------------------
;;; generic test handlers
;;; ---------------------------------------------------------------------

(defn echo [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (json/write-str (marshal/make-couchable req))})


(defn status [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [mgr (.clusterManager (config/couchbase-cluster)
                                    (:delectus-admin-user (config/delectus-configuration))
                                    (:delectus-admin-password (config/delectus-configuration)))
               info (.raw (.info mgr))]
           (.toString info))})

;;; ---------------------------------------------------------------------
;;; support functions
;;; ---------------------------------------------------------------------

(defn login-user [email password]
  (let [found-user (delectus-users/user-from-email email)]
    (if found-user
      (if (hashers/check password (:password-hash found-user))
        found-user
        false)
      false)))

;;; (login-user "mikel@evins.net" "")

;;; ---------------------------------------------------------------------
;;; delectus handlers
;;; ---------------------------------------------------------------------

(defn login [req]
  (let [params (:params req)
        supplied-email (:email params)
        supplied-password (:password params)
        found-user (login-user supplied-email supplied-password)]
    (if found-user
      {:status  200
       :headers {"Content-Type" "application/json"}
       :body (json/write-str {:token "testing login"})}
      {:status  401
       :headers {"Content-Type" "text/html"}
       :body    (html [:h1 "Delectus 2"]
                      [:p "Login failed"])})))


(defn userid [request]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params request))]
              (json/write-str (api/email->user-id email)))})

(defn collections [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))]
              (json/write-str (api/list-collections (api/email->user-id email))))})

(defn lists [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))]
              (json/write-str (api/list-lists (api/email->user-id email))))})

(defn collection-with-id [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  collection-id (:id (:params req))]
              (json/write-str (api/find-collection-by-id (api/email->user-id email)
                                                         collection-id)))})

(defn collection-named [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  collection-name (:name (:params req))]
              (json/write-str (api/find-collection-by-name (api/email->user-id email)
                                                           collection-name)))})

(defn list-with-id [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  list-id (:id (:params req))]
              (json/write-str (api/find-list-by-id (api/email->user-id email)
                                                   list-id)))})

(defn list-named [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  list-name (:name (:params req))]
              (json/write-str (api/find-list-by-name (api/email->user-id email)
                                                     list-name)))})

