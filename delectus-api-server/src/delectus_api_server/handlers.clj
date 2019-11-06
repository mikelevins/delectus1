(ns delectus-api-server.handlers
  (:require
   [buddy.hashers :as hashers]
   [clojure.data.json :as json]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.couchbase.delectus.api :as api]
   [delectus-api-server.utilities :refer [fmt]]
   [hiccup.core :refer :all]
   [org.httpkit.server :as server])
  (:import
   (com.couchbase.client.java.query N1qlQuery)))


;;; ---------------------------------------------------------------------
;;; generic test handlers
;;; ---------------------------------------------------------------------

(defn echo [req]
  (let [req-keys (keys req)
        req-vals (map #(if (instance? org.httpkit.server.AsyncChannel %)
                         (.toString %)
                         %)
                      (vals req))]
    {:status  200
     :headers {"Content-Type" "application/json"}
     :body    (json/write-str (zipmap req-keys req-vals))}))

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
  (let [found-user (api/user email)]
    (if found-user
      (if (hashers/check password (.get found-user "password-hash"))
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
              (json/write-str (api/email->userid email)))})

(defn collections [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))]
              (json/write-str (api/list-collections (api/email->userid email))))})

(defn lists [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))]
              (json/write-str (api/list-lists (api/email->userid email))))})

(defn collection-with-id [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  collection-id (:id (:params req))]
              (json/write-str (api/find-collection-by-id (api/email->userid email)
                                                         collection-id)))})

(defn collection-named [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  collection-name (:name (:params req))]
              (json/write-str (api/find-collection-by-name (api/email->userid email)
                                                           collection-name)))})

(defn list-with-id [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  list-id (:id (:params req))]
              (json/write-str (api/find-list-by-id (api/email->userid email)
                                                   list-id)))})

(defn list-named [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (let [email (:email (:params req))
                  list-name (:name (:params req))]
              (json/write-str (api/find-list-by-name (api/email->userid email)
                                                     list-name)))})

