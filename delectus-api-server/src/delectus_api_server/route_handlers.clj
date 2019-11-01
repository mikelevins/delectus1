(ns delectus-api-server.route-handlers
  (:require
   [buddy.auth :refer [authenticated? throw-unauthorized]]
   [buddy.auth.backends :as backends]
   [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]
   [buddy.sign.jwt :as jwt]
   [clj-time.core :as time]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [compojure.core :refer :all]
   [compojure.response :refer [render]]
   [compojure.route :as route]
   [delectus-api-server.configuration :as config]
   [delectus-api-server.couchbase.delectus.route-handlers :as delectus-handlers]
   [delectus-api-server.couchbase.delectus.users :as delectus-users]
   [delectus-api-server.couchbase.route-handlers :as couch-handlers]
   [delectus-api-server.couchbase.travel-sample.route-handlers :as travel-handlers]
   [delectus-api-server.couchbase.marshal :as marshal]
   [delectus-api-server.route-handlers :as handlers]
   [org.httpkit.server :as server]
   [ring.middleware.cors :refer [wrap-cors]]
   [ring.middleware.defaults :refer :all]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.util.response :refer [response redirect content-type]]))


;;; ---------------------------------------------------------------------
;;; generic test handlers
;;; ---------------------------------------------------------------------

(defn landing-page [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "<h1>Delectus 2 API Server</h1>"})

(defn echo [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (json/write-str (marshal/make-couchable req))})
