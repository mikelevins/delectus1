(ns delectus-api-server.couchbase.delectus.route-handlers
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [hiccup.core :refer :all]
            [delectus-api-server.couchbase.delectus.users :refer [delectus-user-emails]]
            [delectus-api-server.couchbase.delectus.api :as api]))

;;; ---------------------------------------------------------------------
;;; delectus handlers
;;; ---------------------------------------------------------------------

(defn root [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (html [:h1 "Delectus 2"]
                  [:p "version 0.1"])})

(defn users [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (json/write-str (delectus-user-emails))})

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

