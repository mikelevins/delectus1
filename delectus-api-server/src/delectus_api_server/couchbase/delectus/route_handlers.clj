(ns delectus-api-server.couchbase.delectus.route-handlers
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]))

;;; ---------------------------------------------------------------------
;;; delectus handlers
;;; ---------------------------------------------------------------------

(defn root [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    (html [:h1 "Delectus 2"]
                  [:p "version 0.1"])})

;; (defn users [req]
;;   {:status  200
;;    :headers {"Content-Type" "application/json"}
;;    :body    (let [found (delectus-users)]
;;               (json/write-str found))})

(defn users [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    (json/write-str "/users not yet implemented")})

