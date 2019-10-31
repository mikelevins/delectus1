(ns delectus-api-server.route-handlers
  (:require [clojure.data.json :as json]
            [delectus-api-server.couchbase.marshal :as marshal]))

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

(defn hello-name [req]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body (let [nm (:name (:params req))]
            (if nm
              (str "Hello, " nm "!")
              (str "Hello!")))})
