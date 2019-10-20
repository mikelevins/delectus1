(ns delectus-api-server.couchbase.travel-sample.route-handlers
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.utilities :as couch-utils])
  (:import
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; support functions
;;; ---------------------------------------------------------------------

;;; returns: ("airline" "airport" "hotel" "landmark" "route")
(defn document-types [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [bucket (config/travel-sample-bucket)]
           (couch-utils/ensure-primary-index bucket)
           (let [select-expression (pp/cl-format nil
                                                 "SELECT type FROM `~A` WHERE type IS NOT MISSING"
                                                 (.name bucket))
                 result (.query bucket (N1qlQuery/simple select-expression))
                 vals (distinct (map #(.get (.value %) "type") result))]
             (json/write-str vals)))})

(defn objects-of-type [req bucket type-name]
  (couch-utils/ensure-primary-index bucket)
  (let [limit (or (:limit (:params req)) 10)
        offset (or (:offset (:params req)) 0)
        select-expr (pp/cl-format nil
                                  "SELECT * FROM `~A` WHERE type = \"~A\" LIMIT ~A OFFSET ~A"
                                  (.name bucket) type-name limit offset)
        result (.query bucket (N1qlQuery/simple select-expr))
        vals (map (fn [r](get (json/read-json (.toString (.value r))) (keyword (.name bucket))))
                  result)]
    (json/write-str vals)))


;;; ---------------------------------------------------------------------
;;; travel-sample handlers
;;; ---------------------------------------------------------------------

(defn airlines [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (config/travel-sample-bucket) "airline")})

(defn airports [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (config/travel-sample-bucket) "airport")})

(defn hotels [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (config/travel-sample-bucket) "hotel")})

(defn landmarks [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (config/travel-sample-bucket) "landmark")})

(defn travel-routes [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (config/travel-sample-bucket) "route")})
