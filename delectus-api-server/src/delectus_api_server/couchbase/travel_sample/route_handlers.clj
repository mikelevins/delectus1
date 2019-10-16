(ns delectus-api-server.couchbase.travel-sample.route-handlers
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [delectus-api-server.configuration :as config]
            [delectus-api-server.couchbase.utilities :as couch-utils])
  (:import
   (com.couchbase.client.java.query N1qlQuery)))

;;; ---------------------------------------------------------------------
;;; travel-sample handlers and support functions
;;; ---------------------------------------------------------------------

;;; returns: ("airline" "airport" "hotel" "landmark" "route")
(defn document-types [req]
  (let [bucket-name (:travel-sample-bucket-name (config/delectus-configuration))]
    {:status 200
     :headers {"Content-type" "application/json"}
     :body (let [couch (config/couchbase-cluster)
                 configuration (config/delectus-configuration)]
             (.authenticate couch
                            (:travel-sample-user configuration)
                            (:travel-sample-password configuration))
             (let [bucket (.openBucket couch bucket-name)]
               (couch-utils/ensure-primary-index bucket)
               (let [select-expression (pp/cl-format nil
                                                     "SELECT type FROM `~A` WHERE type IS NOT MISSING"
                                                     bucket-name)
                     result (.query bucket (N1qlQuery/simple select-expression))
                     vals (distinct (map (fn [r] (.value r)) result))
                     objs (map (fn [v](:type (json/read-json (.toString v)))) vals)]
                 (json/write-str objs))))}))

(defn objects-of-type [req bucket-name type-name]
  (let [couch (config/couchbase-cluster)
        configuration (config/delectus-configuration)]
    (.authenticate couch
                   (:travel-sample-user configuration)
                   (:travel-sample-password configuration))
    (let [bucket (.openBucket couch bucket-name)]
      (couch-utils/ensure-primary-index bucket)
      (let [limit (or (:limit (:params req)) 10)
            offset (or (:offset (:params req)) 0)
            select-expr (pp/cl-format nil
                                      "SELECT * FROM `~A` WHERE type = \"~A\" LIMIT ~A OFFSET ~A"
                                      bucket-name type-name limit offset)
            result (.query bucket (N1qlQuery/simple select-expr))
            vals (map (fn [r](get (json/read-json (.toString (.value r))) (keyword bucket-name)))
                      result)]
        (json/write-str vals)))))

(defn airlines [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (config/delectus-configuration)) "airline")})

(defn airports [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (config/delectus-configuration)) "airport")})

(defn hotels [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (config/delectus-configuration)) "hotel")})

(defn landmarks [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (config/delectus-configuration)) "landmark")})

(defn travel-routes [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (config/delectus-configuration)) "route")})
