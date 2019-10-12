(ns delectus-api-server.core
  (:require [org.httpkit.server :as server]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer :all]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.data.json :as json])
  (:gen-class))

;;; ---------------------------------------------------------------------
;;; read the credential store
;;; ---------------------------------------------------------------------

(defonce +delectus-credentials+ (atom nil))

(defn delectus-credentials []
  (if (not @+delectus-credentials+)
    (swap! +delectus-credentials+
           (fn [ignored]
             (json/read-str (slurp "/Users/mikel/.delectus/env.json") :key-fn keyword))))
  @+delectus-credentials+)

(defn reset-delectus-credentials []
  (swap! +delectus-credentials+
         (constantly nil)))

;;; ---------------------------------------------------------------------
;;; couchbase connection
;;; ---------------------------------------------------------------------

(defonce +couchbase-cluster-name+ "mars.local")
(defonce +couchbase-cluster+ (atom nil))

(defn couchbase-cluster []
  @+couchbase-cluster+)

(defn init-couchbase-cluster []
  (when (nil? @+couchbase-cluster+)
    (swap! +couchbase-cluster+
           (fn [old-val]
             (com.couchbase.client.java.CouchbaseCluster/create [+couchbase-cluster-name+]))))
  @+couchbase-cluster+)

;;; ---------------------------------------------------------------------
;;; test handlers
;;; ---------------------------------------------------------------------

;;; generic handlers
;;; ----------------

(defn landing-page [req]
  {:status  200
   :headers {"Content-Type" "text/html"}
   :body    "<h1>Delectus 2 API Server, v 0.1</h1>"})

(defn hello-name [req]
  {:status 200
   :headers {"Content-type" "text/html"}
   :body (let [nm (:name (:params req))]
            (if nm
              (str "Hello, " nm "!")
              (str "Hello!")))})

;;; handlers that use the Couchbase API
;;; -----------------------------------

(defn ensure-primary-index [bucket]
  ;; create a N1QL primary index, unless it already exists
  (.createN1qlPrimaryIndex (.bucketManager bucket) true false))

(defn status [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (init-couchbase-cluster)
               credentials (delectus-credentials)]
           (.authenticate couch
                          (:delectus-travel-sample-user credentials)
                          (:delectus-travel-sample-password credentials))
           (let [mgr (.clusterManager couch)
                 info (.raw (.info mgr))]
             (.toString info)))})

;;; returns: ("airline" "airport" "hotel" "landmark" "route")
(defn document-types [req bucket-name]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (init-couchbase-cluster)
               credentials (delectus-credentials)]
           (.authenticate couch
                          (:delectus-travel-sample-user credentials)
                          (:delectus-travel-sample-password credentials))
           (let [bucket (.openBucket couch bucket-name)]
             (ensure-primary-index bucket)
             (let [select-expression (pp/cl-format nil
                                                   "SELECT type FROM `~A` WHERE type IS NOT MISSING"
                                                   bucket-name)
                   result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple select-expression))
                   vals (distinct (map (fn [r] (.value r)) result))
                   objs (map (fn [v](:type (json/read-json (.toString v)))) vals)]
               (json/write-str objs))))})

(defn objects-of-type [req bucket-name type-name]
  (let [couch (init-couchbase-cluster)
        credentials (delectus-credentials)]
    (.authenticate couch
                   (:delectus-travel-sample-user credentials)
                   (:delectus-travel-sample-password credentials))
    (let [bucket (.openBucket couch bucket-name)]
      (ensure-primary-index bucket)
      (let [limit (or (:limit (:params req)) 10)
            offset (or (:offset (:params req)) 0)
            select-expr (pp/cl-format nil
                                      "SELECT * FROM `~A` WHERE type = \"~A\" LIMIT ~A OFFSET ~A"
                                      bucket-name type-name limit offset)
            result (.query bucket (com.couchbase.client.java.query.N1qlQuery/simple select-expr))
            vals (map (fn [r](get (json/read-json (.toString (.value r))) (keyword bucket-name)))
                      result)]
        (json/write-str vals)))))

(defn airlines [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req "travel-sample" "airline")})

(defn airports [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req "travel-sample" "airport")})

(defn hotels [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req "travel-sample" "hotel")})

(defn landmarks [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req "travel-sample" "landmark")})

(defn travel-routes [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req "travel-sample" "route")})

;;; ---------------------------------------------------------------------
;;; routes
;;; ---------------------------------------------------------------------

(defroutes app-routes
  ;; landing page
  ;; ------------
  (GET "/" [] landing-page)
  ;; testing routes
  ;; --------------
  (GET "/hello" [] hello-name)
  (GET "/status" [] status)
  (GET "/document-types" [] (fn [req] (document-types req "travel-sample")))
  (GET "/airlines" [] airlines)
  (GET "/airports" [] airports)
  (GET "/hotels" [] hotels)
  (GET "/landmarks" [] landmarks)
  (GET "/routes" [] travel-routes)
  ;; Delectus API routes
  ;; -------------------
  
  ;; default ("Page not found") route
  ;; --------------------------------
  (route/not-found "Error, page not found!"))

;;; ---------------------------------------------------------------------
;;; main server program
;;; ---------------------------------------------------------------------

(defn -main
  "This is our main entry point"
  [& args]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "9000"))]
    ;; Run the server with Ring.defaults middleware
    (server/run-server (wrap-defaults #'app-routes site-defaults) {:port port})
    ;; Run the server without ring defaults
    ;;(server/run-server #'app-routes {:port port})
    (println (str "Running webserver at http:/127.0.0.1:" port "/"))))
