(ns delectus-api-server.core
  (:require [org.httpkit.server :as server]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer :all]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [aero.core :as aero])
  (:gen-class))


;;; ---------------------------------------------------------------------
;;; general utility functions
;;; ---------------------------------------------------------------------

(defonce +delectus-session-rng+ (atom (new java.security.SecureRandom)))
;;; @+delectus-session-rng+

(defn uuid
  ([] (java.util.UUID/randomUUID))
  ([idstr] (java.util.UUID/fromString idstr)))

(defn ->base64 [int-vector]
  (.encodeToString (java.util.Base64/getUrlEncoder)
                   (byte-array int-vector)))

(defn make-api-key []
  (let [bytes (byte-array 32)]
    (.nextBytes @+delectus-session-rng+ bytes)
    (->base64 bytes)))

;;; (make-api-key)

;;; ---------------------------------------------------------------------
;;; server configuration
;;; ---------------------------------------------------------------------

(defonce +delectus-configuration+ (atom nil))

(defn delectus-configuration []
  (if (not @+delectus-configuration+)
    (swap! +delectus-configuration+
           (fn [ignored]
             (aero/read-config (str (java.lang.System/getenv "HOME") "/.config/delectus/config.edn")))))
  @+delectus-configuration+)

(defn reset-delectus-configuration []
  (swap! +delectus-configuration+
         (constantly nil)))

;;; ---------------------------------------------------------------------
;;; couchbase connection
;;; ---------------------------------------------------------------------

(defonce +couchbase-cluster-name+ (:delectus-db-server (delectus-configuration)))
(defonce +couchbase-cluster+ (atom nil))

(defn couchbase-cluster []
  (when (nil? @+couchbase-cluster+)
    (swap! +couchbase-cluster+
           (fn [old-val]
             (com.couchbase.client.java.CouchbaseCluster/create [(:delectus-db-server (delectus-configuration))]))))
  @+couchbase-cluster+)

;;; ---------------------------------------------------------------------
;;; Couchbase support functions
;;; ---------------------------------------------------------------------

(defn ensure-primary-index [bucket]
  ;; create a N1QL primary index, unless it already exists
  (.createN1qlPrimaryIndex (.bucketManager bucket) true false))

;;; ---------------------------------------------------------------------
;;; generic test handlers
;;; ---------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------
;;; common Couchbase functions
;;; ---------------------------------------------------------------------

(defn status [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (couchbase-cluster)
               configuration (delectus-configuration)]
           (.authenticate couch
                          (:travel-sample-user configuration)
                          (:travel-sample-password configuration))
           (let [mgr (.clusterManager couch)
                 info (.raw (.info mgr))]
             (.toString info)))})

;;; ---------------------------------------------------------------------
;;; travel-sample handlers and support functions
;;; ---------------------------------------------------------------------

;;; returns: ("airline" "airport" "hotel" "landmark" "route")
(defn document-types [req bucket-name]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (let [couch (couchbase-cluster)
               configuration (delectus-configuration)]
           (.authenticate couch
                          (:travel-sample-user configuration)
                          (:travel-sample-password configuration))
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
  (let [couch (couchbase-cluster)
        configuration (delectus-configuration)]
    (.authenticate couch
                   (:travel-sample-user configuration)
                   (:travel-sample-password configuration))
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
   :body (objects-of-type req (:travel-sample-bucket-name (delectus-configuration)) "airline")})

(defn airports [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (delectus-configuration)) "airport")})

(defn hotels [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (delectus-configuration)) "hotel")})

(defn landmarks [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (delectus-configuration)) "landmark")})

(defn travel-routes [req]
  {:status 200
   :headers {"Content-type" "application/json"}
   :body (objects-of-type req (:travel-sample-bucket-name (delectus-configuration)) "route")})

;;; ---------------------------------------------------------------------
;;; collection-test handlers and support functions
;;; ---------------------------------------------------------------------

;;; (def $couch (couchbase-cluster))
;;; (def $conf (delectus-configuration))
;;; (.authenticate $couch (:delectus-admin-user $conf)(:delectus-admin-password $conf))
;;; (def $bucket (.openBucket $couch "collection-test"))

;;; List
;;; (def $listid "test_list_1")
;;; (def $flavor-list (new com.couchbase.client.java.datastructures.collections.CouchbaseArrayList $listid $bucket))
;;; (.size $flavor-list)
;;; (.add $flavor-list "Apple")
;;; (.add $flavor-list "Banana")

;;; Map
;;; (def $mapid "test_map_1")
;;; (def $mapval { "Apple" "red", "Banana" "yellow", "Cherry" "red"})
;;; (def $flavor-map (new com.couchbase.client.java.datastructures.collections.CouchbaseMap $mapid $bucket $mapval))
;;; (def $flavor-map (new com.couchbase.client.java.datastructures.collections.CouchbaseMap $mapid $bucket))
;;; (.size $flavor-map)

;;; ---------------------------------------------------------------------
;;; delectus handlers and support functions
;;; ---------------------------------------------------------------------

;;; (def $couch (couchbase-cluster))
;;; (def $conf (delectus-configuration))
;;; (.authenticate $couch (:delectus-admin-user $conf)(:delectus-admin-password $conf))
;;; (def $bucket (.openBucket $couch (:delectus-main-bucket-name (delectus-configuration))))


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
  (GET "/document-types" [] (fn [req] (document-types req (:travel-sample-bucket-name (delectus-configuration)))))
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
