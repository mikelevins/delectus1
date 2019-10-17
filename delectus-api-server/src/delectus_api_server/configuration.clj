(ns delectus-api-server.configuration
  (:require 
   [aero.core :as aero])
  (:import
   (java.lang System)
   (com.couchbase.client.java CouchbaseCluster)))

(defonce +delectus-session-rng+ (atom (new java.security.SecureRandom)))
(defonce +delectus-configuration+ (atom nil))

;;; ---------------------------------------------------------------------
;;; server configuration
;;; ---------------------------------------------------------------------

(defn delectus-configuration []
  (if (not @+delectus-configuration+)
    (swap! +delectus-configuration+
           (fn [ignored]
             (aero/read-config (str (System/getenv "HOME") "/.config/delectus/config.edn")))))
  @+delectus-configuration+)

(defn reset-delectus-configuration []
  (swap! +delectus-configuration+ (constantly nil)))

;;; (reset-delectus-configuration)
;;; (delectus-configuration)

;;; ---------------------------------------------------------------------
;;; couchbase connection
;;; ---------------------------------------------------------------------

(def +couchbase-cluster-name+ (:delectus-db-server (delectus-configuration)))
(defonce +couchbase-cluster+ (atom nil))

(defn couchbase-cluster []
  (when (nil? @+couchbase-cluster+)
    (swap! +couchbase-cluster+
           (fn [old-val](CouchbaseCluster/create [(:delectus-db-server (delectus-configuration))]))))
  @+couchbase-cluster+)

(defn reset-couchbase-cluster []
  (when @+couchbase-cluster+
    (.disconnect @+couchbase-cluster+))
  (swap! +couchbase-cluster+ (constantly nil)))

;;; (couchbase-cluster)
;;; (reset-couchbase-cluster)

(defonce +delectus-bucket+ (atom nil))

(defn delectus-bucket []
  (when (nil? @+delectus-bucket+)
    (swap! +delectus-bucket+
           (fn [old-val]
             (let [bucketname (:delectus-main-bucket-name (delectus-configuration))
                   bucketpass (:delectus-user-password (delectus-configuration))]
               (.openBucket (couchbase-cluster) bucketname bucketpass)))))
  @+delectus-bucket+)


(defn reset-delectus-bucket []
  (when @+delectus-bucket+
    (.close @+delectus-bucket+))
  (swap! +delectus-bucket+ (constantly nil)))

;;; (delectus-bucket)
;;; (reset-delectus-bucket)


(defonce +travel-sample-bucket+ (atom nil))

(defn travel-sample-bucket []
  (when (nil? @+travel-sample-bucket+)
    (swap! +travel-sample-bucket+
           (fn [old-val]
             (let [bucketname (:travel-sample-bucket-name (delectus-configuration))
                   bucketpass (:travel-sample-password (delectus-configuration))]
               (.openBucket (couchbase-cluster) bucketname bucketpass)))))
  @+travel-sample-bucket+)

(defn reset-travel-sample-bucket []
  (when @+travel-sample-bucket+
    (.close @+travel-sample-bucket+))
  (swap! +travel-sample-bucket+ (constantly nil)))

;;; (travel-sample-bucket)
;;; (reset-travel-sample-bucket)

