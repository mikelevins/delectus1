(ns delectus-api-server.configuration
  (:require 
   [aero.core :as aero]
   [delectus-api-server.constants :as constants])
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

;;; ---------------------------------------------------------------------
;;; couchbase buckets
;;; ---------------------------------------------------------------------

;;; delectus
;;; --------

(defonce +delectus-users-bucket+ (atom nil))

(defn delectus-users-bucket []
  (when (nil? @+delectus-users-bucket+)
    (swap! +delectus-users-bucket+
           (fn [old-val]
             (let [bucketname constants/+delectus-users-bucket-name+
                   bucketpass (:delectus-user-password (delectus-configuration))]
               (.openBucket (couchbase-cluster) bucketname bucketpass)))))
  @+delectus-users-bucket+)

(defn reset-delectus-users-bucket []
  (when @+delectus-users-bucket+
    (.close @+delectus-users-bucket+))
  (swap! +delectus-users-bucket+ (constantly nil)))

;;; (delectus-users-bucket)
;;; (reset-delectus-users-bucket)

(defonce +delectus-content-bucket+ (atom nil))

(defn delectus-content-bucket []
  (when (nil? @+delectus-content-bucket+)
    (swap! +delectus-content-bucket+
           (fn [old-val]
             (let [bucketname constants/+delectus-content-bucket-name+
                   bucketpass (:delectus-user-password (delectus-configuration))]
               (.openBucket (couchbase-cluster) bucketname bucketpass)))))
  @+delectus-content-bucket+)

(defn reset-delectus-content-bucket []
  (when @+delectus-content-bucket+
    (.close @+delectus-content-bucket+))
  (swap! +delectus-content-bucket+ (constantly nil)))

;;; (delectus-content-bucket)
;;; (reset-delectus-content-bucket)


;;; travel-sample
;;; -------------

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

