(ns delectus-api-server.utilities
  (:require [delectus-api-server.configuration :as config])
  (:import
   (java.util Base64 UUID)))

;;; ---------------------------------------------------------------------
;;; general utility functions
;;; ---------------------------------------------------------------------

(defn uuid
  ([] (UUID/randomUUID))
  ([idstr] (UUID/fromString idstr)))

(defn ->base64 [int-vector]
  (.encodeToString (Base64/getUrlEncoder)
                   (byte-array int-vector)))

(defn make-api-key []
  (let [bytes (byte-array 32)]
    (.nextBytes @config/+delectus-session-rng+ bytes)
    (->base64 bytes)))

;;; (make-api-key)
