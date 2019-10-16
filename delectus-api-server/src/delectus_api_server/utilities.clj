(ns delectus-api-server.utilities
  (:require [delectus-api-server.configuration :as config]))

;;; ---------------------------------------------------------------------
;;; general utility functions
;;; ---------------------------------------------------------------------

(defn uuid
  ([] (java.util.UUID/randomUUID))
  ([idstr] (java.util.UUID/fromString idstr)))

(defn ->base64 [int-vector]
  (.encodeToString (java.util.Base64/getUrlEncoder)
                   (byte-array int-vector)))

(defn make-api-key []
  (let [bytes (byte-array 32)]
    (.nextBytes @config/+delectus-session-rng+ bytes)
    (->base64 bytes)))

;;; (make-api-key)
