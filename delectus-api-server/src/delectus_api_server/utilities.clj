(ns delectus-api-server.utilities
  (:require [clojure.pprint :as pp]
            [delectus-api-server.configuration :as config])
  (:import
   (java.util Base64 UUID)))

;;; ---------------------------------------------------------------------
;;; general utility functions
;;; ---------------------------------------------------------------------

(defn uuid
  ([] (UUID/randomUUID))
  ([idstr] (UUID/fromString idstr)))

;;; (uuid)

(defn ->base64 [int-vector]
  (.encodeToString (Base64/getUrlEncoder)
                   (byte-array int-vector)))

(defn make-api-key []
  (let [bytes (byte-array 32)]
    (.nextBytes @config/+delectus-session-rng+ bytes)
    (->base64 bytes)))

;;; (make-api-key)

;;; ---------------------------------------------------------------------
;;; string utilities
;;; ---------------------------------------------------------------------

(defn fmt [control-str & args]
  (apply pp/cl-format nil control-str args))

(defn valid-email? [thing]
  (if (string? thing)
    (let [mail-regex "^[\\w-_\\.+]*[\\w-_\\.]\\@([\\w]+\\.)+[\\w]+[\\w]$"]
      (.matches thing mail-regex))
    false))

;;; (valid-email? "mikel@evins.net")
;;; (valid-email? "evins.mikel@gmail.com")
;;; (valid-email? "foo@bar")
