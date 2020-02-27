(ns delectus.identifiers
  (:import
   (java.nio ByteBuffer)
   (java.util Base64 UUID)))

;;; makeid [] => String
;;; ---------------------------------------------------------------------

(defn makeid []
  (let [uuid (UUID/randomUUID)]
    (.toString uuid)))

;;; (makeid)

