(ns delectus-api-server.identifiers
  (:import
   (java.nio ByteBuffer)
   (java.util Base64 UUID)))

;;; makeid [] => String
;;; ---------------------------------------------------------------------
;;; returns a unique ID string. First generates a random UUID, then
;;; converts it to Java's URL-safe base64 encoding, then, finally,
;;; strips off the final two padding characters (which are
;;; always "==") and returns the result

(defn makeid []
  (let [buf (ByteBuffer/wrap (byte-array 16))
        uuid (UUID/randomUUID)
        high (.getMostSignificantBits uuid)
        low (.getLeastSignificantBits uuid)]
    (.putLong buf high)
    (.putLong buf low)
    (let [s (.encodeToString (Base64/getUrlEncoder)
                             (.array buf))]
      (subs s 0 (- (count s) 2)))))

;;; (makeid)

