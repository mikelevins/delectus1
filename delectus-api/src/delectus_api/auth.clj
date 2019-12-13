(ns delectus-api.auth
  (:require
   [buddy.core.nonce :as nonce]
   [buddy.hashers :as hashers]
   [buddy.sign.jwt :as jwt]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [tick.alpha.api :as t]
   ))


;;; ---------------------------------------------------------------------
;;; auth
;;; ---------------------------------------------------------------------

;;; tokens
;;; ---------------------------------------------------------------------
;;; the token works right if $auth-map-2 equals $auth-map-1
;;;
;;; (def $auth-map-1 (make-auth-token (email->user "mikel@evins.net")))
;;; (def $jwt-key (nonce/random-bytes 32))
;;; (def $token (jwt/encrypt $auth-map-1 $jwt-key))
;;; (def $auth-map-2 (jwt/decrypt $token $jwt-key))
;;; (= $auth-map-1 $auth-map-2)

(defn make-auth-map [user-record remote-addr]
  (let [userid (.get user-record "id")]
    {;; identifies the logged-in account
     :userid userid
     ;; identifies the account the client thinks it's authenticating
     :email (.get user-record +email-attribute+)
     ;; used to prevent a different device from hijacking a token
     :remote-addr remote-addr
     ;; designates when the authentication became valid
     :timestamp (str (t/now))
     ;; specifies how long it lasts; in seconds; default 1 hour
     :expiration 3600}))

(defonce +jwt-secret+ (nonce/random-bytes 32))

(defn auth-map->token [auth-map]
  (jwt/encrypt auth-map +jwt-secret+))

(defn make-auth-token [user-record remote-addr]
  (auth-map->token
   (make-auth-map user-record remote-addr)))

(defn decode-auth-token [token]
  (jwt/decrypt token +jwt-secret+))

;;; (def $token (make-auth-token (couchio/email->user "mikel@evins.net") "127.0.0.1"))
;;; (def $token-map (decode-auth-token $token))
;;; (decode-auth-token "foo")

(defn authenticate-user [email password]
  (let [found-user (couchio/email->user email)]
    (if found-user
      (if (hashers/check password (.get found-user +password-hash-attribute+))
        found-user
        nil)
      nil)))

;;; (authenticate-user "mikel@evins.net" "foo")
;;; (authenticate-user "nobody@evins.net" "foo")
