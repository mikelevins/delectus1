(ns delectus-api.auth
  (:require
   [buddy.core.nonce :as nonce]
   [buddy.hashers :as hashers]
   [buddy.sign.jwt :as jwt]
   [clj-time.core :as t]
   [clj-time.local :as l]
   [delectus-api.constants :refer :all]
   [delectus-api.couchio :as couchio]
   [delectus-api.model :as model]
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

(defn make-auth-map [user-record]
  (let [userid (.get user-record +id-key+)]
    {;; identifies the logged-in account
     :userid userid
     ;; identifies the account the client thinks it's authenticating
     :email (.get user-record +email-key+)
     ;; designates when the authentication became valid
     :timestamp (str (l/local-now))
     ;; specifies how long it lasts; in seconds; default 1 hour
     :expiration 3600}))

(defonce +jwt-secret+ (nonce/random-bytes 32))

(defn auth-map->token [auth-map]
  (jwt/encrypt auth-map +jwt-secret+))

(defn make-auth-token [user-record]
  (auth-map->token
   (make-auth-map user-record)))

(defn decode-auth-token [token]
  (jwt/decrypt token +jwt-secret+))

;;; (def $token (make-auth-token (couchio/email->user "mikel@evins.net") "127.0.0.1"))
;;; (def $token-map (decode-auth-token $token))
;;; (decode-auth-token "foo")

(defn authenticate-user [userid password]
  (let [found-user (model/get-user userid)]
    (if found-user
      (if (hashers/check password (.get found-user +password-hash-key+))
        found-user
        false))))

(defn login-user [email password]
  (let [userid (model/email->userid email)]
    (if userid
      (authenticate-user userid password)
      false)))

;;; (login-user "mikel@evins.net" "foo")
;;; (login-user "nobody@evins.net" "foo")
