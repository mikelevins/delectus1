(ns delectus-api-server.couchbase.delectus.authenticatable)

(defprotocol Authenticatable
  (get-login-name [data])
  (get-password-hash [data])
  (update-password-hash [data new-password-hash]))

