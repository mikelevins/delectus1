(ns delectus-api-server.couchbase.delectus.nameable)

(defprotocol Nameable
  (get-name [data])
  (rename [data new-name]))

