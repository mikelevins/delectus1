(ns delectus-api-server.couchbase.delectus.enableable)

(defprotocol Enableable
  (enabled? [data])
  (enable [data])
  (disable [data]))

