(ns delectus-api-server.couchbase.delectus.typable)

(defprotocol Typable
  (get-type [data]))

