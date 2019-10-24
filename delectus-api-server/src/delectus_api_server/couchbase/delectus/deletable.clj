(ns delectus-api-server.couchbase.delectus.deletable)

(defprotocol Deletable
  (mark-deleted [data deleted?]))

