(ns delectus-api-server.couchbase.delectus.ownable)

(defprotocol Ownable
  (get-owner-id [data])
  (update-owner-id [data new-owner-id]))

