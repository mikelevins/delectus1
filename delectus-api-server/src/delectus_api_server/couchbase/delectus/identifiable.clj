(ns delectus-api-server.couchbase.delectus.identifiable)

(defprotocol Identifiable
  (get-id [data]))

