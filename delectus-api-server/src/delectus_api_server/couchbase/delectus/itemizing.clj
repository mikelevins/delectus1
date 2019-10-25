(ns delectus-api-server.couchbase.delectus.itemizing)

(defprotocol Itemizing
  (get-items [data])
  (item-at [data index])
  (update-items [data new-items])
  (upsert-item-at [data index new-item]))

