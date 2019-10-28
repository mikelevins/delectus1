(ns delectus-api-server.couchbase.delectus.itemizing)

(defprotocol Itemizing
  (add-item [data item])
  (get-items [data])
  (item-at [data index])
  (max-item-index [data])
  (remove-item-at [data index])
  (update-items [data new-items])
  (upsert-item-at [data index new-item]))

