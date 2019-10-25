(ns delectus-api-server.couchbase.delectus.itemizing)

(defprotocol Itemizing
  (add-item [data field-values])
  (get-items [data])
  (item-at [data index])
  (max-item-index [data])
  (update-items [data new-items])
  (upsert-item-at [data index new-item]))

