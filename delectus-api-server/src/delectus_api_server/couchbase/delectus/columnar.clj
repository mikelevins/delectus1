(ns delectus-api-server.couchbase.delectus.columnar)

(defprotocol Columnar
  (add-column [data column-name])
  (column-at [data index])
  (find-column-name [data column-name])
  (get-columns [data])
  (max-column-index [data])
  (update-columns [data new-columns])
  (upsert-column-at [data index new-column]))

