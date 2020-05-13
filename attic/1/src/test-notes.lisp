;;; notes accumulates while testing implementation strategies

;;; ---------------------------------------------------------------------
;;; timing temporary tables in SQLite
;;; ---------------------------------------------------------------------

;;; does it matter if I create the temporary on disk or in memory?
;;; below are some notes from some testing. The answer? It doesn't matter.
;;; SQLite's page cache keeps things equally fast from disk for any file size
;;; that we care about for Delectus.

;;; to tell SQLite to put the temp store in memory:
;;; PRAGMA temp_store = MEMORY

;;; time to create the temp table:
;;; on disk: 360ms
;;; in memory: 360ms

;;; time to create an ascending index on the timestamp field
;;; on disk: 158ms
;;; in memory: 157ms
;;; CREATE INDEX idx_timestamp_asc on latest_items (timestamp)

;;; on disk: 530ms
;;; in memory: 520ms
;;; select * from latest_items order by timestamp

;;; on disk: 50ms
;;; in memory: 52ms
;;; select * from latest_items order by timestamp asc limit 25

;;; on disk: 45ms
;;; in memory: 60ms
;;; select * from latest_items order by timestamp asc limit 25 offset 80000

;;; (sqlgen-get-latest-items :limit 25 :offset 1500)

;;; ---------------------------------------------------------------------
;;; testing creation and use of temporary tables
;;; ---------------------------------------------------------------------

;;; code to check that creating the temp table works
;;; (time (with-open-database (db $words-test-path)
;;;   (format t "~%check creation of latest-items-table...")
;;;   (format t "~%latest-items-table is: ~S" (db-check-latest-items-table-exists db))
;;;   (format t "~%creating latest-items-table...")
;;;   (db-create-latest-items-table db)
;;;   (format t "~%latest-items-table is: ~S" (db-check-latest-items-table-exists db))
;;;   (format t "~%Done.~%")))
