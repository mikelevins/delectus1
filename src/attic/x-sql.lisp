(in-package :delectus)
(in-readtable :sql)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; experimenting with an s-expression-based syntax for SQL statements
;;; this code is not yet used for anything except to see if I can
;;; find plausible ways to expression SQLite statements in s-expressions

;;; ---------------------------------------------------------------------
;;; sql-increment-next-revision
;;; ---------------------------------------------------------------------

[:update `delectus :set `next_revision `next_revision :+ 1]

;;; ---------------------------------------------------------------------
;;; sql-next-revision
;;; ---------------------------------------------------------------------

[:select `next_revision :from `delectus :limit 1]

;;; ---------------------------------------------------------------------
;;; sql-create-delectus-table
;;; ---------------------------------------------------------------------
;;; NOTE: always render strings with single quotes in the output

[:create-table `delectus
               [[`id "TEXT"]
                [`origin "TEXT"]
                [`format "TEXT"]
                [`next_revision "INTEGER"]]]


;;; ---------------------------------------------------------------------
;;; sql-populate-delectus-table
;;; ---------------------------------------------------------------------

[:insert-into `delectus
              [`id `origin `format `next_revision]
              :values [(MAKEID) *ORIGIN* +DELECTUS-FORMAT-VERSION+ 3]]


;;; ---------------------------------------------------------------------
;;; sql-create-list_data-table
;;; ---------------------------------------------------------------------

[:create-table `list_data
               [[`optype "TEXT"]
                [`opid "TEXT"]
                [`origin "TEXT"]
                [`revision "INTEGER"]
                [`timestamp "TEXT"]
                [`item "TEXT"]
                [`name "TEXT"]
                [`deleted "TEXT"]
                [`peer "TEXT"]]]

;;; ---------------------------------------------------------------------
;;; sql-add-userdata-column
;;; ---------------------------------------------------------------------

[:alter-table `list_data :add [`foo "TEXT"]]

;;; ---------------------------------------------------------------------
;;; sql-assert-op
;;; ---------------------------------------------------------------------
;;; NOTE: the column id is a Delectus identity. We need to read it
;;; case-preserving. In general we probably should read all symbols
;;; as case-preserving

[:insert-into `list_data
              [`optype `opid `origin `revision `timestamp `item `name `deleted `peer [:columnid (MAKEID)]]
              :values ["listname" (MAKEID) *ORIGIN* 3 (NOW-TIMESTAMP) NIL NIL NIL "some column attributes"]]

;;; ---------------------------------------------------------------------
;;; sql-get-column-attributes
;;; ---------------------------------------------------------------------

[:pragma `table_info `list_data]

;;; ---------------------------------------------------------------------
;;; sql-get-latest-listname-op
;;; ---------------------------------------------------------------------

[:select :* :from `list_data
         :where [`optype := "listname"]
         :order-by [`revision "DESC" `origin "DESC"]
         :limit 1]

;;; ---------------------------------------------------------------------
;;; sql-get-latest-columns-op
;;; ---------------------------------------------------------------------

[:select :* :from `list_data
         :where [`optype := "columns"]
         :order-by [`revision "DESC" `origin "DESC"]
         :limit 1]

;;; ---------------------------------------------------------------------
;;; sql-get-latest-item-ops
;;; ---------------------------------------------------------------------

[:select `a.*
         :from [:select [:function "ROW_NUMBER()"]
                        :over [:partition-by `item :order-by [`revision "DESC" `origin "DESC"]]]
         :as `a
         :where [`a.rank := 1]
         :order-by [`a.revision]]


;;; ---------------------------------------------------------------------
;;; sql-get-latest-sync-op
;;; ---------------------------------------------------------------------

[:select :* :from `list_data
         :where [`optype := "sync"]
         :order-by [`revision "DESC" `origin "DESC"]
         :limit 1]

