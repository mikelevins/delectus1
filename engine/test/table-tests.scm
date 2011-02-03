(define $tbl1 (table:make))
(table:add-column! $tbl1 "Name")
(table:add-column! $tbl1 "Age")

(table:add-row! $tbl1)
(table:add-row! $tbl1)
(table:add-row! $tbl1)

$tbl1

(table:value-at $tbl1 (table:column-index $tbl1 "Name") 0)
(table:put-value-at! $tbl1 (table:column-index $tbl1 "Name") 0 "Fred")
(table:value-at $tbl1 (table:column-index $tbl1 "Name") 0)

(table:value-at $tbl1 (table:column-index $tbl1 "Age") 0)
(table:put-value-at! $tbl1 (table:column-index $tbl1 "Age") 0 35)
(table:value-at $tbl1 (table:column-index $tbl1 "Age") 0)

$tbl1


