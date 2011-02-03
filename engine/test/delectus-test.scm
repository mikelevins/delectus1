(api:version)
(define $d1 (api:make-document))
(api:add-column! $d1 "Name")
(api:add-row! $d1)
(doc:filter-text (reg:find-document $d1))
(api:value-at $d1 "Name" 0)
(api:put-value-at! $d1 "Name" 0 "Fred")
(doc:put-value-at! (reg:find-document $d1) "Name" 0 "Fred")
(api:value-at $d1 "Name" 0)


(define $oid1 (api:read-delectus/csv "/Users/mikel/Projects/delectus/test-data/zipcode.csv"))
$oid1
(api:value-at $oid1 "city" 0)

(api:set-sort-column! $oid1 "city")
(api:sort-column $oid1)


(api:set-sort-order! $oid1 $SORT_ASCENDING)
$SORT_ASCENDING
(api:sort-order $oid1)

(api:set-sort-type! $oid1 $SORT_ALPHABETICAL)
$SORT_ALPHABETICAL
(api:sort-type $oid1)

(api:value-at $oid1 "city" 0)
