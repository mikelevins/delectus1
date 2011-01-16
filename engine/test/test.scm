;;; testing
;;; ----------------------------------------------------------------------

(load-delectus)

(define $d1 (api:make-document))
$d1
(reg:find-document $d1)
(doc:add-column! (reg:find-document $d1) "Name")
(doc:add-row! (reg:find-document $d1))
(doc:value-at (reg:find-document $d1) "Name" 0)
(doc:put-value-at! (reg:find-document $d1) "Name" 0 "Fred")
(doc:value-at (reg:find-document $d1) "Name" 0)
(doc:column-deleted? (reg:find-document $d1) "Name")
(doc:row-deleted? (reg:find-document $d1) 0)
(doc:mark-column-deleted! (reg:find-document $d1) "Name" #t)
(doc:column-deleted? (reg:find-document $d1) "Name")
(doc:mark-column-deleted! (reg:find-document $d1) "Name" #f)
(doc:column-deleted? (reg:find-document $d1) "Name")
(doc:mark-row-deleted! (reg:find-document $d1) 0 #t)
(doc:row-deleted? (reg:find-document $d1) 0)
(doc:mark-row-deleted! (reg:find-document $d1) 0 #f)
(doc:row-deleted? (reg:find-document $d1) 0)
(doc:show-deleted? (reg:find-document $d1))
(doc:set-show-deleted! (reg:find-document $d1) #t)
(doc:show-deleted? (reg:find-document $d1))
(doc:sort-column (reg:find-document $d1))
(doc:set-sort-column! (reg:find-document $d1) "Name")
(doc:sort-column (reg:find-document $d1))
(doc:sort-order (reg:find-document $d1))
(doc:set-sort-order! (reg:find-document $d1) $SORT_ASCENDING)
(doc:sort-order (reg:find-document $d1))
(doc:sort-type (reg:find-document $d1))
(doc:set-sort-type! (reg:find-document $d1) $SORT_ALPHABETICAL)
(doc:sort-type (reg:find-document $d1))
(doc:filter-text (reg:find-document $d1))
(doc:set-filter-text! (reg:find-document $d1) "foo")
(doc:filter-text (reg:find-document $d1))


(define $d2 (api:make-document))
$d2
(reg:find-document $d2)
(doc:add-column! (reg:find-document $d2) "Name")
(doc:add-column! (reg:find-document $d2) "Age")
(doc:add-column! (reg:find-document $d2) "Size")
(doc:add-row! (reg:find-document $d2))
(doc:add-row! (reg:find-document $d2))
(doc:mark-column-deleted! (reg:find-document $d2) "Size" #t)
(doc:mark-row-deleted! (reg:find-document $d2) 1 #t)
(doc:compact-table! (reg:find-document $d2))