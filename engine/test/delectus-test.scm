(api:version)
(define $d1 (api:make-document))
(api:add-column! $d1 "Name")
(api:add-row! $d1)
(api:value-at $d1 "Name" 0)
(api:put-value-at! $d1 "Name" 0 "Fred")
(api:value-at $d1 "Name" 0)


