(define $doc1 (doc:make))
$doc1 ; should be a document

(doc:add-column! $doc1 "Name")
$doc1 ; should have a "Name" column

(doc:add-row! $doc1)
$doc1 ; should have 1 empty row


