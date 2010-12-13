
(define $csv #f)
(time 
 (set! $csv (csv:read-csv-file "/Users/mikel/Projects/delectus/drafts/delectus2/test-data/zipcode.csv")))

(define $bytes #f)
(time 
 (set! $bytes (read-binary-file "/Users/mikel/Projects/delectus/drafts/delectus1/test/Movies.delectus")))

(time 
 (set! $bytes (read-binary-file "/Users/mikel/Projects/delectus/drafts/delectus2/test-data/zipcode.csv")))

(time 
 (write-binary-file $bytes "/Users/mikel/Desktop/test-bytes.out"))

(define $movies #f)
(time 
 (set! $movies (read-delectus-file "/Users/mikel/Projects/delectus/drafts/delectus1/test/Movies.delectus")))

(describe-store $movies)