(define $zips #f)
(time (begin
        (set! $zips (read-csv-file "/Users/mikel/Projects/delectus/drafts/delectus2/test-data/zipcode.csv"))
        'done))

(time (begin
        (write-delectus-file $zips "/Users/mikel/Projects/delectus/delec/test/zips.delectus")
        'done))

(define $movies #f)
(set! $movies (read-delectus-file "/Users/mikel/Projects/delectus/delec/test/Movies.delectus"))

