
(define $movies #f)
(time 
 (set! $movies (read-delectus-file "/Users/mikel/Projects/delectus/drafts/delectus1/test/Movies.delectus")))

(describe-store $movies)