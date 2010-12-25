(define (catch-all-errors thunk)
  (with-exception-catcher
   (lambda (exc)
     (write-to-string exc))
   thunk))

(c-define (hello str) (char-string) int "hello" "extern"
  (catch-all-errors
    (lambda () 
      (newline)
      (display "hello, ")
      (display str)
      (newline)
      0)))
