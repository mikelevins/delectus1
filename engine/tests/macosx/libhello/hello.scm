(define (catch-all-errors thunk)
  (with-exception-catcher
   (lambda (exc)
     (write-to-string exc))
   thunk))

(c-define (hello str) (char-string) char-string "hello" "extern"
  (catch-all-errors
    (lambda () 
      (string-append "Hello, " str))))
