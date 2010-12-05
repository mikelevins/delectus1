;;; testing code

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; reading csv files
;;; ---------------------------------------------------------------------

(time (progn
        (setf $zips (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))
        'done))

(keys $zips)
(elt (items $zips) 10000)

(time (with-open-file (out "/tmp/zips.sexp" :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (s-serialization:serialize-sexp $zips out)))

(time (progn (setq $rezips (with-open-file (in "/tmp/zips.sexp" :direction :input)
                             (s-serialization:deserialize-sexp in)))
             'done))
