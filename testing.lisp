;;; testing code

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; reading csv files
;;; ---------------------------------------------------------------------

(time (progn
        (setf $zips (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))
        'done))

(time (progn
        (sort! $zips :key (index-sort-key 1))
        'done))

(count $zips)

(item $zips 100)
(item $zips 101)

(time (progn
        (remove! $zips 100)
        'done))

(gzip-stream:with-open-gzip-file (out "/tmp/test.gz" 
                                      :direction :output 
                                      :if-does-not-exist :create
                                      :if-exists :supersede
                                      :element-type '(unsigned-byte 8))
  (princ "Foobar" out))

(gzip-stream:with-open-gzip-file (in "/tmp/test.gz" 
                                      :direction :input
                                      :element-type '(unsigned-byte 8))
  (read in))