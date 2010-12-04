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

