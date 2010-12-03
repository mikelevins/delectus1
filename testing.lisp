;;; testing code

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; reading csv files
;;; ---------------------------------------------------------------------

(time
 (progn
   (setf $zips (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))
   'done))

(columns $zips)
(time (value-at $zips "city" 0))
(time (length (rows $zips)))
(time (max-item-length (rows $zips) 1))


