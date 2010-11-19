;;; testing code

(in-package :delectus)


(time
 (progn
   (setf $zips (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))
   'done))

(time 
 (progn
   (store $zips (cl-user::path "test-data/zipcode.out"))
   'done))

(time 
 (progn
   (setq $zips-serialized (to-serialized-form $zips))
   'done))

(array-dimensions (elt $zips-serialized 4))

(time 
 (progn
   (setq $reloaded-zips (reload (cl-user::path "test-data/zipcode.out")))
   'done))

(type-of $reloaded-zips)
(seq:length (rows $reloaded-zips))


