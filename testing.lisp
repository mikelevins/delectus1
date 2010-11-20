;;; testing code

(in-package :delectus)


(time
 (progn
   (setf $zips (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))
   'done))

(type-of $zips)
(type-of (rows $zips))

(time 
 (progn
   (setq $serialized-zips (to-serialized-form $zips))
   'done))

(length $serialized-zips)

(time 
 (progn
   (store-data $serialized-zips (cl-user::path "test-data/zipcode.out"))
   'done))

(time 
 (progn
   (store $zips (cl-user::path "test-data/zipcode.out"))
   'done))





(time 
 (progn
   (setq $reloaded-data (load-data (cl-user::path "test-data/zipcode.out")))
   'done))

(length $reloaded-data)
