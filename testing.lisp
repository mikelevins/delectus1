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
   (store $zips "/Users/mikel/Desktop/testzips.out")
   'done))

(time 
 (progn
   (setq $loaded-zips (load-model (cl-user::path "test-data/zipcode.out")))
   'done))


(time 
 (progn
   (setq $loaded-zips (load-model "/Users/mikel/Desktop/testzips2.out"))
   'done))


