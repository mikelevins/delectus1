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
   (cl-store:store $zips (cl-user::path "test-data/zipcode.out"))
   'done))

(time 
 (progn
   (setq $reloaded-zips (cl-store:restore (cl-user::path "test-data/zipcode.out")))
   'done))

(type-of $reloaded-zips)
(seq:length (rows $reloaded-zips))


(setq $m (make-model :columns nil :rows nil))
(describe $m)