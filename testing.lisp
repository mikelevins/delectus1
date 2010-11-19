;;; testing code

(in-package :delectus)


(time
 (progn
   (setf $zips (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))
   'done))

(type-of $zips)
(columns $zips)
(seq:element (rows $zips) 0)

(time (cl-store:store $zips (cl-user::path "test-data/zipcode.out")))
(time (setq $restored-zips (cl-store:restore (cl-user::path "test-data/zipcode.out"))))
(type-of $restored-zips)
(value-at $restored-zips "city" 43190)
(time (%canonicalize-column! $restored-zips "city"))

(time
 (progn 
   (setq $pres (make-instance 'presentation :model $zips))
   'done))

(time (cl-store:store $pres (namestring (cl-user::path "test-data/zips.delectus"))))
(time (setq $restored-pres (cl-store:restore (namestring (cl-user::path "test-data/zips.delectus")))))
(type-of $restored-pres)
(value-at $restored-pres "city" 43190)
(presented-columns $restored-pres)
(count-rows $restored-pres)


(setq $doc
      (make-instance 'document
                     :presentation
                     (make-instance 'presentation
                                    :model (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))))

(time (save-document $doc "/tmp/zips.out"))
