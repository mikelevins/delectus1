;;; testing code

(in-package :delectus)


(time
 (progn
   (setf $zips (read-csv "/Applications/factor/extra/usa-cities/zipcode.csv"))
   'done))

(time (cl-store:store $zips "/tmp/zips.out"))
(time (setq $restored-zips (cl-store:restore "/tmp/zips.out")))
(type-of $restored-zips)

(time
 (progn 
   (setq $pres (make-instance 'presentation :model $zips))
   'done))

(time (cl-store:store $pres "/tmp/zips.out"))
(time (setq $restored-pres (cl-store:restore "/tmp/zips.out")))
(type-of $restored-pres)
(value-at $restored-pres "city" 0)
(presented-columns $restored-pres)
(count-rows $restored-pres)


(setq $doc
      (make-instance 'document
                     :presentation
                     (make-instance 'presentation
                                    :model (read-csv "/Applications/factor/extra/usa-cities/zipcode.csv"))))

(time (save-document $doc "/tmp/zips.out"))
