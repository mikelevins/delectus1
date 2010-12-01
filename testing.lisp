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
(value-at $zips "city" 0)
(length (rows $zips))

;;; ---------------------------------------------------------------------
;;; creating documents
;;; ---------------------------------------------------------------------

(time 
 (progn
     (setf $doc
      (make-instance 'document
                     :presentation (make-instance 'presentation :model $zips)))))

;;; ---------------------------------------------------------------------
;;; presentations
;;; ---------------------------------------------------------------------

(setq $pres (make-instance 'presentation :model $zips))

(time
 (progn
   (store $pres "/tmp/zips.delectus")
   'done))

(time
 (progn
   (setf $repres (reload-presentation "/tmp/zips.delectus"))
   'done))

;;; ---------------------------------------------------------------------
;;; saving and loading
;;; ---------------------------------------------------------------------

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

(columns $loaded-zips)
(type-of (columns $loaded-zips))
(count-elements (rows $zips))
(type-of (rows $zips))
(value-at $zips "city" 43190)

