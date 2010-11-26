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
;;; presentations
;;; ---------------------------------------------------------------------

(setq $pres (make-instance 'presentation :model $zips))

(columns $pres)
(setf (sort-column $pres) "city")
(setf (sort-reversed? $pres) nil)
(sort-reversed? $pres)
(mark-changed! $pres t)
(time (value-at $pres "city" 0))

(row-deleted? $pres (elt (rows $pres) 43190))
(mark-row-deleted! $pres (elt (rows $pres) 43190) t)
(mark-row-deleted! $pres (elt (rows $zips) 43190) nil)
(mark-changed! $pres t)

(value-at $pres "city" 43190)
(length (rows $pres))
(length (row-cache $pres))

(show-deleted? $pres)
(setf (show-deleted? $pres) nil)

(column-deleted? $pres "city")
(mark-column-deleted! $pres "city" t)
(mark-column-deleted! $pres "city" nil)

(update $pres)

(time
 (progn
   (setf $rows (rows $pres))
   'done))


;;; ---------------------------------------------------------------------
;;; creating documents
;;; ---------------------------------------------------------------------

(time 
 (progn
     (setf $doc
      (make-instance 'document
                     :presentation (make-instance 'presentation :model $logms)))))

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

