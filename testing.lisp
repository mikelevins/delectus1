;;; testing code

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; sorting rows
;;; ---------------------------------------------------------------------

(progn (setf $model (read-csv (namestring (cl-user::path "test-data/zipcode.csv")))) 'done)
(progn (setf $model (read-csv (namestring (cl-user::path "test-data/logmessages.csv")) 
                              :first-row-headers? nil)) 'done)

(progn (setf $rows (rows $model)) 'done)
(length $rows)
(time (progn (setf $rows1 (sort-rows $rows 1 #'string>)) 'done))

;;; ---------------------------------------------------------------------
;;; reading csv files
;;; ---------------------------------------------------------------------

(time
 (progn
   (setf $zips (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))
   'done))

(describe (columns $zips))
(value-at $zips "city" 0)

(time
 (progn
   (setf $logms (read-csv (namestring (cl-user::path "test-data/logmessages.csv")) 
                          :first-row-headers? nil))
   'done))

;;; ---------------------------------------------------------------------
;;; presentations
;;; ---------------------------------------------------------------------

(setq $pres (make-instance 'presentation :model $logms))

(row-deleted? $pres 0)
(mark-row-deleted! $pres 0)
(row-deleted? $pres 1)

(columns $pres)
(column-deleted? $pres "A")
(mark-column-deleted! $pres "B")
(column-deleted? $pres "B")

(filter-match? $pres 0)

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

