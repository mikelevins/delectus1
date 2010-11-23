;;; testing code

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; sorting rows
;;; ---------------------------------------------------------------------

(progn (setf $model (read-csv (namestring (cl-user::path "test-data/zipcode.csv")))) 'done)
(progn (setf $model (read-csv (namestring (cl-user::path "test-data/logmessages.csv")) 
                              :first-row-headers? nil)) 'done)

(progn (setf $pres (make-instance 'presentation :model $model)) 'done)
(setf (sort-column $pres) "zip")
(setf (sort-column $pres) "A")

(time
 (progn
   (setq $rows (rows $model))
   'done))

(seq:length (rows $model))

(time (progn (setq $strings (seq:image (^ (row)(seq:element (elements row) 1)) (rows $model))) 'done))
(time (progn (fset:sort $rows #'row<) 'done))
(seq:length $rows)


;;; ---------------------------------------------------------------------
;;; reading csv files
;;; ---------------------------------------------------------------------

(time
 (progn
   (setf $zips (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))
   'done))

(describe (columns $zips))
(count-elements (rows $zips))
(value-at $zips "city" 43190)

(time
 (progn
   (setf $logms (read-csv (namestring (cl-user::path "test-data/logmessages.csv")) 
                          :first-row-headers? nil))
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

