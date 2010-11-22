;;; testing code

(in-package :delectus)


(time
 (progn
   (setf $zips (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))
   'done))

(time
 (progn
   (setf $logms (read-csv (namestring (cl-user::path "test-data/logmessages.csv")) 
                          :first-row-headers? nil))
   'done))

(time 
 (progn
     (setf $doc
      (make-instance 'document
                     :presentation (make-instance 'presentation :model $logms)))))

;;; yay! this works:
(objc:invoke
 (slot-value 
  (slot-value (first (layout-description (table-rows (window $doc))))
              'capi-internals::representation)
  'capi-cocoa-library::main-view)
 "setUsesAlternatingRowBackgroundColors:" t)

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


