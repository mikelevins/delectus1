;;; testing code

(in-package :delectus)


(time
 (progn
   (setf $zips (read-csv "/Applications/factor/extra/usa-cities/zipcode.csv"))
   'done))

(time 
 (progn
   (save-model $zips "/Users/mikel/Desktop/test.delectus")
   'done))

(time 
 (progn
   (setf $zips1 (load-model "/Users/mikel/Desktop/test.delectus"))
   'done))

(value-at $zips1 "city" 0)
(value-at $zips1 "city" 22022)

(setq $col1 (make-instance 'column :deleted nil :label "Foo"))
(setq $col1-data (to-serialized-form $col1))
(setq $col1a (deserialize-column $col1-data))
(describe $col1a)

(setq $row1 (make-instance 'row :deleted nil :elements (as 'fset:seq (seq:image #'box:make '(0 1 2 3 4)))))
(setq $row1-data (to-serialized-form $row1))
(setq $row1a (deserialize-row $row1-data))
(describe $row1a)

(time 
 (progn
   (setq $m1 (to-serialized-form $zips))
   'done))