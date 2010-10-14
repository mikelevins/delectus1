(in-package :delectus)

(defparameter $zipcodes-path "/Applications/factor/extra/usa-cities/zipcode.csv")

(time
 (progn
   (setq $test-csv
         (fare-csv:read-csv-file $zipcodes-path))
   'done))

(time
 (progn
   (setq $test-model
         (seq:filter (complement 'seq:empty?)
                     (as 'fset:seq (seq:image (^ (c)(as 'fset:seq c))
                                              $test-csv))))
   'done))

(type-of $test-model)
(delectus::columns $test-model)
(seq:length (delectus::rows $test-model))
(seq:element $test-model (random (seq:length $test-model)))

(time (seq:element (add-column $test-model "Foo") (random (seq:length $test-model))))
(time (seq:element (add-column $test-model "foo") 0))

(setq $model (make-instance 'delectus-model :data $test-model))
(setf (deleted-columns $model) '(1))
(seq:element $test-model 0)
(seq:element $test-model 1)
(time (seq:element (remove-deleted-items $model $test-model) 0))

(column-name->index $model "city")

