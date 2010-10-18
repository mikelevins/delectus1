(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; reading and writing csv
;;; ---------------------------------------------------------------------

(defparameter $alphabet '("A" "B" "C" "D" "E" "F" "G" "H" "I" 
                          "J" "K" "L" "M" "N" "O" "P" "Q" "R"
                          "S" "T" "U" "V" "W" "X" "Y" "Z"))

(defun extend-alphabet (alpha)
  (apply 'append
         (seq:image (^ (a) 
                      (seq:image (^ (b) (seq:concat a b)) 
                                 alpha))
                    alpha)))

(defun take-letters (n &optional (alphabet $alphabet))
  (if (<= n (length alphabet))
      (seq:take n alphabet)
      (take-letters n (append alphabet (extend-alphabet alphabet)))))

(defmethod read-csv ((path pathname) &key (first-row-headers? t))
  (let* ((csv-data (delete nil (fare-csv:read-csv-file path)))
         (row-length (length (first csv-data))))
    (if (every (^ (row) (= row-length (length row)))
               csv-data)
        (let ((rows (if first-row-headers?
                        (cdr csv-data)
                        csv-data))
              (columns (if first-row-headers?
                           (car csv-data)
                           (take-letters (length (first csv-data))))))
          (make-instance 'model :columns columns :rows rows))
        (error "Malformed CSV data"))))

(defmethod read-csv ((path string) &key (first-row-headers? t))
  (read-csv (pathname path) :first-row-headers? first-row-headers?))

#|
(time
 (progn
   (setf $zips (read-csv "/Applications/factor/extra/usa-cities/zipcode.csv"))
   'done))

(type-of $zips)
(time (columns $zips))
(time (column-index $zips "city"))
(time (value-at $zips "city" 21212))
(time (progn
        (put-value-at $zips "city" 21212 "Fort Mudge")
        'done))

(time (count-rows $zips))

(progn (time (add-column $zips "flavor")) 'done)
(time (value-at $zips "flavor" 21212))
(progn (time (add-row $zips 1)) 'done)
(progn (time (add-row $zips 43191)) 'done)
(time (value-at $zips "city" 43191))
(seq:element (rows $zips) 43191)
(seq:element (rows $zips) 43190)


(time
 (progn
   (setq $col (make-instance 'column :name "zip" :model $zips))
   'done))

(count-rows $col)
(elt (rows $zips) 0)

(setf $pres (make-instance 'presentation :model $zips))
(time (count-rows $pres))
(time (rows $pres))
(time (count-columns $pres))
(time (columns $pres))
(time (value-at $pres "city" 21345))

(time (value-at $pres "city" 21346))
(time (put-value-at $pres "city" 21346 "Whoville"))
(time (value-at $pres "city" 21346))

(time (add-column $pres "color"))

(time (value-at $pres "city" 0))



|#