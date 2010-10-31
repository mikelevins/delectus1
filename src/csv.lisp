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
  (let* ((csv-data (delete nil (fare-csv:read-csv-file path))))
    (let ((rows (if first-row-headers?
                    (cdr csv-data)
                    csv-data))
          (columns (if first-row-headers?
                       (car csv-data)
                       (take-letters (length (first csv-data))))))
      (model :columns columns :rows rows))))

(defmethod read-csv ((path string) &key (first-row-headers? t))
  (read-csv (pathname path) :first-row-headers? first-row-headers?))

#|
(time
 (progn
   (setf $zips (read-csv "/Applications/factor/extra/usa-cities/zipcode.csv"))
   'done))

(time (value-at $zips "city" 22022))

(setf $pres (make-instance 'presentation :model $zips))

(time (value-at $pres "city" 22022))
(time (value-at $pres "city" 0))
(time (value-at $pres "city" 286))
(time (value-at $pres "city" 43190))

(set-sort! $pres "city")
(set-sort! $pres "city" :reverse t)

(set-filter! $pres "Q")
(time (count-rows $pres))

|#