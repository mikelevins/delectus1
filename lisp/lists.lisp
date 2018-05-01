;;; lists.lisp
;;; constructing, accessing, reading, and writing Delectus lists in
;;; Lisp

(in-package :delectus)

(defun length= (seq1 seq2)
  (= (length seq1)
     (length seq2)))

(defun lists-congruent-p (ref-list lists)
  (every (lambda (s)(length= ref-list s))
         lists))

(defun make-row (value-list)
  (mapcar #'identity value-list))

(defun delectus-list (title username columns rows &optional (note ""))
  (assert (lists-congruent-p columns rows)()
          "Some rows have lengths different from the number of columns in ~S"
          columns)
  `((:|user| . ,(username->dbname username))
    (:|type| . "List")
    (:|title| . ,title)
    (:|note| . ,note)
    (:|created_time| . ,(local-time:format-timestring nil (local-time:now)))
    (:|modified_time| . "")
    (:|columns| . ,(mapcar #'identity columns))
    (:|rows| . ,(mapcar #'make-row rows))))

;;; (defparameter $minimovies (delectus-list "Mini-Movies" "delectus" '("title" "star" "rating") '(("Bruce Almighty" "Jim Carrey" "***")("Home Alone" "Macauley Culkin" "***")("The Maltese Falcon" "Humphrey Bogart" "*****"))))

