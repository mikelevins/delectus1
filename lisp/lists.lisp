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

(defclass delectus-list ()
  ((title :accessor title :initform "" :initarg :title)
   (username :accessor username :initform nil :initarg :username)
   (columns :accessor columns :initform nil :initarg :columns)
   (rows :accessor rows :initform nil :initarg :rows)
   (note :accessor note :initform "" :initarg :note)
   (created-time :accessor created-time
                 :initform nil
                 :initarg :created-time)
   (modified-time :accessor modified-time :initform "" :initarg :modified-time)))

(defmethod initialize-instance :after ((dlist delectus-list)
                                       &rest initargs
                                       &key (created-time nil)
                                         &allow-other-keys)
  (declare (ignore initargs))
  (if created-time
      (setf (created-time dlist) created-time)
      (setf (created-time dlist) (local-time:format-timestring nil (local-time:now)))))

(defun delectus-list (title username
                      &key
                        (columns nil)
                        (rows nil)
                        (note "")
                        (created-time nil)
                        (modified-time nil))
  (assert (lists-congruent-p columns rows)()
          "Some rows have lengths different from the number of columns in ~S"
          columns)
  (make-instance 'delectus-list
                 :username username
                 :title title
                 :note note
                 :created-time created-time
                 :created-time modified-time
                 :columns (mapcar #'identity columns)
                 :rows (mapcar #'make-row rows)))

(defun write-delectus-alist (d stream)
  (write-char #\{ stream)
  (loop for e on d 
     do 
       (let ((cons (car e)))
         (cond ((stringp (car cons))
                (write-string (clouchdb::doublequote (car cons)) stream))
               ((symbolp (car cons))
                (clouchdb::write-json-symbol (car cons) stream)))
         (write-char #\: stream)
         (encode-delectus-data (cdr (car e)) stream))
     when (cdr e) do (write-char #\, stream))
  (write-char #\} stream))

(defun write-delectus-list (d stream)
  (write-char #\[ stream)
  (loop for e on d 
     do (encode-delectus-data (car e) stream)
     when (cdr e) do (write-char #\, stream))
  (write-char #\] stream))

(defun encode-delectus-data (d stream)
  (cond ((null d)
         (write-string "[]" stream))
        ((numberp d)
         (clouchdb::write-json-number d stream))
        ((symbolp d)
         (clouchdb::write-json-symbol d stream))
        ((stringp d)
         (clouchdb::write-json-string d stream))
        ((clouchdb::assoclp d)
         (write-delectus-alist d stream))
        ((listp d)
         (write-delectus-list d stream))))

(defmethod delectus-list->json ((dlist delectus-list))
  (let ((doc `((:|user| . ,(username->dbname (username dlist)))
               (:|type| . "List")
               (:|title| . ,(title dlist))
               (:|note| . ,(note dlist))
               (:|created_time| . ,(created-time dlist))
               (:|modified_time| . ,(modified-time dlist))
               (:|columns| . ,(columns dlist))
               (:|rows| . ,(rows dlist)))))
    (with-output-to-string (stream)
      (encode-delectus-data doc stream))))

;;; (defparameter $minimovies (delectus-list "Mini-Movies" "delectus"))
