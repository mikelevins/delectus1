;;; users.lisp
;;; constructing valid CouchDB usernames for use with Delectus

(in-package :delectus)

(defun string->hexstring (str)
  (string-downcase
   (with-output-to-string (out)
     (format out "~{~X~}"
             (map 'list #'char-code str)))))

(defun username->dbname (username)
  (let ((hex (string->hexstring username)))
    (format nil "userdb-~A" hex)))
