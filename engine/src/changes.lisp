;;; changes.lisp
;;; change API

(in-package :engine)

;;; ---------------------------------------------------------------------
;;; errors
;;; ---------------------------------------------------------------------

(define-condition store-creation-error (error)
  ((store-pathname :reader store-pathname :initarg :store-pathname)
   (message :reader message :initarg :message)))

;;; ---------------------------------------------------------------------
;;; state
;;; ---------------------------------------------------------------------

(defun compute-state-token (changelist)
  (let ((bytes (sha3:sha3-digest-vector (encode changelist)
                                        :output-bit-length 256)))
    (string-downcase
     (with-output-to-string (out)
       (loop for byte across bytes
          do (format out "~2,'0X" byte))))))

(defparameter *empty-state-token*
  (compute-state-token '()))

;;; ---------------------------------------------------------------------
;;; Operations on stores
;;; ---------------------------------------------------------------------

;;; TODO: compute the state token for the newly-created changelog in
;;; the new store, and store it in the 'state' row of the Delectus2
;;; table.

(defmethod create-store ((store-id string))
  (ensure-directories-exist (node-store-directory))
  (let* ((store-pathname (merge-pathnames store-id (node-store-directory)))
         (already (probe-file store-pathname)))
    (when already
      (error 'store-creation-error
             :message (format nil "Store ~A at path ~S already exists"
                              store-id store-pathname)
             :store-pathname store-pathname))
    ;; the store doesn't already exist; create it
    (with-open-database (db store-pathname)
      ;; the Metadata table
      (execute-non-query db "CREATE TABLE Delectus2 (rowid integer PRIMARY KEY, key text NOT NULL, value text NOT NULL);")
      (execute-non-query db (format nil "INSERT INTO Delectus2 (key,value) VALUES ('version','~A')"
                                    *delectus-version-string*))
      (execute-non-query db (format nil "INSERT INTO Delectus2 (key,value) VALUES ('node','~A')"
                                    (local-nodeid)))
      (execute-non-query db (format nil "INSERT INTO Delectus2 (key,value) VALUES ('storeid','~A')"
                                    store-id))
      (execute-non-query db (format nil "INSERT INTO Delectus2 (key,value) VALUES ('username','~A')"
                                    *anonymous-identity*))
      (execute-non-query db (format nil "INSERT INTO Delectus2 (key,value) VALUES ('state','~A')"
                                    *empty-state-token*))
      ;; the Directory table
      (execute-non-query db "CREATE TABLE Directory (rowid integer PRIMARY KEY, identity text NOT NULL, title text NOT NULL, type text DEFAULT 'String');")
      ;; the Changelog
      (execute-non-query db "CREATE TABLE Changelog (rowid integer PRIMARY KEY, execute_order integer DEFAULT 0, timestamp text NOT NULL, message text NOT NULL);")
      (execute-non-query db (format nil "INSERT INTO Changelog (execute_order,timestamp,message) VALUES (0,'~A','~A')"
                                    (timestamp-now)
                                    (format nil "(:CREATE-STORE \"~A\")" store-id))))))

(defmethod create-store ((store-id null))
  (create-store (make-identity)))

;;; (create-store nil)
