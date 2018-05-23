;;; store.lisp
;;; interaction with the Delectus store

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
  (let ((bytes (sha3:sha3-digest-vector
                (encode changelist)
                :output-bit-length 256)))
    (string-downcase
     (with-output-to-string (out)
       (loop for byte across bytes
          do (format out "~2,'0X" byte))))))

;;; ---------------------------------------------------------------------
;;; stores
;;; ---------------------------------------------------------------------

;;; execute with an open db
(defun get-changes (open-db)
  (mapcar #'car (execute-to-list open-db "SELECT message from Changelog")))

(defmethod create-store ((store-directory pathname)
                         &key
                           (format-version *delectus-version-string*)
                           (storeid (make-identity))
                           (username "anonymous"))
  (let ((storefile (merge-pathnames storeid store-directory)))
    (when (probe-file storefile)
      (error 'store-creation-error
             :store-pathname storefile
             :message (format nil "Store ~S already exists at pathname ~S"
                              storeid storefile)))
    (with-open-database (db storefile)
      ;; the Metadata table
      (execute-non-query db "CREATE TABLE Delectus (rowid integer PRIMARY KEY, key text NOT NULL, value text NOT NULL);")
      (execute-non-query db (format nil "INSERT INTO Delectus (key,value) VALUES ('format_version','~A')"
                                    format-version))
      (execute-non-query db (format nil "INSERT INTO Delectus (key,value) VALUES ('storeid','~A')"
                                    storeid))
      (execute-non-query db (format nil "INSERT INTO Delectus (key,value) VALUES ('username','~A')"
                                    username))
      ;; the Directory table
      (execute-non-query db "CREATE TABLE Directory (rowid integer PRIMARY KEY, identity text NOT NULL, title text NOT NULL, type text DEFAULT 'List');")
      ;; the Changelog
      (execute-non-query db "CREATE TABLE Changelog (rowid integer PRIMARY KEY, timestamp text NOT NULL, message text NOT NULL, state text NOT NULL);")
      (let* ((timestamp (timestamp-now))
             (message (format nil "(:CREATE-STORE \"~A\")" storeid)))
        (execute-non-query db (format nil "INSERT INTO Changelog (timestamp,message,state) VALUES ('~A','~A','~A')"
                                      timestamp message "NULL"))
        (let* ((changes (get-changes db))
               (state-token (compute-state-token changes)))
          (execute-non-query db (format nil "UPDATE Changelog SET state = '~A' WHERE message = '~A'"
                                        state-token message)))))))

(defmethod create-store ((store-directory string)
                         &key
                           (format-version *delectus-version-string*)
                           (storeid (make-identity))
                           (username "anonymous"))
  (create-store (pathname store-directory)
                :format-version format-version
                :storeid storeid
                :username username))

;;; (create-store "/Users/mikel/.delectus/store/")
