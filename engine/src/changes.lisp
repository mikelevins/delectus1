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
;;; Operations on stores
;;; ---------------------------------------------------------------------

(defmethod create-store ((store-id string))
  (ensure-directories-exist *delectus-store-pathname*)
  (let* ((store-pathname (merge-pathnames store-id *delectus-store-pathname*))
         (already (probe-file store-pathname)))
    (when already
      (error 'store-creation-error
             :message (format nil "Store ~A at path ~S already exists"
                              store-id store-pathname)
             :store-pathname store-pathname))
    ;; the store doesn't already exist; create it
    (with-open-database (db store-pathname)
      (execute-non-query db "CREATE TABLE Directory (rowid integer PRIMARY KEY, identity text NOT NULL, title text NOT NULL, type text DEFAULT 'String');"))))

(defmethod create-store ((store-id null))
  (create-store (make-identity)))

;;; (create-store nil)
