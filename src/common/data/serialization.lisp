(in-package :delectus)

(defmethod save-document ((doc document)(path pathname))
  (let ((pres (presentation doc)))
    (cl-store:store pres path)
    (setf (pathname doc) path)
    path))

(defmethod save-document ((doc document)(path string))
  (save-document doc (pathname path)))

(defmethod open-document ((path pathname))
  (let* ((document-name (pathname-name path))
         (presentation (cl-store:restore path))
         (document (make-instance 'document :presentation presentation :name document-name)))
    (setf (pathname document) path)
    (setf (documents (app))(cons document (documents (app))))
    (show document)
    document))

(defmethod open-document ((path string))
  (open-document (pathname path)))


