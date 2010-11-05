(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; support
;;; ---------------------------------------------------------------------

(define-condition delectus-file-type-error ()())

(defmethod read-delectus-data ((path pathname))
  (let* ((filetype (pathname-type path)))
    (cond
      ((string= filetype "delectus")(load-model path))
      ((string= filetype "csv")(read-csv path))
      (t (error 'delectus-file-type-error)))))

(defmethod read-delectus-data ((path string))
  (read-delectus-data (pathname path)))

;;; ---------------------------------------------------------------------
;;; application
;;; ---------------------------------------------------------------------

(defclass application ()
  ((documents :accessor documents :initform nil)
   (untitled-index :accessor untitled-index :initform 0))
  (:metaclass singleton-class))

(defmethod untitled-index ((app application))
  (setf (untitled-index app)(1+ (untitled-index app)))
  (untitled-index app))

(defmethod new-untitled-document ()
  (let* ((document-name (format nil "Untitled ~A" (untitled-index (app))))
         (model (make-instance 'model))
         (presentation (make-instance 'presentation :model model))
         (document (make-instance 'document :name document-name :presentation presentation)))
    (setf (documents (app))(cons document (documents (app))))
    (show document)
    document))

(defmethod open-document ((path pathname))
  (let* ((document-name (pathname-name path))
         (model (read-delectus-data path))
         (presentation (make-instance 'presentation :model model))
         (document (make-instance 'document :presentation presentation :name document-name)))
    (setf (documents (app))(cons document (documents (app))))
    (show document)
    document))

(defmethod open-document ((path string))
  (open-document (pathname path)))

(defun app ()
  (make-instance 'application))

(defmethod find-document ((win interface))
  (seq:find (^ (doc)(equal win (window doc))) 
            (documents (app))))

;;; (app)