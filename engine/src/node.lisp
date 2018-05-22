;;; node.lisp
;;; the endpoint that manages Delectus storage and processes messages

(in-package :engine)

(defun node-home-directory ()
  #+windows (merge-pathnames "_delectus/" (user-homedir-pathname))
  #-windows (merge-pathnames ".delectus/" (user-homedir-pathname)))

(defun node-store-directory ()
  (merge-pathnames "store/" (node-home-directory)))

(defun nodeid-pathname ()
  (merge-pathnames "nodeid" (node-home-directory)))

(defun local-nodeid ()
  (ensure-directories-exist (node-store-directory))
  (let ((nodeid-file (probe-file (nodeid-pathname))))
    (if nodeid-file
        (with-open-file (in (nodeid-pathname) :direction :input)
          (read-line in nil nil nil))
        (let* ((id (make-identity))
               (idpath (nodeid-pathname)))
          (with-open-file (out idpath :direction :output)
            (format out "~A~%" id))
          id))))

