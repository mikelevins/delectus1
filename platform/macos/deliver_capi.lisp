
(in-package "CL-USER")

(load-all-patches)
(require :asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :delectus-engine)
(ql:quickload :delectus-capi-macos)

(defvar *project-root-path* (asdf:system-relative-pathname :delectus-engine ""))
(defvar *target-application-path* (merge-pathnames "platform/macos/Delectus2" *project-root-path*))
(defvar *bundle-template-path* (merge-pathnames "platform/macos/bundle-templates/Delectus2.app/" *project-root-path*))
(defvar *startup-image-path* (merge-pathnames "assets/images/delectus-splash.png" *project-root-path*))


(defun create-delectus-bundle (bundle-path)
  (create-macos-application-bundle bundle-path
                                   :template-bundle (lw:pathname-location *bundle-template-path*)))

;;; inform the built app that it has been built
(pushnew :delectus2 cl:*features*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; inform the delivered image that it's not a development-time image
  (setf (symbol-function 'cl-user::delivered-application-p)
        (constantly t)))

(deliver 'delectus-cocoa-application
         (create-delectus-bundle *target-application-path*)
         0
         :interface :capi
         :quit-when-no-windows nil
         :startup-bitmap-file *startup-image-path*)
