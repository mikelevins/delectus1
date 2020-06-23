;;;; delectus2.asd

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :hunchentoot)
(ql:quickload :cl-who)

(asdf:defsystem #:delectus2
  :description "Describe Delectus2 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:hunchentoot :cl-who :parenscript :delectus-engine)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "delectus2")))))

;;; (asdf:load-system :delectus2)

(defun buildapp ()
  (asdf:load-system :delectus2)
  (save-lisp-and-die "delectus_engine.exe"
                     :toplevel 'cl-user::main
                     :executable t))

