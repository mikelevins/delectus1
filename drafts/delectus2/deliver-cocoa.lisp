(in-package "CL-USER")

(load-all-patches)
(load "/Users/mikel/quicklisp/setup.lisp")
(require :asdf)
(load "delectus.asd")
(load-delectus)
(set-delivered t)
(deliver 'delectus::delectus
         (write-macos-application-bundle
          (path "build/Delectus")
          :template-bundle (pathname-location (path "template/Delectus.app/")))
         0
         :interface :capi
         :quit-when-no-windows nil)

