(in-package "CL-USER")

(load-all-patches)

(load (current-pathname "../../configuration/macos-application-bundle.lisp"))

(deliver 'test-cocoa-application-full
         (write-macos-application-bundle
          *target-application-path*
          :template-bundle (pathname-location
                            (current-pathname "templates/FullApplication.app/")))
         5
         :interface :capi
         :quit-when-no-windows nil)

