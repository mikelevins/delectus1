
(in-package "CL-USER")

(load-all-patches)
(require :asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


(load "/Users/mikel/Workshop/src/delectus/delectus.asd")

(defvar *target-application-path* "/Users/mikel/Workshop/src/delectus/delivery/macos/Delectus2.0.0d1")

(asdf:load-system :delectus)

(defun cocoa-application-interface-item-title (self prefix)
  (string-append prefix " " (capi:interface-title self)))

(defun cocoa-application-interface-multiple-about ()
  (capi:display-message-on-screen (capi:convert-to-screen nil)
                                  "Delectus2 pre-alpha build"))

(defun cocoa-application-interface-multiple-preferences ()
  )

(defun cocoa-application-interface-multiple-message (self message &rest args)
  (declare (ignore self))
  )

(defun handle-open-file ()
  (let ((path (capi:prompt-for-file "Open a Delectus list..." :filter "*.delectus2")))
    (when path
      (capi:contain (make-instance 'delectus-ui::items-sheet :dbpath path)))))

(capi:define-interface delectus2-application (capi:cocoa-default-application-interface)
  ()
  (:menus
   (application-menu
    (capi:interface-title capi:interface)
    ((:component
      (((cocoa-application-interface-item-title capi:interface "About")
        :callback 'cocoa-application-interface-multiple-about
        :callback-type :none)))
     (:component
      (("Preferences..."
        :callback 'cocoa-application-interface-multiple-preferences
        :callback-type :none)))
     (:component
      ()
      ;; This is a special named component where the CAPI will
      ;; attach the standard Services menu.
      :name :application-services)
     (:component
      (((cocoa-application-interface-item-title capi:interface "Hide")
        :accelerator "accelerator-h"
        :callback-data :hidden)
       ("Hide Others"
        :accelerator "accelerator-meta-h"
        :callback-data :others-hidden)
       ("Show All"
        :callback-data :all-normal))
      :callback #'(setf capi:top-level-interface-display-state)
      :callback-type :data-interface)
     (:component
      (((cocoa-application-interface-item-title capi:interface "Quit")
        :accelerator "accelerator-q"
        :callback 'capi:destroy
        :callback-type :interface)))))
   (file-menu
    "File"
    (("Open"
      :callback 'handle-open-file
      :callback-type :none))))
  (:menu-bar application-menu file-menu)
  (:default-initargs
      :title "Delectus2"
    :application-menu 'application-menu
    :message-callback 'cocoa-application-interface-multiple-message))

(defun delectus-cocoa-application ()
  (let ((application (make-instance 'delectus2-application)))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with no windows initially.
    (capi:convert-to-screen nil)))

(deliver 'delectus-cocoa-application
         (create-macos-application-bundle
          *target-application-path*
          :template-bundle (pathname-location
                            (current-pathname "bundle-templates/Delectus2.app/")))
         0
         :interface :capi
         :quit-when-no-windows nil)