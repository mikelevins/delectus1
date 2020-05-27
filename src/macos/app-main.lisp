;;;; ***********************************************************************
;;;;
;;;; Name:          app-main.lisp
;;;; Project:       delectus 2
;;;; Purpose:       the main macOS application
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:ui)

;;; ---------------------------------------------------------------------
;;; main entry point
;;; ---------------------------------------------------------------------

(in-package #:cl-user)



(defun delectus-cocoa-application ()
  (let ((application (make-instance 'ui::delectus2-application)))
    ;; initialize app parameters
    (if (cl-user::delivered-application-p)
        
        ;; delivered app:
        ;;   the delectus root path is the app bundle
        (delectus:bind ((image-path (lw:lisp-image-name))
                        (flag image-directory filename maybe-filename
                              (uiop:split-unix-namestring-directory-components image-path))
                        ;; two directories up from the image is the bundle path
                        (bundle-path (system::executable-application-bundle-directory))
                        (lib-path (merge-pathnames "Contents/MacOS/" bundle-path)))
          (setf delectus:*delectus-root-pathname* bundle-path)
          ;; find and set up the correct SQLite library
          (pushnew lib-path
                   cffi:*foreign-library-directories*
                   :test #'equal)
          (cffi:define-foreign-library sqlite3-lib
            (:darwin "libsqlite3.dylib")
            (:unix "libsqlite3.so")
            (:windows "libsqlite3.dll"))
          (cffi:use-foreign-library sqlite3-lib)
          ;; does the sqlite library work?
          (format t "~%SQLite library version: ~A~%"
                  (fli:convert-from-foreign-string (delectus::sqlite3-libversion)))
          ;; yay! it works!
          )

        ;; not a delivered app; development-time image:
        (progn
          ;;   the delectus root path is the project root
          (setf delectus::*delectus-root-pathname*
                (asdf:system-relative-pathname :delectus ""))))
    
    ;; debugging output on launch
    (format t "~%Delivered app? ~A~%"
            (if (cl-user::delivered-application-p) "Yes." "No."))
    (format t "~%Bundle path: ~S~%" delectus::*delectus-root-pathname*)
    (format t "~%SQLite3 loaded from: ~S~%" (CFFI:FOREIGN-LIBRARY-PATHNAME 'sqlite-ffi::sqlite3-lib))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with no windows initially.
    (capi:convert-to-screen nil)))
