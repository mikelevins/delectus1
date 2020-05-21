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
    (if (hcl:delivered-image-p)
        
        ;; delivered app:
        ;;   the delectus root path is the app bundle
        (delectus:bind ((image-path (lw:lisp-image-name))
                        (flag image-directory filename maybe-filename
                              (uiop:split-unix-namestring-directory-components image-path))
                        ;; two directories up from the image is the bundle path
                        (bundle-directory (subseq image-directory 0 (- (length image-directory) 2)))
                        (bundle-path (make-pathname :directory (cons flag bundle-directory)))
                        (lib-path (merge-pathnames "Contents/MacOS/" bundle-path)))
          (setf delectus:*delectus-root-pathname* bundle-path)
          ;; find and set up the correct SQLite library
          (setf delectus-libs:*delectus-libraries-path* lib-path)
          (pushnew delectus-libs::*delectus-libraries-path*
                   cffi:*foreign-library-directories*
                   :test #'equal)
          (cffi:define-foreign-library libsqlite
            (:darwin "libsqlite3.dylib")
            (:unix "libsqlite3.so")
            (:windows "libsqlite3.dll"))
          (cffi:use-foreign-library libsqlite)
          ;; does the sqlite library work?
          (format t "~%SQLite library version: ~A~%"
                  (fli:convert-from-foreign-string (delectus::sqlite3-libversion)))
          ;; yay! it works!
          )

        ;; development time:
        ;;   the delectus root path is the project root
        (setf delectus::*delectus-root-pathname*
              (asdf:system-relative-pathname :delectus "")))
    
    ;; debugging output on launch
    (format t "~%Bundle path: ~S~%" delectus::*delectus-root-pathname*)
    (format t "~%Dynamic library path: ~S~%" delectus-libs::*delectus-libraries-path*)
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with no windows initially.
    (capi:convert-to-screen nil)))
