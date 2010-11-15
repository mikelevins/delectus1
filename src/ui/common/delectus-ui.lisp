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
;;; main UI
;;; ---------------------------------------------------------------------

(define-interface delectus-ui (cocoa-default-application-interface)
  ;; slots
  ()
  ;; menus
  (:menus
   (application-menu "Delectus"
                     ((:component (("About Delectus" :callback 'delectus-about :callback-type :none)))
                      (:component (("Preferences..." :callback 'delectus-preferences :callback-type :none)))
                      (:component ()
                       ;; This is a special named component where the CAPI will
                       ;; attach the standard Services menu.
                       :name :application-services)
                      (:component (("Hide Delectus" :accelerator "accelerator-h" :callback-data :hidden)
                                   ("Hide Others" :accelerator "accelerator-meta-h" :callback-data :others-hidden)
                                   ("Show All" :callback-data :all-normal))
                                  :callback #'(setf capi:top-level-interface-display-state)
                                  :callback-type :data-interface)
                      (:component (("Quit Delectus" :accelerator "accelerator-q"
                                                    :callback 'capi:destroy :callback-type :interface)))))
   (file-menu "File" () :items-function 'file-menu-items)
   (edit-menu "Edit" () :items-function 'edit-menu-items)
   (windows-menu "Window" () :items-function 'windows-menu-items)
   (help-menu "Help" () :items-function 'help-menu-items))
  ;; menubar
  (:menu-bar application-menu file-menu edit-menu windows-menu help-menu)
  ;; defaults
  (:default-initargs :title "Delectus" 
    :application-menu 'application-menu))

;;; ---------------------------------------------------------------------
;;; application
;;; ---------------------------------------------------------------------

(defclass application ()
  ((documents :accessor documents :initform nil)
   (active-interface :accessor active-interface :initform nil)
   (untitled-index :accessor %untitled-index :initform 0)
   (ui :accessor ui :initform (make-instance 'delectus-ui)))
  (:metaclass singleton-class))

(defun app ()
  (make-instance 'application))

(defmethod untitled-index ((app application))
  (setf (%untitled-index app)(1+ (%untitled-index app)))
  (%untitled-index app))

(defmethod new-untitled-document ()
  (let* ((document-name (format nil "Untitled ~A" (untitled-index (app))))
         (model (make-instance 'model))
         (presentation (make-instance 'presentation :model model))
         (document (make-instance 'document :name document-name :presentation presentation)))
    (setf (documents (app))(cons document (documents (app))))
    (show document)
    document))

(defmethod find-document ((win interface))
  (seq:find (^ (doc)(equal win (window doc))) 
            (documents (app))))


