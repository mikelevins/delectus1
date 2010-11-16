;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          document.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       platform-independent document code
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

(defmethod show ((doc document))
  (display (window doc)))

(defmethod notify-document-changed! ((doc document))
  (notify-redisplay-document doc))

;;; ---------------------------------------------------------------------
;;;  model methods

(defmethod count-rows ((doc document))
  (count-rows (presentation doc)))

(defmethod value-at ((doc document)(column-name string)(row integer))
  (value-at (presentation doc) column-name row))

(defmethod put-value-at! ((doc document)(column-name string)(row integer) val)
  (put-value-at! (presentation doc) column-name row val))

(defmethod document-add-row! ((doc document))
  (clear-sort! (presentation doc))
  (clear-filter! (presentation doc))
  (add-row! (presentation doc))
  (notify-document-changed! doc))

(defmethod delete-selected-row! ((doc document))
  (let ((row (get-selected-row doc)))
    (mark-row-deleted! row t)
    (notify-document-changed! doc)))

(defmethod document-add-column! ((doc document))
  (let ((label (prompt-for-string "Choose a name for the new column:")))
    (if (seq:find (^ (c)(equalp label (label c)))
                  (columns (get-model (presentation doc))))
        (display-message (format nil "The column '~A' already exists" label))
        (progn
          (clear-sort! (presentation doc))
          (clear-filter! (presentation doc))
          (add-column! (presentation doc) label)
          (notify-document-changed! doc)
          (setup-columns (row-pane (window doc)) doc)))))

(defmethod delete-selected-column! ((doc document))
  (let ((col (get-selected-column doc)))
    (mark-column-deleted! col t)
    (notify-document-changed! doc)))

(defmethod toggle-trash ((doc document))
  (setf (show-deleted? doc)
        (not (show-deleted? doc)))
  (notify-redisplay-document doc))

;;; ---------------------------------------------------------------------
;;;  common
;;; ---------------------------------------------------------------------

;;; serialization

(defmethod save-document ((doc document)(path pathname))
  (let ((pres (presentation doc)))
    (cl-store:store pres path)
    (setf (pathname doc) path)
    path))

(defmethod save-document ((doc document)(path string))
  (save-document doc (pathname path)))

(defmethod open-document ((path pathname))
  (let* ((document-name (pathname-name path))
         (presentation (cl-store:restore path))
         (document (make-instance 'document :presentation presentation :name document-name)))
    (setf (pathname document) path)
    (setf (documents (app))(cons document (documents (app))))
    (show document)
    document))

(defmethod open-document ((path string))
  (open-document (pathname path)))
