;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          storage.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       serialization and storage of the data model
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; constants
;;; ---------------------------------------------------------------------

(defparameter $delectus-format-alpha-1 0)
(defparameter $delectus-format-alpha-2 1)
(defparameter $delectus-format-alpha-4 2)
(defparameter $delectus-format-beta-2 3)
(defparameter $delectus-format-1.9a 4)

(defun current-store-format-version () $delectus-format-1.9a)

(defparameter $delectus-format-sentinel #(68 69 76 67))

;;; ---------------------------------------------------------------------
;;; serialization tags
;;; ---------------------------------------------------------------------

(defparameter $tag-delectus-document 4)
(defparameter $tag-columns 5)
(defparameter $tag-rows 6)

;;; ---------------------------------------------------------------------
;;; serializing the state of a presentation
;;; ---------------------------------------------------------------------

(defun write-binary-sort-column (pres out)
  (write-binary (sort-column pres) out))

(defun write-binary-sort-type (pres out)
  (write-binary (sort-type pres) out))

(defun write-binary-sort-reversed? (pres out)
  (write-binary (sort-reversed? pres) out))

(defun write-binary-filter-text (pres out)
  (write-binary (filter-text pres) out))

(defun write-binary-show-deleted? (pres out)
  (write-binary (show-deleted? pres) out))

(defun write-binary-deleted-columns (pres out)
  (write-binary (as 'vector (deleted-columns pres)) out))

(defun write-binary-columns (pres out)
  (write-tag $tag-columns out)
  (write-binary (as 'vector (columns (model pres)))))

(defun write-binary-deleted-rows (pres out)
  (write-binary (as 'vector (deleted-rows pres)) out))

(defun write-binary-rows (pres out)
  (write-tag $tag-rows out)
  (write-binary (as 'vector (rows (model pres)))))

(defmethod write-binary ((pres presentation)(out vector))
  (write-tag $tag-delectus-document out)
  (write-binary-sort-column pres out)
  (write-binary-sort-type pres out)
  (write-binary-sort-reversed? pres out)
  (write-binary-filter-text pres out)
  (write-binary-show-deleted? pres out)
  (write-binary-deleted-columns pres out)
  (write-binary-deleted-rows pres out)
  (write-binary-columns pres out)
  (write-binary-rows pres out))

;;; ---------------------------------------------------------------------
;;; deserializing the state of a presentation
;;; ---------------------------------------------------------------------

(defmethod read-tagged-data ((inbuf array)(tag (eql $tag-rows))(index integer))
  )

(defmethod read-tagged-data ((inbuf array)(tag (eql $tag-columns))(index integer))
  )

(defmethod read-tagged-data ((inbuf array)(tag (eql $tag-delectus-document))(index integer))
  )


