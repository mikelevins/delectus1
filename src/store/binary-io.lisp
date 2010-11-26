;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          binary.lisp
;;;; Project:       Delectus 2
;;;; Purpose:       binary I/O
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :delectus)

;;; =====================================================================
;;; binary output
;;; =====================================================================

(defmethod write-bytes ((bytes vector)(out stream))
  (assert (subtypep (array-element-type bytes) '(unsigned-byte 8))()
          "The BYTES parameter to WRITE-BYTES must be a vector whose element-type is '(UNSIGNED-BYTE 8)")
  (write-sequence bytes out))

(defmethod write-binary ((str string) (out stream))
  )

(defmethod write-binary ((sym symbol)(out stream))
  )

(defmethod write-binary ((b (eql t)) (out stream))
  )

(defmethod write-binary ((b (eql nil)) (out stream))
  )

(defmethod write-binary ((vec vector)(out stream))
  )

(defmethod deleted-column-indexes ((pres presentation))
  #())

(defmethod deleted-row-indexes ((pres presentation))
  #())

(defmethod to-serialized-form ((pres presentation))
  (flexi-streams:with-output-to-sequence (out)
    (write-bytes $delectus-format-sentinel out)
    (write-byte (current-store-format-version) out)
    (write-binary (sort-column pres) out)
    (write-binary (sort-type pres) out)
    (write-binary (sort-reversed? pres) out)
    (write-binary (filter-text pres) out)
    (write-binary (show-deleted? pres) out)
    (write-binary (deleted-column-indexes pres) out)
    (write-binary (deleted-row-indexes pres) out)
    (write-binary (columns (model pres)) out)
    (write-binary (rows (model pres)) out)))

;;; =====================================================================
;;; binary input
;;; =====================================================================

(defmethod read-format-sentinel ((in stream))
  (let ((inbuf (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence inbuf in :start 0 :end 4)
    inbuf))

(defmethod read-format-version ((in stream))
  (let ((inbuf (make-array 1 :element-type '(unsigned-byte 8))))
    (read-sequence inbuf in :start 0 :end 1)
    (elt inbuf 0)))

(defmethod read-serialized-data ((in stream)(version (eql (current-store-format-version))))
  ;; sort-column
  ;; sort-type
  ;; sort-reversed?
  ;; filter-text
  ;; show-deleted?
  ;; deleted-column indexes
  ;; deleted-row indexes
  ;; columns
  ;; rows
  )


(defmethod from-serialized-form ((bytes vector))
  (flexi-streams:with-input-from-sequence (in bytes)
    (let ((format-sentinel (read-format-sentinel in)))
      (if (equalp format-sentinel $delectus-format-sentinel)
          (let ((format-version (read-format-version in)))
            (read-serialized-data in format-version))
          (error "Invalid data format")))))

;;; (setq $bin (to-serialized-form $pres))
;;; (from-serialized-form $bin)