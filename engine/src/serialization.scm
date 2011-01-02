;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          serialization.scm
;;;; Project:       Delectus
;;;; Purpose:       serialization and deserialization of delectus structures
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type delectus-container
  id: 325DABB4-1D51-4B3D-B083-2F3BC0408E99
  constructor: %make-delectus-container
  uuid
  delectus 
  show-deleted?
  sort-column sort-order sort-type)

(define (to-serialized-form dview)
  (let ((container (%make-delectus-container (delectus-uuid (view-delectus dview)) 
                                             (view-delectus dview)
                                             (view-show-deleted? dview)
                                             (view-sort-column dview)
                                             (view-sort-order dview)
                                             (view-sort-type dview))))
    (object->u8vector container)))

(define (from-serialized-form container-bytes)
  (let* ((container (u8vector->object container-bytes))
         (del (delectus-container-delectus container))
         (dview (%make-view del
                            (delectus-container-show-deleted? container)
                            (delectus-container-sort-column container)
                            (delectus-container-sort-order container)
                            (delectus-container-sort-type container)
                            #f
                            #t)))
    dview))

