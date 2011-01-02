;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          api.scm
;;;; Project:       Delectus
;;;; Purpose:       the delectus api
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------

(define $NONE         0)
(define $DESCENDING   1)
(define $ASCENDING    2)
(define $NUMERIC      3)
(define $ALPHABETICAL 4)

(define $NO-VALUE 0)
(define $NO-DELECTUS 0)
(define $NO_ERROR 0)

(define $CANT_CREATE        -1)
(define $CANT_ADD_ROW       -2)
(define $LABEL_IN_USE       -3)
(define $NO_SUCH_COLUMN     -4)
(define $INDEX_OUT_OF_RANGE -5)
(define $CANT_UPDATE        -6)
(define $CANT_WRITE         -7)
(define $CANT_READ          -8)
(define $BAD_FORMAT         -9)
(define $NO_SUCH_ID         -10)
(define $LABEL_IN_USE       -11)


(define (api:error? x)(< x 0))

;;; ---------------------------------------------------------------------
;;; API functions
;;; ---------------------------------------------------------------------

(define (api:version)
  (current-delectus-format-version))

(define (api:make-delectus)
  (register-view 
   (%make-view (make-delectus '() '())
               #f
               #f #f #f
               #f
               #f)))

(define (api:add-row! del)
  (let ((dview (find-view del)))
    (if dview
        (let* ((del (view-delectus dview))
               (rows (delectus-rows del))
               (new-row (ensure-row (repeat (count-columns del)(nothing))))
               (new-rows (vector-add-last rows new-row)))
          (delectus-rows-set! del new-rows)
          $NO_ERROR)
        $NO_SUCH_ID)))

(define (api:add-column! del label)
  (let ((dview (find-view del)))
    (if dview
        (let* ((del (view-delectus dview))
               (col (column-index del label)))
          (if col
              $LABEL_IN_USE
              (let ((new-cols (vector-add-last (delectus-columns del) (ensure-column label)))
                    (new-rows (vector-map (partial (flip row-add-last!) (nothing))
                                          (delectus-rows del))))
                (delectus-columns-set! del new-cols)
                (delectus-rows-set! del new-rows)
                $NO_ERROR)))
        $NO_SUCH_ID)))

(define (api:value-at del label index)
  (let ((dview (find-view del)))
    (if dview
        (let* ((del (view-delectus dview))
               (col (column-index del label)))
          (if col
              (if (< index (count-rows del))
                  (let ((row (vector-ref (view-rows dview) index)))
                    (vector-ref (row-elements row) col))
                  $NO-VALUE)
              $NO-VALUE))
        $NO-VALUE)))

(define (api:put-value-at! del label index val)
  (let ((dview (find-view del)))
    (if dview
        (let* ((del (view-delectus dview))
               (col (column-index del label)))
          (if col
              (if (< index (count-rows del))
                  (let ((row (vector-ref (view-rows dview) index)))
                    (vector-set! (row-elements row) col val))
                  $INDEX_OUT_OF_RANGE)
              $NO_SUCH_COLUMN))
        $NO_SUCH_ID)))

(define (api:mark-column-deleted! del label deleted?)
  (let ((dview (find-view del)))
    (if dview
        (let* ((del (view-delectus dview))
               (col (column-index del label)))
          (if col
              (let ((col (vector-ref (delectus-columns del) col)))
                (column-deleted?-set! col deleted?))
              $NO_SUCH_COLUMN))
        $NO_SUCH_ID)))

(define (api:mark-row-deleted! del index deleted?)
  (let ((dview (find-view del)))
    (if dview
        (let* ((del (view-delectus dview)))
          (if (< index (count-rows del))
              (let ((row (vector-ref (view-rows dview) index)))
                (row-deleted?-set! row deleted?))
              $INDEX_OUT_OF_RANGE))
        $NO_SUCH_ID)))

(define (api:show-deleted del)
  (let ((dview (find-view del)))
    (if dview
        (view-show-deleted? dview)
        $NO_SUCH_ID)))

(define (api:set-show-deleted! del show?)
  (let ((dview (find-view del)))
    (if dview
        (view-show-deleted?-set! dview show?)
        $NO_SUCH_ID)))

(define (api:compact-delectus! del)
  0)

(define (api:sort-column del)
  "")

(define (api:set-sort-column! del label)
  0)

(define (api:sort-order del)
  0)

(define (api:set-sort-order! del order)
  0)

(define (api:sort-type del)
  0)

(define (api:set-sort-type! del type)
  0)

(define (api:filter-text del)
  "")

(define (api:set-filter-text! del text)
  0)

(define (api:write-delectus del)
  0)

(define (api:read-delectus del)
  $NO-DELECTUS)

(define (api:write-delectus/csv del)
  0)

(define (api:read-delectus/csv del)
  $NO-DELECTUS)

;;; (define $d #f)
;;; (set! $d (api:make-delectus))
;;; $d
;;; (api:add-column! $d "Name")
;;; (api:add-row! $d)
;;; (api:value-at $d "Name" 0)
;;; (api:put-value-at! $d "Name" 0 "Fred")
;;; (api:put-value-at! $d "Shape" 0 "Oblong")
;;; (api:value-at $d "Shape" 0)
;;; (api:value-at $d "Name" 101)
;;; (define $d2 (from-serialized-form (to-serialized-form (find-view $d))))
;;; (api:value-at $d2 "Name" 0)
;;; $d2
