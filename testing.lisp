;;; testing code

(in-package :delectus)

;;; ---------------------------------------------------------------------
;;; reading csv files
;;; ---------------------------------------------------------------------

(time
 (progn
   (setf $zips (read-csv (namestring (cl-user::path "test-data/zipcode.csv"))))
   'done))

(columns $zips)
(time (value-at $zips "city" 0))
(time (length (rows $zips)))
(time (max-item-length (rows $zips) 1))

;;; ---------------------------------------------------------------------
;;; creating documents
;;; ---------------------------------------------------------------------

(time 
 (progn
     (setf $doc
      (make-instance 'document
                     :presentation (make-instance 'presentation :model $zips)))))



(setq $list-pane (first (layout-description (contents (contents-layout (window $doc))))))
(setq $cont (contents-layout (window $doc)))

(apply-in-pane-process $list-pane 'capi:scroll $list-pane :pan :move (list 0 100))
(get-scroll-position $list-pane :vertical)
(simple-pane-vertical-scroll $list-pane)

;;; ---------------------------------------------------------------------
;;; presentations
;;; ---------------------------------------------------------------------

(setq $pres (make-instance 'presentation :model $zips))

(time
 (progn
   (store $pres "/tmp/zips.delectus")
   'done))

(time
 (progn
   (setf $repres (reload-presentation "/tmp/zips.delectus"))
   'done))

(columns $repres)
(type-of (columns $repres))
(value-at $repres "city" 0)
(value-at $repres "city" 43190)
(elt (rows $repres) 100)


