;;;; ***********************************************************************
;;;;
;;;; Name:          system-utils.lisp
;;;; Project:       delectus 2
;;;; Purpose:       general-purpose utility functions
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)

;;; ---------------------------------------------------------------------
;;;  file utilities
;;; ---------------------------------------------------------------------

;;;  file-pathname-p (pathname)
;;; ---------------------------------------------------------------------
;;; returns true if the pathname's name or type part is nonempty
;;; does not check whether the named file actually exists

(defun file-pathname-p (pathname)
  (when pathname
    (let* ((pathname (pathname pathname))
           (name (pathname-name pathname))
           (type (pathname-type pathname)))
      (when (or (not (member name '(nil :unspecific "") :test 'equal))
                (not (member type '(nil :unspecific "") :test 'equal)))
        pathname))))


;;; ---------------------------------------------------------------------
;;;  list utilities
;;; ---------------------------------------------------------------------

(defun alist->plist (alist)
  (loop for pair in alist
     appending (list (car pair)(cdr pair))))

(defun wb-map->plist (wb-map)
  (let ((alist (fset:convert 'list wb-map)))
    (loop for (k . v) in alist
       appending (list k
                       (if (typep v 'wb-map)
                           (wb-map->plist v)
                           v)))))

;;; (wb-map->plist {:|a| 1 :|b| 2 :|c| {:d 4 :e 5}})

(defun remove-list-elements (remove-list from-list &key (test #'eql))
  (if (null remove-list)
      from-list
      (remove-list-elements (cdr remove-list)
                            (remove (first remove-list)
                                    from-list
                                    :test test))))

;;; ---------------------------------------------------------------------
;;;  map utilities
;;; ---------------------------------------------------------------------

(defmethod get-key ((map fset:map) key &optional (default nil))
  (bind ((val found? (lookup map key)))
    (if found?
        val
        default)))

;;; (get-key { :a 1 :b 3} :name 'nope)
;;; (get-key { :a 1 :b 3 :name "Fred"} :name 'nope)


(defmethod get-keys ((map fset:map))
  (fset:convert 'list (fset:domain map)))

;;; (get-keys { :a 1 :b 3 :name "Fred"})


(defmethod get-values ((map fset:map))
  (fset:convert 'list (fset:range map)))

;;; (get-values { :a 1 :b 3 :name "Fred"})

(defmethod merge-maps ((left-map wb-map) (right-map wb-map))
  )

(defun plist->map (plist)
  (convert 'wb-map
           (loop for tail on plist by #'cddr
              collect (cons (first tail)
                            (second tail)))))

;;; ---------------------------------------------------------------------
;;;  string utilities
;;; ---------------------------------------------------------------------

(defun join-strings (cupola strings)
  (cond ((null strings) nil)
        ((null (rest strings)) (first strings))
        (t (reduce (lambda (left right)(concatenate 'string left cupola right))
                   strings))))

;;; (join-strings ", " (list "apple" "banana" "cherry"))
;;; (join-strings "" (list "apple" "banana" "cherry"))

(defun str (&rest vals)
  (join-strings ""
                (mapcar (lambda (v)(format nil "~A" v))
                        vals)))

;;; (str 1 2 3)
;;; (str "1" "2" "3")
;;; (str '("CREATE TABLE " "`list_data` " "(`optype` TEXT, `opid` TEXT, `origin` TEXT, `revision` INTEGER, `timestamp` TEXT, `item` TEXT, `name` TEXT, `deleted` TEXT, `peer` TEXT);"))

(defun trim (s)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) s))


;;; ---------------------------------------------------------------------
;;;  symbol utilities
;;; ---------------------------------------------------------------------

(defmethod as-keyword ((s string))
  (intern s :keyword))

(defmethod as-keyword ((s symbol))
  (intern (symbol-name s) :keyword))

(defmethod as-string ((s symbol))
  (symbol-name s))

;;; ---------------------------------------------------------------------
;;;  time utilities
;;; ---------------------------------------------------------------------


;;; now-timestamp
;;; ---------------------------------------------------------------------

(defun now-timestamp ()
  (local-time:format-timestring nil (local-time:now) :timezone local-time:+utc-zone+))

;;; (now-timestamp)
