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

(defmethod path ((path string))
  (pathname (uiop:native-namestring path)))

;;; ---------------------------------------------------------------------
;;;  list utilities
;;; ---------------------------------------------------------------------

(defun alist->plist (alist)
  (loop for pair in alist
     appending (list (car pair)(cdr pair))))

(defun any (seq)
  (elt seq (random (length seq))))

;;; (wb-map->plist {:|a| 1 :|b| 2 :|c| {:d 4 :e 5}})

(defmethod drop ((count integer)(ls list))
  (subseq ls count))

(defun remove-list-elements (remove-list from-list &key (test #'eql))
  (if (null remove-list)
      from-list
      (remove-list-elements (cdr remove-list)
                            (remove (first remove-list)
                                    from-list
                                    :test test))))


(defmethod take ((count integer)(s sequence))
  (subseq s 0 count))

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

;;; for plists
(defmethod get-keys ((map list))
  (loop for tail on map by (lambda (ls)(cddr ls)) collect (first tail)))

;;; (get-keys [:a 1 :b 3 :name "Fred"])


(defmethod get-values ((map fset:map))
  (fset:convert 'list (fset:range map)))

;;; (get-values { :a 1 :b 3 :name "Fred"})

;;; for plists
(defmethod get-values ((map list))
  (loop for tail on map by (lambda (ls)(cddr ls)) collect (second tail)))

;;; (get-values [:a 1 :b 3 :name "Fred"])

(defmethod merge-maps ((left-map wb-map) (right-map wb-map))
  (fset:map-union left-map right-map))

(defun plist->map (plist)
  (convert 'wb-map
           (loop for tail on plist by #'cddr
              collect (cons (first tail)
                            (second tail)))))

(defun wb-map->plist (wb-map)
  (let ((alist (fset:convert 'list wb-map)))
    (loop for (k . v) in alist
       appending (list k
                       (if (typep v 'wb-map)
                           (wb-map->plist v)
                           v)))))

;;; ---------------------------------------------------------------------
;;;  sequence utilities
;;; ---------------------------------------------------------------------

(defmethod empty? ((s null)) t)
(defmethod empty? ((s list)) nil)
(defmethod empty? ((s sequence)) (<= (length s) 0))
(defmethod empty? ((m wb-map)) (fset:empty? m))

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
;;;  system utilities
;;; ---------------------------------------------------------------------

(defun getpid ()
  #+lispworks (system::getpid)
  #+sbcl (sb-posix:getpid)
  )

;;; computes the size of the interval between representable double
;;; floats in the range 2^x and 2^y
(defun double-float-interval-size (x y)
  (/ (- (expt 2.0 y)
        (expt 2.0 x))
     (expt 2.0 53)))
