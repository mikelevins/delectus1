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
(in-readtable :delectus)

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
;;;  map utilities
;;; ---------------------------------------------------------------------

(defmethod get-key ((map fset:map) key &optional (default nil))
  (bind ((val found? (fset:lookup map key)))
    (if found?
        val
        default)))

;;; (get-key { :a 1 :b 3} :name 'nope)
;;; (get-key { :a 1 :b 3 :name "Fred"} :name 'nope)


(defun plist->map (plist)
  (fset:convert 'fset:wb-map
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
;;;  time utilities
;;; ---------------------------------------------------------------------


;;; now-timestamp
;;; ---------------------------------------------------------------------

(defun now-timestamp ()
  (local-time:format-timestring nil (local-time:now) :timezone local-time:+utc-zone+))

;;; (now-timestamp)
