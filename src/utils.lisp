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
