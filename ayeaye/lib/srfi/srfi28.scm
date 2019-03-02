;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          srfi28.scm
;;;; Project:       Delectus
;;;; Purpose:       selected functions from SRFI28: string-formatting
;;;; Author:        various (as noted)
;;;;                selected by mikel evins
;;;; ***********************************************************************

;;; FROM:
;;; (author: "Scott G. Miller <scgmille at freenetproject.org>")
;;; (maintainer: "Jeremie Lasalle Ratelle <pouexmachinax at gmail.com>")
;;; (description: "Format strings")
;;; (keywords: srfi)
;;; (homepage: "http://srfi.schemers.org/srfi-28")

(define format
  (lambda (format-string . objects)
    (let ((buffer (open-output-string)))
      (let loop ((format-list (string->list format-string))
                 (objects objects))
        (cond ((null? format-list) (get-output-string buffer))
              ((char=? (car format-list) #\~)
               (if (null? (cdr format-list))
                   (error 'format "Incomplete escape sequence")
                   (case (cadr format-list)
                     ((#\a)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (display (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
	             ((#\s)
                      (if (null? objects)
                          (error 'format "No value for escape sequence")
                          (begin
                            (write (car objects) buffer)
                            (loop (cddr format-list) (cdr objects)))))
                     ((#\%)
                      (newline buffer)
                      (loop (cddr format-list) objects))
                     ((#\~)
                      (write-char #\~ buffer)
                      (loop (cddr format-list) objects))
                     (else
                      (error 'format "Unrecognized escape sequence")))))
              (else (write-char (car format-list) buffer)
                    (loop (cdr format-list) objects)))))))
