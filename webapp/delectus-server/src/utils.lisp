;;;; utils.lisp

(in-package #:delectus-server)

;;; (range start end &key by) => sequence
;;; ---------------------------------------------------------------------

(defmethod range ((start integer) (end integer) &key (by 1))
  (let ((step by))
    (if (plusp (- end start))
        (loop for i from start below end by step collect i)
        (loop for i downfrom start above end by step collect i))))

;;; (take2 i sequence) => sequence'
;;; ---------------------------------------------------------------------

(defmethod take2 ((offset integer)(sequence cl:sequence))
  (cl:subseq sequence offset (cl:min (+ 2 offset) (length sequence))))

;;; (string->hex s) => hex-string
;;; ---------------------------------------------------------------------

(defmethod string->hex ((s string))
  (with-output-to-string (out)
    (dotimes (i (length s))
      (let ((ch (elt s i)))
        (format out "~2,x" (char-code ch))))))

;;; (hex->string x) => string
;;; ---------------------------------------------------------------------

(defmethod hex->string ((x string))
  (let* ((len (length x))
         (indexes (range 0 len :by 2))
         (hexpairs (mapcar (lambda (i)(take2 i x))
                           indexes))
         (codes (mapcar (lambda (hp)(parse-integer hp :radix 16))
                        hexpairs))
         (chars (mapcar (lambda (code)(code-char code))
                        codes)))
    (coerce chars 'string)))

