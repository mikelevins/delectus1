;;; identity.lisp

(in-package :engine)

;;; generate identities for Delectus' identifiable objects. An
;;; identity is approximately a v1 UUID, but rendered as an unbroken
;;; hexidecimal string (e.g. "DEAAC53831A91E1E83E30FD2EF385CFF"). The
;;; node field is always random, to avoid identifying the originating
;;; device. The high 28 bits of the time are also random.

(defparameter *identity-random-state* nil)

(defun make-identity ()
  (unless *identity-random-state*
    (setf *identity-random-state* (make-random-state t)))
  (let* ((now (get-universal-time))
         (clock-seq (random #x1fff *identity-random-state*))
         (node (random #xffffffffffff *identity-random-state*))
         (time-low now)
         (time-mid (random #xffff *identity-random-state*))
         (time-high (random #xfff *identity-random-state*))
         (time-high+version (dpb #b0001 (byte 4 12) time-high))
         (clock-seq-var (dpb #b10 (byte 2 6) (ldb (byte 6 8) clock-seq)))
         (clock-seq-low (ldb (byte 8 0) clock-seq)))
    (with-output-to-string (out)
      (format out "~8,'0x" time-low)
      (format out "~4,'0x" time-mid)
      (format out "~4,'0x" time-high+version)
      (format out "~2,'0x" clock-seq-var)
      (format out "~2,'0x" clock-seq-low)
      (format out "~12,'0x" node)
      out)))


