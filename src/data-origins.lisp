;;;; ***********************************************************************
;;;;
;;;; Name:          data-origins.lisp
;;;; Project:       delectus 2
;;;; Purpose:       creating identifiers for ops
;;;; Author:        mikel evins
;;;; Copyright:     2010-2020 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; An origin is a 64-bit signed integer that identifies where an op
;;; came from. We compute one each time Delectus opens a file for
;;; writing.
;;;
;;; We compute an origin by concatenating the delectus node
;;; identity with delectus pid and the pathname of the Delectus file
;;; where it is to be used, hashing that string with SHA256, and then
;;; taking the first 64 bits of the result.
;;;
;;; The odds of computing a collision in this way can be
;;; estimated as about k^2/2*N, where k is the number of 
;;; hashes we expect to compute, and N is the number of
;;; different possible hash values.
;;; see:
;;;   https://preshing.com/20110504/hash-collision-probabilities/
;;; for details.

(defun odds-of-collision (number-of-possible-hash-values number-generated)
  (float (/ (* number-generated number-generated)
            (* 2 number-of-possible-hash-values))))

;;; suppose we use for origins a cryptographic hash truncated to 64 bits.
;;; let's assume a Delectus user opens 100 Delectus files a day for 50 years.
;;; that's (* 100 365 50) = 1,825,000 origins we have to compute.
;;; the odds of ever seeing the same one more than once are
;;; (odds-of-collision $64bits 1825000) => 9.027677E-8 = 1 in 11,077,047
;;; much less likely than being struck by lighting; about as likely as
;;; winning a 6/49 lottery.
;;;
;;; Note that origins are computed per file, and two or more origins
;;; are only used in the same file if 1) two files contain copies of
;;; the same list and 2) we merge ops from one of them into the other.
;;; The above odds are computed as if all the origins we ever
;;; compute appear in the same file, so in a real-world scenario,
;;; the likelihood of a collision between origins is much much smaller.

(defmethod make-origin-string ((nodeid vector)(pid integer)(list-file pathname))
  (assert (uiop/pathname:absolute-pathname-p list-file)()
          "Expected a full pathname; found ~S" list-file)
  (let ((nodeid (identity->string nodeid))
        (pid (format nil "~D" pid)))
    (concatenate 'string
                 nodeid
                 ":"
                 pid
                 ":"
                 (namestring list-file))))

;;; (make-origin-string (delectus-node-identity) (osicat-posix:getpid) (pathname "/Users/mikel/.emacs"))

(defmethod make-origin ((nodeid vector)(pid integer)(list-file pathname))
  (intbytes:octets->int64
   ;; hash the origin string, then take the first 8 bytes
   (subseq (ironclad:digest-sequence :sha256
                                     (babel:string-to-octets (make-origin-string nodeid pid list-file)))
           0 8)))

;;; (make-origin (delectus-node-identity) (osicat-posix:getpid)(pathname "/Users/mikel/.emacs"))
