;;;; ***********************************************************************
;;;;
;;;; Name:          to-sql.lisp
;;;; Project:       delectus 2
;;;; Purpose:       convert sql s-expression to SQL statements
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:delectus)
(in-readtable :sql)

(DEFUN >> (SQL-SEXPR) 
  (COND
    ((NULL SQL-SEXPR) "NULL")
    ((MEMBER SQL-SEXPR '(:FALSE :false)) 0)
    ((MEMBER SQL-SEXPR '(:TRUE :true)) 1)
    ((KEYWORDP SQL-SEXPR) (STRING-UPCASE (SYMBOL-NAME SQL-SEXPR)))
    ((SYMBOLP SQL-SEXPR) (SYMBOL-NAME SQL-SEXPR))
    ((STRINGP SQL-SEXPR) (FORMAT NIL "'~A'" SQL-SEXPR))
    ((NUMBERP SQL-SEXPR) (FORMAT NIL "~A" SQL-SEXPR))
    (T (ERROR "Unrecognized SQL operator: ~S" SQL-SEXPR))))

;;; (>> NIL)
;;; (>> :false)
;;; (>> :TRUE)
;;; (>> :SELECT)
;;; (>> `Delectus)
;;; (>> "Name")
;;; (>> 10.0)
