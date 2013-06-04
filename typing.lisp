
(defpackage typelisp.types
  (:use #:cl)
  (:export #:type-of-ast))

(in-package :typelisp.types)

;(defun nbr-bits-for-int (int)
;  (if (= int 0)
;      1
;      (+ 1 (truncate (log int 2)))))

(defun type-of-ast (ast &optional (env nil) )
  (case (car ast)
    (:int 
     (print "is an int"))
    (:float 
     (print "is a float"))
    (:list (progn
	     (print "is a list of something ...")))
    (t (error "unexpected ast"))))