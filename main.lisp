
(ql:quickload 'llvm)

(defpackage fabteste
  (:use #:cl)
  (:export #:main))

(in-package :fabteste)

(defun main () 
  (llvm:with-objects ((*builder* llvm:builder)
		      (*module* llvm:module "test"))
    (print 'hey)))