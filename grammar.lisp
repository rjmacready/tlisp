
(ql:quickload 'cl-lex)
(ql:quickload 'yacc)
(ql:quickload 'parse-number)

(defpackage typelisp
  (:use #:cl #:cl-lex #:yacc)
  (:export #:parse-string))

(in-package :typelisp)

(defun pr-read-line ()
  (read-line *standard-input*))

(cl-lex:define-string-lexer lexer
  ; delimiters
  ("\\(" (return (values :oparen $@)))
  ("\\)" (return (values :cparen $@)))
  ; id
  ("[a-zA-Z][a-zA-Z0-9]*" (return (values :id $@)))
  ; float
  ("[0-9]*\\.[0-9]+" (return (values :float $@)))
  ("([0-9]+)f" (return (values :float $1)))
  ; integer
  ("[0-9]+" (return (values :int $@)))
  ; crush whitespace
  ("\s+" (return (values nil nil)))
  ; quote
  ("'" (return (values :quote :quote)))
  )

(defun open-delimiter-p (c)
  (declare (ignore c))
  nil)

(defun close-delimiter-p (c)
  (declare (ignore c))
  nil)

(defun make-stream-lexer (stream)
  (cl-lex:stream-lexer 
   #'pr-read-line #'lexer #'open-delimiter-p #'close-delimiter-p :stream stream))

;(defun parse-number (a)
;  (print `(:a := ,a))
;  (parse-number:parse-number a))
;(set-dispatch-macro-character #\# #\K (lambda (s c n) (lambda () c)))

(set-dispatch-macro-character #\# #\K (lambda (stream c n) 
					(declare (ignore c))
					(declare (ignore n))
					`(lambda () ,(read stream t nil t))))

(eval-when (:compile-toplevel :execute)

  (defun ast-list (oparen stuff cparen)
    (declare (ignore oparen))
    (declare (ignore cparen))
    `(:list ,@stuff))
  
  (defun flatten-tail (&rest args)
    (setf (cdr args) (cadr args))
    args
    )

  (defun parse-number (nbr)
    `(:number ,(parse-number:parse-number nbr)))
  
  )

(define-parser parser
  (:start-symbol term) ; expression
  (:terminals ( :oparen :cparen :id :float :int :quote ))
  
;  (expression 
;   term)

  (expressions
;   (expression expressions #'flatten-tail)
;   (expression)
   (term expressions #'flatten-tail)
   (term)
   )
  
  (int
   (:int #'parse-number)
   )
  
  (float
   (:float #'parse-number)
  )

  (id
   (:id (lambda (n) `(:id ,n)))
   )
  
  (term 
   float
   int
   id
   ( :oparen expressions :cparen #'ast-list )
   ( :quote term )
   )
  )

(defun parse-string (str)
  "Parse a string, returns a simple AST"
  (let* ((l (lexer str))
	 (ast (parse-with-lexer l parser)))
	 ast))

; ???

(defun parse-ast (ast)
  "One more pass on the ast, so we get a little help with special forms, etc"
  1)

; === types ===
; we need:
; * a list of types
; * a list of checkers / binders / guessers / inducers (...)
; * generics - which need type constructors
; * lazy types? 
; * overloads / overload resolution

(defun type-of (ast)
  "Tries to deduce the type of an ast."
  ast)

(defun type-check (ast)
  "Check if an AST has its types ok"
  (values 'ok ast))
