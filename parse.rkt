;Thomas Nemeh. Abstractions Lab 6. 11/15/2018
#lang racket
;Should I check that val in new-lit-exp is a number?
(provide parse new-lit-exp lit-exp? LitValue new-var-ref var-ref? Symbol new-define-exp define-exp? define-exp-sym define-exp-exp app-exp? app-exp-proc app-exp-args if-exp?
         if-exp-cond if-exp-body1 if-exp-body2 let-exp? let-exp-syms let-exp-vals let-exp-body lambda-exp? assign-exp? assign-exp-sym assign-exp-exp begin-exp? begin-exp-lat)
;returns true if input is an atom, false otherwise
(define atom?
  (lambda (x)
    (cond
      ((list? x) #f)
      (else #t))))
;counts number of elements in a list
(define count
  (lambda (lat)
    (foldr (lambda (head tail) (+ 1 tail)) 0 lat)))

;parse user input
(define parse
  (lambda (input)
    (cond
      [(not (pair? input)) (expression input)]
      [(eq? (car input) 'define) (if (= (count input) 3) (new-define-exp (cadr input) (parse (caddr input))) (error 'parse "Invalid input ~s" input))]
      [(eq? (car input) 'if)  (if (= (count input) 4) (new-if-exp (cadr input) (caddr input) (cadddr input)) (error 'parse "Invalid input ~s" input))]
      [(eq? (car input) 'let)  (if (= (count input) 3) (new-let-exp (map car (cadr input)) (map cadr (cadr input)) (caddr input)) (error 'parse "Invalid input ~s" input))]
      [(eq? (car input) 'lambda) (if (= (count input) 3) (new-lambda-exp (cadr input) (caddr input)) (error 'parse "Invalid input ~s" input))]
      [(eq? (car input) 'set!) (if (= (count input) 3) (new-assign-exp (cadr input) (caddr input)) (error 'parse "Invalid input ~s" input))]
      [(eq? (car input) 'letrec)  (if (= (count input) 3) (make-letrec (map car (cadr input)) (map cadr (cadr input)) (caddr input)) (error 'parse "Invalid input ~s" input))]
      [(eq? (car input) 'begin) (if (> (count input) 1) (new-begin-exp (cdr input)) (error 'parse "Invalid input ~s" input))]
      [else (new-app-exp input)])))

;parses variables and numbers 
(define expression
  (lambda (input)
    (cond
      [(null? input) (new-var-ref 'nil)]
      [(number? input) (new-lit-exp input)]
      [(and (not (number? input)) (atom? input) (new-var-ref input))]
      [else (error 'expression "Invalid syntax ~s" input)])))

;constructor for exp tree
(define new-lit-exp
 (lambda (val)
   (cond
     [(list 'lit-exp val)])))

;recognizer for exp tree
(define lit-exp?
  (lambda (x)
    (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'lit-exp)])))

;getter for exp tree
(define LitValue
  (lambda (x)
    (cond
      [(lit-exp? x) (cadr x)])))

;constructors for variable tree
(define new-var-ref
  (lambda (x)
    (list 'var-ref x)))

;getter for variable tree
(define var-ref?
  (lambda (x)
    (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'var-ref)])))

;getter for variable tree
(define Symbol
  (lambda (x)
    (cond
      [(var-ref? x) (cadr x)])))

;constructor for define tree
(define new-define-exp
  (lambda (var exp)
    (list 'define-exp var exp)))

;recognizer for define trees
(define define-exp?
  (lambda (x)
    (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'define-exp)])))

;get symbol of define expression
(define define-exp-sym
  (lambda (x)
    (cadr x)))

;get expression of define expression
(define define-exp-exp
  (lambda (x)
    (caddr x)))

;constructor for application trees
(define new-app-exp
  (lambda (x)
    (list 'app-exp (parse (car x)) (map parse (cdr x)))))

;recognizer for application trees
(define app-exp?
  (lambda (x)
    (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'app-exp)])))

;get procedure for application trees
(define app-exp-proc
  (lambda (x)
    (cadr x)))

;get arguments for application trees
(define app-exp-args
  (lambda (x)
    (caddr x)))

;constructor for if expression
(define new-if-exp
  (lambda (exp1 exp2 exp3)
    (list 'if-exp (parse exp1) (parse exp2) (parse exp3))))

;recognizer for if expression
(define if-exp?
  (lambda (x)
    (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'if-exp)])))

;get conditional expression for if expressions
(define if-exp-cond
  (lambda (x)
    (cadr x)))

;get expression which is evaluated if conditional is true
(define if-exp-body1
  (lambda (x)
    (caddr x)))

;get expression which is evaluated if conditional is false
(define if-exp-body2
  (lambda (x)
    (cadddr x)))

;constructor for let expression tree
(define new-let-exp
  (lambda (syms vals body)
    (list 'let-exp syms (map parse vals) (parse body))))

(define new-let-exp-2
  (lambda (syms vals body)
    (list 'let-exp syms (map parse vals) body)))

;recognizer for let expressions
(define let-exp?
  (lambda (x)
    (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'let-exp)])))

;get symbols provided to let expression
(define let-exp-syms
  (lambda (x)
    (cadr x)))

;get values provided to let expression
(define let-exp-vals
  (lambda (x)
    (caddr x)))

;get value of let expression
(define let-exp-body
  (lambda (x)
    (cadddr x)))

;lambda-exp constructor
(define new-lambda-exp
  (lambda (params body)
    (list 'lambda-exp params (parse body))))

;lambda-exp identifier
(define lambda-exp?
 (lambda (x)
   (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'lambda-exp)])))

;assign exp constructor
(define new-assign-exp
  (lambda (sym exp)
     (list 'assign-exp sym (parse exp))))

;assign-exp identifier
(define assign-exp?
  (lambda (x)
     (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'assign-exp)])))

;assign-exp getters
(define assign-exp-sym
  (lambda (x)
    (cadr x)))

(define assign-exp-exp
  (lambda (x)
    (caddr x)))

;begin exp constructor
(define new-begin-exp
  (lambda (arg-list)
    (list 'begin-exp (map parse arg-list))))

;begin exp identifier
(define begin-exp?
  (lambda (x)
    (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'begin-exp)])))

;begin-exp getter
(define begin-exp-lat
  (lambda (x)
    (cadr x)))

;(define placeholder (gensym))
;
;(define make-letrec
 ; (lambda (syms vals outer-body)
  ;   (list 'let-exp (list placeholder) (list (parse 0)) (new-let-exp syms vals (make-inner-body placeholder outer-body)))))


;(define make-inner-body
 ; (lambda (placeholder outer-body)
  ;  (list 'begin (list 'set! placeholder (car outer-body)) (list placeholder (cadr outer-body)))))

;transform letrec expression into appropriate let expression
(define make-letrec
  (lambda (syms vals outer-body)
    (begin (define placeholders (make-placeholders syms null))
         (list 'let-exp syms (map parse (make-zeros syms null)) (new-let-exp placeholders vals (make-inner-body placeholders syms outer-body))))))

;make inside body of transformation of letrec expression
(define make-inner-body
  (lambda (placeholders syms outer-body)
    (list 'begin (list 'set! (car outer-body) (lookup placeholders syms (car outer-body))) (list (car outer-body) (cadr outer-body)))))

;make list of placeholders for letrec expression
(define make-placeholders
  (lambda (syms placeholders)
    (cond
      [(null? syms) placeholders]
      [else (make-placeholders (cdr syms) (cons (gensym) placeholders))])))

;Get list of zeros of size equivalent to list of symbols in letrec expression
(define make-zeros
  (lambda (syms zeros)
    (cond
      [(null? syms) zeros]
      [else (make-zeros (cdr syms) (cons 0 zeros))])))

;get placeholder corresponding to a symbol in letrec expression
(define lookup
  (lambda (placeholders syms target)
    (cond
      [(null? placeholders) null]
      [(null? syms) null]
      [(equal? (car syms) target) (car placeholders)]
      [else (lookup (cdr placeholders) (cdr syms) target)])))






 