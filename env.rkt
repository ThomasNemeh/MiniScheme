;Thomas Nemeh Abstractions lab 6. 11/8/2018 
#lang racket

(provide environment? empty-env? extended-env? empty-env extended-env syms vals old-env init-env lookup-env do-define bool? bool-val prim-proc? apply-primitive-op prim-proc-sym
         sym-defined?)

(define empty-env (lambda () (list 'empty-env)))
(define extended-env (lambda (syms vals old-env) (list 'extended-env syms vals old-env)))

(define empty-env? (lambda (x)
      (cond
            [(not (pair? x)) #f]
            [else (eq? (car x) 'empty-env)])))

(define extended-env? (lambda (x)
      (cond
            [(not (pair? x)) #f]
            [else (eq? (car x) 'extended-env)])))

(define environment? (lambda (x) (or (empty-env? x) (extended-env? x))))

(define syms (lambda (env)
     (cond
           [(extended-env? env) (cadr env)]
           [else (error 'syms "bad environment")])))


(define vals (lambda (env)
      (cond
            [(extended-env? env) (caddr env)]
            [else (error 'vals "bad environment")])))


(define old-env (lambda (env)
     (cond
           [(extended-env? env) (cadddr env)]
           [else (error 'old-env "bad environment")])))

(define lookup-helper
  (lambda (syms vals x)
    (cond
      [(null? syms) 'minischemeforbiden]
      [(equal? (car syms) x) (car vals)]
      [else (lookup-helper (cdr syms) (cdr vals) x)])))

;lookup value of symbol in environment
(define lookup-env
  (lambda (env x)
    (cond
      [(empty-env? env) (error 'lookup-env "symbol not in environment: ~s" x)]
      [(not (eq? (lookup-helper (syms env) (vals env) x) 'minischemeforbiden)) (lookup-helper (syms env) (vals env) x)]
      [else (lookup-env (old-env env) x)])))

(define new-sym-helper
  (lambda (x vals)
    (cond
      [(null? vals) #t]
      [(eq? (car vals) x) #f]
      [else (new-sym-helper x (cdr vals))])))

(define sym-defined?
  (lambda (x env)
    (new-sym-helper x (vals env))))

;update environment when executing define expression
(define do-define (lambda (sym val)
  (set! init-env (extended-env (list sym)(list (box val)) init-env))));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CHANGE HERE

;(define the-empty-env (empty-env))
;(define EnvA (extended-env '(x y) '(1 2) the-empty-env))
;(define EnvB (extended-env '(x z) '(5 7) EnvA))

;constructor for primitive procedure
(define new-prim-proc
  (lambda (x)
    (list 'prim-proc x)))

;recognizer for primitive procedure
(define prim-proc?
  (lambda (x)
    (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'prim-proc)])))

;get symbol of primitive procedure
(define prim-proc-sym
  (lambda (x)
    (cond
      [(cadr x)])))

;defines execution of each procedure
(define apply-primitive-op
  (lambda (op arg-values)
    (cond
      [(eq? op '+) (+ (car arg-values) (cadr arg-values))]
      [(eq? op '-) (- (car arg-values) (cadr arg-values))] 
      [(eq? op '*) (* (car arg-values) (cadr arg-values))]
      [(eq? op '/) (/ (car arg-values) (cadr arg-values))]
      [(eq? op 'add1) (add1 (car arg-values))]
      [(eq? op 'sub1) (sub1 (car arg-values))]
      [(eq? op 'minus) (* -1 (car arg-values))]
      [(eq? op 'list) (apply list arg-values)]
      [(eq? op 'build) (cons (car arg-values) (cadr arg-values))]
      [(eq? op 'first) (car (car arg-values))]
      [(eq? op 'rest) (cdr (car arg-values))]
      [(eq? op 'empty?) (or (null? (car arg-values)) (null? arg-values))]
      [(eq? op 'equals?) (if (eqv? (car arg-values) (cadr arg-values)) 'True 'False)]
      [(eq? op 'lt?) (if (< (car arg-values) (cadr arg-values)) 'True 'False)]
      [(eq? op 'gt?) (if (> (car arg-values) (cadr arg-values)) 'True 'False)])))

;list of primitive procedures
(define primitive-operators '(+ - * / add1 sub1 minus list build first rest empty? equals? lt? gt?))

;list of initial environment
(define init-env (extended-env '(nil True False) (map box '(() (bool True) (bool False))) (extended-env primitive-operators
 (map box (map new-prim-proc primitive-operators)) (empty-env))))

;recognizer for bool type
(define bool?
  (lambda (x)
   (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'bool)])))

;getter of bool type
(define bool-val
  (lambda (x)
    (cadr x)))

























    










