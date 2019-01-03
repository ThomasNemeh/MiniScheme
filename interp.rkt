;Thomas Nemeh. Abstractions Lab 6. 11/15/2018
#lang racket

(provide eval-exp)

(require "env.rkt")
(require "parseH.rkt")

;interpreter for parsed user input
(define eval-exp 
  (lambda (tree env)
    (cond
      [(lit-exp? tree) (LitValue tree)]
      [(var-ref? tree) (unbox (lookup-env env (Symbol tree)))];;;;;;;;;;;;;;;CHANGE HERE
      [(define-exp? tree) (let ([sym (define-exp-sym tree)]
                                [val (eval-exp (define-exp-exp tree) env)])
                                  (do-define sym val))]
      [(app-exp? tree) (let ([p (eval-exp (app-exp-proc tree) env)]
                             [args (eval-args (app-exp-args tree) env)])
                               (apply-proc p args))]
      [(if-exp? tree) (let ([cond-exp (eval-exp (if-exp-cond tree) env)])
                              (eval-if-exp cond-exp (if-exp-body1 tree) (if-exp-body2 tree) env))]
      [(let-exp? tree) (let ([new-env (extended-env (let-exp-syms tree) (map box (map (lambda (x) (eval-exp x env)) (let-exp-vals tree))) env)])
                             (eval-exp (let-exp-body tree) new-env))]
      [(lambda-exp? tree) (new-closure tree env)]
      [(assign-exp? tree) (set-box! (lookup-env env (assign-exp-sym tree)) (eval-exp (assign-exp-exp tree) env))]
      [(begin-exp? tree) (eval-begin-exp (begin-exp-lat tree)  env)]
      [else (error 'eval-exp  "Invalid tree: ~s" tree)])))

;evaluate arguments in application tree
(define eval-args
  (lambda (lat env)
    (cond
      [(null? lat) null]
      [else (cons (eval-exp (car lat) env) (eval-args (cdr lat) env))])))

;evulate if expression given value of conditional
(define eval-if-exp
  (lambda (cond-exp body1 body2 env)
    (cond
      [(number? cond-exp) (if (= 0 cond-exp) (eval-exp body2 env) (eval-exp body1 env))]
      [(bool? (unbox (lookup-env env cond-exp))) (if (eq? 'False cond-exp) (eval-exp body2 env) (eval-exp body1 env))]
      [else (error 'eval-if-exp "Invalid input: ~s" cond-exp)])))

;contructor for closure
(define new-closure
  (lambda (tree env)
    (list 'closure (cadr tree) (caddr tree) env)))

;identifier for a closure
(define closure?
  (lambda (x)
    (cond
      [(not (pair? x)) #f]
      [else (eq? (car x) 'closure)])))

;getters for closure
(define closure-params
  (lambda (x)
    (cadr x)))

(define closure-body
  (lambda (x)
    (caddr x)))

(define closure-env
  (lambda (x)
    (cadddr x)))

;execute procedure if it is valid
(define apply-proc
  (lambda (p arg-values)
    (cond
      [(prim-proc? p) (apply-primitive-op (prim-proc-sym p) arg-values)]
      [(and (closure? p) (= (length (closure-params p)) (length arg-values))) (eval-exp (closure-body p) (extended-env (closure-params p) (map box arg-values) (closure-env p)))];;CHANGE HERE 
      [else (error 'apply-proc "Bad procedure: ~s" p)])))

;evaluate a begin expression
(define eval-begin-exp
  (lambda (lat env)
    (cond
      [(null? (cdr lat)) (eval-exp (car lat) env)]
      [(or (assign-exp? (car lat)) (define-exp? (car lat))) (eval-exp (car lat) env) (eval-begin-exp (cdr lat) env)]
      [else (eval-begin-exp (cdr lat) env)])))

















