#lang sicp

;SICP Chapter 4

;Berkeley Labs
;Lab 12
;1.
;apply
;list-of-values
;eval-if
;eval-sequence
;eval-assignment
;eval-definition

;2.
;eval

;3.
;Because make-procedure gives you an expression that is a procedure.
;This procedure can then be applied by using apply, which will evaluate the operators and operands




;Book exercises
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))


;Ex 4.2
(define (application? exp)
  (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;Ex 4.4
(define (and? exp)
  (tagged-list? exp 'and))
(define (or? exp)
  (tagged-list? exp 'or))
(define (and-expressions exp)
  (cdr exp))
(define (or-expressions exp)
  (cdr exp))


(define (eval-and exp env)
  (define (eval-exps exp)
    (if (true? (eval (first-exp exp) env))
        (eval-exps (rest-exps exp) env)
        false))
  (eval-exps (and-expressions exp)))
(define (eval-or exp env)
  (define (eval-exps exp)
    (if (last-exp? exp) (eval (car exp) env)
        (if (true? (eval (first-exp exp) env))
            true
            (eval-exps (rest-exps exp) env))))
  (eval-exps (or-expressions exp)))


;Implementing 'and' and 'or' as derived expressions

(define (and->if exp)
  (expand-and (cdr exp)))
(define (or->if exp)
  (expand-or (cdr exp)))
(define (expand-and exps)
  (if (last-exp? exps)
      (car exps)
      (make-if
       (first-exp exps)
       (expand-and (rest-exps exps))
       false)))
(define (expand-or exps)
  (if (last-exp? exps)
      (car exps)
      (make-if
       (first-exp exps)
       true
       (expand-or (rest-exps exps)))))

;Ex 4.5
