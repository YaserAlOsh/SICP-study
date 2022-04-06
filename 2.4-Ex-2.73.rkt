#lang sicp
(#%require racket/base)
;Section 2.4: Multiple representation for abstract data

;In this chapter, we made a complex numbers arithmatic system that can be used with more than one representation
;We illustrated it using two representations for complex numbers: polar and rectangular forms
;We made generic procedures that take an argument, and applies the procedure that is specific to the type of the arugment
;We also made the system usable with procedures that take more than one argument,
; and procedures that apply to two or more representations

;Ex 2.73:
;Temp get definition
(define *the-table* (make-hash));make THE table 
 (define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
 (define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get 




(define (variable? exp) (symbol? exp))
(define (same-variable? exp var) (and (symbol? exp) (equal? exp var)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else (let ((operation-info (get-operation-info exp)))

               ((get 'deriv (car operation-info))
               (cdr operation-info) var)))))

;This procedure gets the suitable procedure for each type of expression from a table
;It then applies the operands (addend and augend, multiplier and multiplicind, etc..) to that procedure
;We cannot move the number? and variable? expressions type because those don't have an operator,
; and we need an operator to get the suitable procedure to apply

(define (operator exp) (cadr exp))
(define (operands exp) (cons (car exp) (cddr exp)))
(define (multi-operands exp) (not (null? (cdddr exp))))


;(define (operation expr) 
;  (define (check-precedence expr op)
;     (cond  ((not (pair? expr)) (cons #f '()))
;            ((not (pair? (cdddr expr))) (cons #f '()))
;            ((< (operation-order (operator (cddr expr))) (operation-order op))
;             (cons #t (operator (cddr expr))));cddr gives the second expression (after the first operation)
            ;((pair? (cddr expr)(takes
;            (else (check-precedence (cddr expr) op))))
;  (let ((res (check-precedence expr (operator expr))))
;      (if (car res)
;          (cdr res)
;          (operator expr))))

(define (operation-order op)
  (cond ((equal? op '+) 1)
        ((equal? op '-) 1)
        ((equal? op '*) 2)
        ((equal? op '/) 2)
        (else 0)))

(define (get-operation-info expr)
  (define (check-precedence prev-expr rem-expr op)
     (cond  ((not (pair? rem-expr))
             (cons op (operands expr)))
            ((not (pair? (cdddr rem-expr))) (cons op (operands expr)))
            ((< (operation-order (operator (cddr rem-expr))) (operation-order op))
             ;(cons #t
                   (cons (operator (cddr rem-expr))
                        (cons (list prev-expr (operator rem-expr) (car (operands (cddr rem-expr))))
                              (cdr (operands (cddr rem-expr))))))
            ;);cddr gives the second expression (after the first operation)
            ;((pair? (cddr expr)(takes
            (else (check-precedence (cons prev-expr (car rem-expr))
                                    (cddr rem-expr) op))))
  (check-precedence (car (operands expr)) expr (operator expr)))
  ;(let ((res (check-precedence (car (operands expr)) expr (operator expr))))
   ;   (if (car res)
    ;      (cdr res)
     ;     (operator expr))))
  



(define (=number? x n) (and (number? x) (eq? x n)))
(define (make-op op o1 o2 )
  (list o1 op o2))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (make-op '+ a1 a2))))

(define (make-diff a1 a2)
    (cond ((=number? a1 0) (if (number? a2) (- a2) (list '- a2)))
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (- a1 a2))
          (else (make-op '- a1 a2))))

(define (make-product m1 m2)
    (cond ((=number? m1 0) 0)
          ((=number? m2 0) 0)
          ((=number? m2 1) m1)
          ((=number? m1 1) m2)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (make-op '* m1 m2))))

(define (make-quotient n d)
    (cond ((=number? n 0) 0)
          ((=number? d 0) (error "divide by zero" ))
          ((=number? d 1) n)
          ((and (number? n) (number? d)) (/ n d))
          (else (make-op '/ n d))))

(define (make-exp b n)
  (cond ((=number? n 0) 1)
        ((=number? b 0) (error "exp of zero" ))
        ((=number? b 1) 1)
        ((=number? n 1) b)
        ((and (number? n) (number? b)) (exp n b))
        (else (make-op '^ n b))))

(define (square x) (* x x))
(define (exp b n);This uses a successive squaring algorithm
  (define (iter b n r)
    (cond ((= n 0) r)
          ((= (remainder n 2) 0) (iter (square b) (/ n 2) r))
          (else  (iter b (- n 1) (* b r)))))
  (iter b n 1))



(define (install-sum-package)
  ;internal procedures
  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (define (diff-deriv exp var)
    (make-diff (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  (define (addend exp) (car exp))
  (define (augend exp) (let ((n-op (cdr exp)));This checks if there are more than one operands in the augend
                         (if (null? (cdr n-op ))
                             (car n-op)
                             n-op)))
                             ;(list (car n-op) '+ (cdr n-op)))))

  ;interface to the rest of the system
  (put 'deriv '+ sum-deriv)
  (put 'deriv '- diff-deriv)
  'done)

(define (install-product-package)
  (define (prod-deriv exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (multiplicand exp)
                            (deriv (multiplier exp) var))))
  (define (multiplier exp) (car exp))
  (define (multiplicand exp) (let ((n-op (cdr exp)));This checks if there are more than one operands in the multiplicand
                               (if (null? (cdr n-op ))
                                   (car n-op)
                                   n-op)))
                                   ;(list (car n-op) '* (cdr n-op)))))

  ;Interface to the rest of the system
  (put 'deriv '* prod-deriv)
  'done)

(define (install-quotient-package)
  (define (quotient-deriv exp var)
    (make-quotient
     (make-diff (make-product (deriv (multiplier exp) var)
                              (multiplicand exp))    
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var)))
     (make-exp (multiplicand exp) 2)))
  (define (multiplier exp) (car exp))
  (define (multiplicand exp) (cadr exp))
  
  ;Interface to the rest of the system
  (put 'deriv '/ quotient-deriv)
  'done)

(define (install-exp-package)
  (define (exp-deriv exp var)
    (make-product
     (make-product (exponent exp)
                  (make-exp (base exp) (- (exponent exp) 1)))
     (deriv (base exp) var)
     ))
     
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  
  ;Interface to the rest of the system
  (put 'deriv '^ exp-deriv)
  'done)

;d)
;If we changed the indexing, we would change the order in put


(install-sum-package)
(install-product-package)
(install-quotient-package)
(install-exp-package)
