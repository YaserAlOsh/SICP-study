#lang sicp


;Seciton 2.3
;Symbolic Data

;Ex 2.54

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)))


;Symbolic differentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         ;Here we can check if the exponent is an expression or just a number
         ;Now we assume it's just a number
         (make-product
          (exponent exp)
          (make-product
           (make-exp (base exp) (- (exponent exp) 1))
           (deriv (base exp) var))
           ))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
(and (variable? v1) (variable? v2) (eq? v1 v2)))



(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        ((null? a2) a1)
        ;Make it accept arbitrary numbers of terms
        ;((pair? a2) (if (not (null? (cdr a2)))
         ;               (list '+ a1 (make-sum (car a2) (cdr a2)))
          ;              (list '+ a1 (car a2))))
        
        ;Additional conditions for symplifying expressions
        ((same-variable? a1 a2) (list 2 '* a1))
        ((same-variable-summed? a1 a2) (list (+ 1 (oprt1 a1)) '* a2))
        ((same-variable-summed? a2 a1) (list (+ 1 (oprt1 a2)) '* a1))
        ;############
        ;(else (list a1 '+ a2))
        (else (combiner a1 a2 '+))
        ))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) (if (product? m2) (make-product (multiplier m2) (multiplicand m2)) m2)) ;m2)
        ((=number? m2 1) (if (product? m1) (make-product (multiplier m1) (multiplicand m1)) m1))
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((null? m2) m1)
         ;Additional conditions for symplifying expressions
        ((same-variable? m1 m2) (list m1 '^ 2))
        ((same-variable-multiplied? m1 m2) (make-product (get-multiplied-var m1 m2) (list m1 '^ 2)))
        ((same-variable-exp? m1 m2) (list m2 '^ (+ 1 (exponent m1))))
        ((same-variable-exp? m2 m1) (list m1 '^ (+ 1 (exponent m2))))
        ((same-variable-exp-both? m1 m2)
         (list (base m1) '^ (+ (exponent m1) (exponent m2))))
      
        ((or (product? m1) (product? m2))
         (let ((is-prod-m1 (product? m1)) 
                        ;(same-variable? (first-multiplicand-item m1) m2)))
               (is-prod-m2 (product? m2)))
                        ;(same-variable? (first-multiplicand-item m2) m1))))
             
             (if (and (or is-prod-m1 is-prod-m2)
                      (same-variable?
                       (if is-prod-m1 (first-multiplicand-item m1) m1)
                       (if is-prod-m2 (first-multiplicand-item m2) m2)))
                 (make-product
                  (if is-prod-m1 (make-product (multiplier m1) (multiplicand m1)) m1)
                  (if is-prod-m2 (make-product (multiplier m2) (multiplicand m2)) m2))
                 (combiner m1 m2 '*))))
    
        ;((and (product? m1) 
         ;     (same-variable? (first-multiplicand-item m1) m2))
          ; (make-product (make-product (multiplier m1) (multiplicand m1)) m2))
        ;((and (product? m2) 
         ;     (same-variable? (first-multiplicand-item m2) m1))
          ; (make-product m1 (make-product (multiplier m2) (multiplicand m2))))
        ;####################################
        ;(else (list m1 '* m2))
        (else (combiner m1 m2 '*))
        ))

;Predicate
(define (sum? x)
  (and (pair? x)
       (or (eq? (operation x) '+)
           (takes-precedence x '+))))
 
(define (addend s)
  (cond ((equal? (operation s) '+) (oprt1 s))
        (else (first-operands s '+))))
        ;((takes-precedence s '+) (list (oprt1 s) (operation s) (oprt2 s)))))

(define (augend s)
  (cond ((null? (cdddr s)) (oprt2 s))
        ((equal? (operation s) '+)
         (multi-term s '+));Make it accept arbitrary numbers of terms if there is more than one item left to be summed
        ;Implement order of operations here. where summutation is done last
        (else (second-operands s '+))))

  ;(if (null? (cdddr s)) (oprt2 s)
  ;    (if (equal? (operation s) '+)
  ;      (multi-term s '+);Make it accept arbitrary numbers of terms if there is more than one item left to be summed  
  ;      (next-operands s)))
   ;(list-to-oper-groups (cddr s) '+)))
   ;(make-sum (caddr s) (cdddr s)))); (make-sum (cadddr s) (augend (list '+ (cdddr s))))))



(define (product? x)
  ;
  (and (pair? x) (eq? (operation x) '*)))

(define (multiplier p) (oprt1 p))

(define (multiplicand p)
  (if (null? (cdddr p)) (oprt2 p)
       ;Make it accept arbitrary numbers of terms
       ;If there is more than one item left to be multiplied
      (multi-term p '*)))
      ;(list-to-oper-groups (cddr p) '*)))

;Ex 2.57

(define (exp a b)
  (define (iter n res)
    (if (= n 1) res
        (iter (- n 1) (* res a))))
  (iter b a))

(define (make-exp a b)
  (cond ((=number? a 1) 1)
        ((=number? b 0) 1)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (exp a b))
        (else (list a '^ b))))

(define (exponentiation? x) (and (pair? x) (eq? (operation x) '^)))

(define (base e) (oprt1 e))

(define (exponent e) (oprt2 e))


;Ex 2.57
;Use this inside augend and multiplicand if there are more than two items to be summed (or multiplied)
(define (list-to-oper-groups l op)
  (if (null? (cdr l)) (car l)
      (list op (car l) (list-to-oper-groups (cdr l) op))))



;

;(define (sum-in? expr) (= '+ (cadr expr)))

;Prefix notation
;(define (oprt1 expr) (cadr expr))
;(define (operation expr) (car expr))
;(define (oprt2 expr) (caddr expr))

;Infix notation
(define (oprt1 expr) (car expr))
(define (operation expr) (cadr expr))
(define (oprt2 expr) (caddr expr))

(define (multi-term expr op)
  (cddr expr);For infex
  ;(list-to-oper-groups (cddr p) op)For prefix
  )

;Order of operations

(define (takes-precedence expr op)
  (cond  ((not (pair? expr)) #f)
         ((not (pair? (cdddr expr))) #f)
         ((equal? (operation (cddr expr)) op) #t);cddr gives the second expression (after the first operation)
         ;((pair? (cddr expr)(takes
         (else (takes-precedence (cddr expr) op))));(cddr (cdddr expr)) gives the expressions following the second expression
         ;(else #f)))


(define (first-operands expr op)
  (define (iter exp result)
    (cond ((not (pair? (cdddr exp))) result) 
          ((equal? (operation exp) op) result)
          (else (iter (cddr exp) (append result (list (operation exp) (oprt2 exp)))))))
          ;(else (cons result (cons (operation exp) (cons (oprt2 exp) (iter (cddr exp) nil)))))))
  (iter expr (oprt1 expr)))

(define (second-operands expr op)
  (define (iter exp result)
    (cond ((null? (cdr result)) (car result))
          ((not (pair? (cdddr exp))) result)
          ((equal? (operation exp) op) result)
          (else (iter (cddr exp) (cddr result)))))
          ;(else (cons result (cons (operation exp) (cons (oprt2 exp) (iter (cddr exp) nil)))))))
  (iter expr (cddr expr)))

(define (append list1 list2)
  (if (null? list1) list2
      (if (not (pair? list1)) (cons list1 list2)
          (cons (car list1) (append (cdr list1) list2)))))

(define (combiner a1 a2 op)
  ;(list a1 op a2))
  (append a1 (list op a2)))


;Additional procedures for symplifying expressions

(define (first-multiplicand-item expr)
  (let ((multiplicand (multiplicand expr)))
    (if (pair? multiplicand) (car multiplicand) multiplicand)))

;Additional procedures for symplifying expressions
;###############
(define (same-variable-summed? v1 v2)
  (or (and (variable? v1) (variable? v2) (eq? v1 v2))
      (and (product? v1) (number? (oprt1 v1)) (same-variable? (oprt2 v1) v2))))
      ;(and (product? v2) (=number? (oprt1 v2)) (same-variable? (oprt2 v2) v1))))

(define (same-variable-multiplied? v1 v2)
  ;(or (and (variable? v1) (variable? v2) (eq? v1 v2))
  (and (product? v2) ;(number? (oprt1 v2))
       (or (and
            (not (same-variable? (oprt1 v2) v1))
            (same-variable? (oprt2 v2) v1))
           (and
            (not (same-variable? (oprt2 v2) v1))
            (same-variable? (oprt1 v2) v1)))))

(define (get-multiplied-var v1 v2)
  (if (not (same-variable? (oprt1 v2) v1)) (oprt1 v2) (oprt2 v2)))

(define (same-variable-exp? v1 v2)
  (or (and (variable? v1) (variable? v2) (eq? v1 v2))
      (and (exponentiation? v1) (number? (oprt2 v1)) (same-variable? (oprt1 v1) v2))))
(define (same-variable-exp-both? v1 v2)
  ;(or (and (variable? v1) (variable? v2) (eq? v1 v2))
     (and (exponentiation? v1) (exponentiation? v2)
          (number? (exponent v1)) (number? (exponent v2))
          (same-variable? (base v1) (base v2))))

;##############