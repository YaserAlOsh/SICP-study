#lang sicp
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))
(define (for-each procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

;Constraint System.



(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))


(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR" request))))
me))


(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
((connector 'connect) new-constraint))


(define (make-constraint process . args)

  (define (process-new-value)
    ;process logic
    (process me)
    )
  (define (process-forget-value)
    
    (for-each (lambda (x) (forget-value! x me)) args)
    (process-new-value))
  
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: ADDER" request))))
  
  (for-each (lambda (x) (connect x me)) args)
  me)

(define (adder a1 a2 sum)
  (define (process me)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (make-constraint process a1 a2 sum))

(define (multiplier m1 m2 product)
  (define (process me)
    (cond
      ((or (and (has-value? m1) (= (get-value m1) 0))
           (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
      ((and (has-value? m1) (has-value? m2))
       (set-value! product
                   (* (get-value m1) (get-value m2))
                   me))
      ((and (has-value? product) (has-value? m1))
       (set-value! m2
                   (/ (get-value product) (get-value m1))
                   me))
      ((and (has-value? product) (has-value? m2))
       (set-value! m1
                   (/ (get-value product) (get-value m2))
                   me))))
  (make-constraint process m1 m2 product))

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ") (display name)
    (display " = ") (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
'ok))


;Ex 3.33
(define (averager a b c)
  (let
      ((x (make-connector))
       (y (make-connector)))
    (multiplier c y x)
    (adder a b x)
    (constant 2 y))
  'ok)

;Ex 3.34


;Ex 3.35
(define (squarer a b)
  (define (process-new-value me)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (* (get-value a)  (get-value a)) me))))
  (make-constraint process-new-value a b))


;Ex 3.37

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))
(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (celsius-fahrenheit-converter-exp x)
  (c+ (c* (c/ (cv 9) (cv 5)) x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter-exp C))





