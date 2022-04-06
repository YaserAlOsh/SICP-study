#lang sicp
;Chapter 3
;Section 3.1

;Ex 3.1

(define (make-accumulator sum)
  (lambda (amount)
    (begin (set! sum (+ sum amount))
           sum)))

(define A (make-accumulator 0))
(define B (make-accumulator 5))


;Ex 3.2
(define (make-monitored f)
  ;Using lambda:
  ;(let ((count 0))
   ; (lambda (arg)
    ;  (cond ((eq? arg 'how-many-calls?) count)
     ;       ((eq? arg 'reset-count) (set! count 0))
      ;      (else (set! count (inc count)) (f arg))))))
  (define count 0)
  (define (mf arg1 . args)
    (cond ((eq? arg1 'how-many-calls?) count)
            ((eq? arg1 'reset-count) (set! count 0))
            (else (set! count (inc count)) (apply f (cons arg1 args)))));(f arg))))
  mf)

;Ex 3.3
(define (make-account balance password)

  ;Ex 3.4
  (define invalid-attempts 0)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient Funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (apply-request m)
    (cond
      ((eq? m 'joint) joint-acc)
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request: MAKE-ACCOUNT" m))))
  
  (define (wrong-password x) "Incorrect Password")
  
  (define (call-the-cops x) "Cannot access account; Password entered incorrectly more than 7 times")
  (define (reset-attempts) (set! invalid-attempts 0))
  ;Edited check-password for Ex 3.7
  (define (check-password attempt correct)
    (if (and (eq? attempt correct) (<= invalid-attempts 7))
               (begin reset-attempts (cons #t '()))
               (begin (set! invalid-attempts (inc invalid-attempts))
                  (if (> invalid-attempts 7)
                      (cons #f call-the-cops)
                      (cons #f wrong-password)))))
                      ;(begin call-the-cops (cons #f "Cannot access account; Password entered incorrectly more than 7 times")) 
                      ;(cons #f "Incorrect Password" ))))); "Incorrect Password")))))
  
  ;Edited dispatch for ex 3.7: Add original password option
  
  (define (dispatch correct-pass attempt-pass m)
    (let ((password-check (check-password attempt-pass correct-pass)))
      (if (not (car password-check))
          (cdr password-check)
          (apply-request m))))
  ;For Ex 3.7
  (define (joint-acc access-pass)
    (lambda (p m) (dispatch access-pass p m)))
  
  
  (lambda (p m) (dispatch password p m)))
(define acc (make-account 1000 'pass))


;Sec 3.1.2
(define random-init 5)
(define (random-update x) (remainder (+ (* 3 x) 11) 16))
(define rand
  (let ((x 5))
    (lambda ()
      (set! x (random-update x))
      x)))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (random 1000) (random 1000)) 1))
(define (square x) (* x x))
;Ex 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trails)
  (let ((area (* (abs (- x2 x1)) (abs (- y2 y1))))
        (perc (monte-carlo trails
               (lambda ()
                 (let ((random-x (random-in-range x1 x2))
                       (random-y (random-in-range y1 y2)))
                   (p random-x random-y))))))
    (* area perc)))



(define (estimate-circle-area h k r trails)
  (define (circle-predicate x y)
    (< (+ (square (- x h)) (square (- y k))) (square r)))
  (estimate-integral circle-predicate
                     (- h r) (+ h r) (- k r) (+ k r) trails))
(define (unit-circle-area trails) (estimate-circle-area 0 0 1.0 trails))
(define (estimate-pi2) (unit-circle-area 1000))

;Ex 3.6
(define rand2
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin (set! x (random-update x)) x))
            ((eq? m 'reset)
             (lambda (new-x) (set! x new-x)))))))


;Ex 3.7
;Convert password to old password when dispatching:
(define (make-joint original-acc original-pass joint-pass)
  ((original-acc original-pass 'joint) joint-pass))
  ;(lambda (password m)
   ; (if (eq? password joint-pass)
    ;    (original-acc original-pass m)
     ;   "Incorrect Password")))

;Ex 3.8
(define f
  (let ((val 0))
    (lambda (n)
      (let ((old-val val))
        (begin (set! val n)
                old-val))
          )))
  