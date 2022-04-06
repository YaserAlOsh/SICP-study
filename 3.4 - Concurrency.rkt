#lang sicp

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release)
             (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))
(define (test-and-set! cell)
  (if (car cell) true
      (begin (set-car! cell true) false)))

;Exercise 3.47
;a)

(define (make-semaphore n)
  (define (make-mutex-numbered)
    (cons 0 (make-mutex)))
  (define (mutex-WL m)
    (car m))
  (define (mutex-obj m)
    (cadr m))
  
  (define (make-mutexes n)
    (if (= n 0) '()
        (cons (make-mutex-numbered)
              (make-mutexes (- n 1)))))
  (define (find-mutex mutexes)
    (define (find-min min l)
       (cond ((null? l) min)
             ((< (mutex-WL (car l)) (mutex-WL min))
              (find-min (car l) (cdr l)))
             (else (find-min min (cdr l)))))
    (find-min (car mutexes) (cdr mutexes)))
  
  (let ((mutexes (make-mutexes n)))
    (define (release mutex)
      (define (iter l)
        (cond ((null? l) #f)
              ((eq? mutex (car l))
               (mutex 'release))
            (else (iter (cdr l)))))
      (iter mutexes))
    (define (semaphore m)
      (cond ((eq? m 'acquire)
             (let ((mutex (find-mutex mutexes)))
               ;(if (not mutex)
               (mutex 'acquire)))
            ((eq? m 'release) release)))
    semaphore))

(define (make-semaphore-mutex n)
  (let ((mutex (make-mutex));We use a cell so that only one process is accessing the acquired variable at the same time
        (acquired 0))
    
    (define (semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (< acquired n)
                   (begin (set! acquired (+ acquired 1))
                          (mutex 'release))
                   (begin
                     (mutex 'release)
                     (semaphore 'acquire))))
             
            ((eq? m 'release)
             (mutex 'acquire)
             (set! acquired (- acquired 1))
             (mutex 'release))))
    semaphore))

;b)

(define (make-semaphore-prim n)
  (let ((cell (list false));We use a cell so that only one process is accessing the acquired variable at the same time
        (acquired 0))
    
    (define (semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (semaphore 'acquire))
             (if (< acquired n)
                   (begin (set! acquired (+ acquired 1))
                          (set-car! cell false))
                   (begin
                     (set-car! cell false)
                     (semaphore 'acquire))))
             
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (semaphore 'release))
             (set! acquired (- acquired 1))
             (set-car! cell false))))
    semaphore))



;Exercise 3.48

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (1+ x) (+ x 1))
(define (make-counter)
  (let ((counter 0)
        (s (make-serializer)))
    (s (lambda ()
         (begin
           (set! counter (1+ counter))
           counter)))))

(define get-id (make-counter))


(define (make-account-and-serializer balance)
  (define uid 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'uid) uid)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    (set! uid (get-id))
    dispatch))

(define (exchange account1 account2)
  (let ((difference
         (- (account1 'balance) (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;Always entering the lower numbered account first will work because in the exchange problem,
;we are serializing one account at a time, and we know what accounts we need to serialize,
;.. so whoever acquires a process earlier will get to continue, and the other will wait.

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'uid))
        (id2 (account2 'uid)))
    
    ((if (< id1 id2)
         (serializer2 (serializer1 exchange))
         (serializer1 (serializer2 exchange)))
     account1 account2)))

;Exercise 3.49
;For the numbered account idea to work against deadlock, each process needs to know in advance what accounts are going to be serialized.
;A situation could be a process that exchanges specific accounts from a shared list based on a predicate, and that predicate requires access to an account that is shared.
;Example:
;Process(a1, a2, a3)
;  Deposit (a1, x)
;  Deposit (a2, (/ (a1 balance) 2)
;  Withdraw (a3, (+ (balance a1) (balance a2)))

;




