#lang sicp
;Tables
;Section 3.3.3

(define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
;Simple key-value table implementation
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record
             (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record
             (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation: TABLE" m))))
    dispatch))

;Ex 3.24
(define (make-2d-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation: TABLE" m))))
    dispatch))

;Ex 3.25
(define (make-n-table same-key?)
  
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  
  (let ((local-table (list '*table*)))
    (define (lookup keys-list)
      (define (loop-lookup keys table)
        (if (null? keys) (cdr table)
            (let ((subtable
               (assoc (car keys) (cdr table))))
              (if subtable
                  (loop-lookup (cdr keys) subtable)
                  false))))
      (loop-lookup keys-list local-table))

    
    (define (insert! keys-list value)
      (define (insert-loop! keys table)
        (if (null? keys)
            (set-cdr! table value)
            (let ((subtable
                   (assoc (car keys) (cdr local-table))))
              (if subtable
                  (insert-loop! (cdr keys) subtable)
                  (begin
                    (set-cdr! table
                              (cons (list (car keys))
                                    (cdr table)))
                    (insert-loop! (cdr keys) (cadr table)))))))
      (if (null? keys-list)
          #f
          (insert-loop! keys-list local-table)) 
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation: TABLE" m))))
    dispatch))

;This is a simpler approach that uses lists as keys. It supports the requirement "different values may be stored under different numbers of keys"
(define (make-ndim-table)
  
  (define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup keys-list)
      (let ((record
             (assoc keys-list (cdr local-table))))
        (if record
            (cdr record)
            false)))
    
    (define (insert! keys-list value)
      (let ((record (assoc keys-list (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons keys-list value)
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation: TABLE" m))))
    dispatch))


(define (insert! key-list value table)
  ((table 'insert-proc!) key-list value))
(define (lookup  key-list table)
  ((table 'lookup-proc) key-list))
(define (table-structure table)
  (table 'table))
(define t (make-n-table equal?))

;Ex 3.27
;Memoization (Tabulation)


(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f  x)))
              (insert! x result table)
              result))))))
(define memo-fib
  (memoize
   (lambda (n)
     (cond
       ((= n 0) 0)
       ((= n 1) 1)
       (else (+ (memo-fib (- n 1))
                (memo-fib (- n 2))))))))

(define (fib n)
  (cond
       ((= n 0) 0)
       ((= n 1) 1)
       (else (+ (fib (- n 1))
                (fib (- n 2))))))




