#lang sicp
;Ex 3.25
(define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

(define (make-n-table same-key?)
  
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  
  (let ((local-table (list '*table* 'name)))
    (define (lookup keys-list)
      (define (loop-lookup keys table)
        (if (null? keys) (cadr table)
            (let ((subtable
               (assoc (car keys) (cddr table))))
              (if subtable
                  (if (null? (cdr keys))
                      (loop-lookup (cdr keys) subtable)
                      (loop-lookup (cdr keys) subtable))
                  false))))
      (loop-lookup keys-list local-table))
    
    (define (insert! keys-list value)
      (define (insert-loop! keys table)
        (if (null? keys)
            (set-cdr! table (cons value (cdr table)))
            (let ((subtable
                   (assoc (car keys) (cddr table))))
              (if subtable
                  (if (null? (cdr keys))
                      (insert-loop! (cdr keys) subtable)    
                      (insert-loop! (cdr keys) subtable))
                  (begin
                    (set-cdr! (cdr table)
                              (cons (list (car keys) (cons '() '()))
                                    (cddr table)))
                    (insert-loop! (cdr keys) (caddr table)))))))
      (if (null? keys-list)
          #f
          (insert-loop! keys-list local-table)) 
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        ((eq? m 'table) local-table)
        (else (error "Unknown operation: TABLE" m))))
    dispatch))
(define (insert table key-list value)
  ((table 'insert-proc!) key-list value))
(define (lookup table key-list)
  ((table 'lookup-proc) key-list))
(define (table-structure table)
  (table 'table))
(define t (make-n-table equal?))









