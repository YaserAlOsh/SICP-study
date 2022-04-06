#lang sicp




;Tables
;Ex 3.26
;To store key/value pairs in binary tree data structure, we have to change the assoc procedure and how we store key/value pairs in the local-table object
(define (<? a b)
  (cond ((and (number? a) (number? b))
         (< a b))
        ((and (string? a) (string? b))
         (string<? a b))
        (else a)))

;Binary Tree Representation
(define (make-empty-binary-tree)
  (list '() '() '()))
(define (make-binary-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (set-entry! tree value) (set-car! tree value))
(define (set-left-branch! tree value) (set-car! (cdr tree) value))
(define (set-right-branch! tree value) (set-car! (cddr tree) value))
  
;Associate binary tree search
(define (assoc-bt key records-bt)
  (cond ((null? records-bt) false)
        ((null? (entry records-bt)) false)
        ((equal? key (car (entry records-bt))) (entry records-bt))
        ((<?     key (car (entry records-bt))) (assoc-bt key (left-branch records-bt)))
        (else (assoc-bt key (right-branch records-bt)))))

(define (make-table-bt same-key?)
  ;Insert creates new trees in appropriate branches with the supplied key
  (define (insert-binary-tree tree key value)
    (cond ((null? (entry tree))
           (set-entry! tree (cons key value)))
          ((<? key (car (entry tree)))
           (if (null? (left-branch tree))
               (set-left-branch! tree (make-binary-tree (cons key value) '() '()))
               (insert-binary-tree (left-branch tree) key value)))
          (else
           (if (null? (right-branch tree))
               (set-right-branch! tree (make-binary-tree (cons key value) '() '()))
               (insert-binary-tree (right-branch tree) key value)))))
  
  
  (let ((local-table (cons '*table* (make-binary-tree '() '() '()))))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc-bt key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc-bt key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc-bt key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc-bt key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (insert-binary-tree (cdr subtable) key-2 value)))
                  ;(set-cdr! subtable
                   ;         (cons (cons key-2 value)
                    ;              (cdr subtable)))))
            (insert-binary-tree (cdr local-table) key-1 (make-binary-tree (cons key-2 value) '() '()))
             ;(set-cdr! (entry (cdr local-table))
              ;        (make-binary-tree (cons key-2 value) '() '())))
            ))
            ;(set-cdr! local-table
             ;         (cons (list key-1 (cons key-2 value))
              ;              (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        ((eq? m 'structure) local-table)
        (else (error "Unknown operation: TABLE" m))))
    dispatch))


;Compare performance to the normal unordered set representation:
(define (make-table same-key?)
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
        ((eq? m 'structure) local-table)
        (else (error "Unknown operation: TABLE" m))))
    dispatch))


(define t-bt (make-table-bt equal?))
(define t (make-table equal?))


(define (insert-into-table table a b)
  (define (s-loop i j n)
    (if (> j n) 'done
        (begin
          ((table 'insert-proc!) (list i j) (* j 2))
          (s-loop i (+ j 1) n))))
  (define (loop i)
    (if (> i b) 'done
        (begin 
          (s-loop i a b)
          (loop (+ i 1)))))
  (loop a))


;;#############
;This supports arbitrary dimensions tables, with binary tree search.

(define (make-n-bt-table)
  
  (define (make-item key value next-tree)
    (list key value next-tree))
  
  (define (key tree-entry)
    (car tree-entry))
  (define (entry-value tree)
    (cadr tree))
  (define (next-tree-ref tree)
    (caddr tree))
  
  (define (set-next-tree-ref! tree next-tree)
    (set-car! (cddr (entry tree))
              next-tree))
  
  (define (set-entry-value! tree-entry value)
    (set-car! (cdr tree-entry) value))
  
  (define (insert-binary-tree tree new-entry);key value)
    (cond
      ((null? tree)
       (begin (set! tree (make-binary-tree new-entry '() '())) tree))
      ((null? (entry tree))
           (begin (set-entry! tree new-entry) tree))
      ((<? (key new-entry) (key (entry tree)))
           (if (null? (left-branch tree))
               (begin
                 (set-left-branch! tree (make-binary-tree new-entry '() '()))
                 (left-branch tree))
               (insert-binary-tree (left-branch tree) new-entry)))
      (else
           (if (null? (right-branch tree))
               (begin
                 (set-right-branch! tree (make-binary-tree new-entry '() '()))
                 (right-branch tree))
               (insert-binary-tree (right-branch tree) new-entry)))))
  
  (let ((local-table (make-item '*table* 'name (make-binary-tree '() '() '()))))
    
    (define (lookup keys-list)
      (define (loop-lookup keys table)
        (if (null? keys) (entry-value table) ;(cadr table)
            (let ((subtable
               (assoc-bt (car keys) (next-tree-ref table))))
              (if subtable
                  ;(if (null? (cdr keys))
                      (loop-lookup (cdr keys) subtable)
                      ;(loop-lookup (cdr keys) (next-tree-ref subtable)))
                  false))))
      (loop-lookup keys-list local-table))
    
    (define (insert! keys-list value)
      (define (insert-loop! keys table)
        (if (null? keys)
            (set-entry-value! table value)
            ;(set-cdr! table (cons value (cdr table)))
            ;(insert-binary-tree
            (let ((subtable
                   (assoc-bt (car keys) (next-tree-ref table))))
              (if subtable
                  ;(if (null? (cdr keys))
                      (insert-loop! (cdr keys) subtable)
                      ;(insert-loop! (cdr keys) (next-tree-ref subtable)))
                  ;(if (null? (cdr keys))
                   ;   (insert-loop! (cdr keys) subtable)
                    ;  (insert-loop! (cdr keys) subtable))
                  ;(begin
                    ;(if (null? (cdr keys))
                        ;(insert-binary-tree table (make-item (car keys) value (make-empty-binary-tree)))
                        ;(set-entry! table (make-item (car keys) value '()))
                        ;(insert-loop! (cdr keys) (entry table)))
                        (begin
                          ;
                          ;(set-entry! table (make-item (car keys) '() (make-binary-tree '() '() '())))
                          (insert-loop! (cdr keys)
                                        ;(next-tree-ref
                                         (entry (insert-binary-tree
                                                 (next-tree-ref table)
                                                 (make-item (car keys) '() (make-empty-binary-tree)))))
                          ;)
                          
                    ;(set-cdr! (cdr table)
                     ;         (cons (list (car keys) (cons '() '()))
                      ;              (cddr table)))
                    ;(insert-loop! (cdr keys) (caddr table))
                        )))))
      (if (null? keys-list) #f
          (insert-loop! keys-list (identity local-table))) 
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        ((eq? m 'table) local-table)
        (else (error "Unknown operation: TABLE" m))))
    dispatch))






