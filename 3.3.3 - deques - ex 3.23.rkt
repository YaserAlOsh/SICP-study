#lang sicp

;Ex 3.23
;Deques

(define (make-deque)
  (cons '() '()))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item)
  (set-car! deque item));(make-deque-item item '())))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item));(make-deque-item item (rear-ptr deque))))

(define (empty-deque? deque)
  (null? (front-ptr deque)))
(define (deque-forward-ptr ptr) (cdr ptr))
(define (deque-back-ptr ptr) (caar ptr))
(define (item-value item)
  (cdr item))
(define (prev-ptr item)
  (car item))
(define (ptr-data ptr)
  (car ptr))

(define (set-prev-ptr! ptr prev-ptr)
  (set-car! (ptr-data ptr) prev-ptr))

(define (make-deque-item value prev-ptr)
  (cons prev-ptr value))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "")
      (cdar (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "")
      (cdar (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-item (cons (make-deque-item item '()) '())))
    (cond ((empty-deque? deque)
           ;(set-car! new-item (make-deque-item item '()))
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item)
           (print-deque deque))
          (else
           (set-cdr! new-item (front-ptr deque))
           (set-prev-ptr! (front-ptr deque) new-item)
           (set-front-ptr! deque new-item)
           (print-deque deque)))))
(define (rear-insert-deque! deque item)
  (let ((new-item (cons item '())))
    (cond ((empty-deque? deque)
             (set-car! new-item (make-deque-item item '()))
             (set-front-ptr! deque new-item )
             (set-rear-ptr! deque new-item)
           (print-deque deque))
          (else
           (set-car! new-item (make-deque-item item (rear-ptr deque)))
           (set-cdr! (rear-ptr deque) new-item)
           (set-rear-ptr! deque new-item)
           (print-deque deque)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque) (error "Deleting an empty deque"))
         (else (set-front-ptr! deque (deque-forward-ptr (front-ptr deque)))
               (set-prev-ptr! (front-ptr deque) '()))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque) (error "Deleting an empty deque"))
         (else  (set-cdr! (deque-back-ptr (rear-ptr deque)) '())
                (set-rear-ptr! deque (deque-back-ptr (rear-ptr deque))))))

(define (print-deque deque)
  (define (loop list item)
    (if (null? item) list
        (loop (cons (item-value (ptr-data item)) list) (deque-back-ptr item))))
  (if (empty-deque? deque)
     "" (loop '() (rear-ptr deque))))



(define q2 (make-deque))
(rear-insert-deque! q2 'a)
(rear-insert-deque! q2 'b)
(rear-insert-deque! q2 'c)
