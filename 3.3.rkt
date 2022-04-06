#lang sicp
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
;(define x (list 'a 'b))
;(define y (list 'c 'd))
;(define z (append x y))
;z
;(cdr x)
;(define w (append! x y))
;w
;(cdr x)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;(define v (list 'a 'b 'c 'd))
;(define w (mystery v))


;Ex 3.16
;(list 'a 'b 'c) ;Returns 3; Actually 3
(define x (cons 'b '()))
;(cons 'a (cons x x));Returns 4; Actually 3
(define x2 (cons 'a 'b))
(define x1 (cons x2 x2))
;(cons x1 x1);Returns 7; Actually 3
(define x4 (cons 'c '()))
(define x3 (cons 'a (cons 'b x4)))
(set-cdr! x4 x3)

(define (count-pairs-wrong x) 
      (if (not (pair?    x)) 
           0
           (+ (count-pairs-wrong (car x))
               (count-pairs-wrong (cdr x))
               1)))

;Ex 3.17
(define (contains list p)
    (cond ((null? list) #f)
          ((eq? (car list) p) #t)
          (else (contains (cdr list) p))))
(define (count-pairs x)
  (define pairs '())
  (define (add p) (set! pairs (cons p pairs)))
  
  (define (traverse node)
    (if (or (not (pair? node)) (contains pairs node))
        0
        (begin (add node)
               (+ (traverse (car node))
                  (traverse (cdr node))
                  1))))
  (traverse x))

;Ex 3.18
;Only cdrs
(define (list-cycles? list)
  (define pairs '())
  (define (add p) (set! pairs (cons p pairs)))

  (define (cdr-it pair)
    (if (not (pair? pair)) #f
        (if (contains pairs pair) #t
            (begin (add pair)
                   (cdr-it (cdr pair))))))
  (cdr-it list))
;Supports car's
(define (list-structure-cycles? list)
  (define pairs '())
  (define (add p) (set! pairs (cons p pairs)))

  (define (has-cycle? pair history)
    (if (not (pair? pair)) #f
        (if (contains history pair) #t
            (let ((car-result (has-cycle? (car pair) (cons pair history))))
              (if car-result #t
                  (has-cycle? (cdr pair) (cons pair history));(add pair)
                   )))))
  (has-cycle? list '()))

(define t1 (cons 'a 'b)) 
(define t2 (cons t1 t1))

(define x5 '(a b c)) 
(define y '(d e f)) 
(set-car! (cdr x5) y) 
(set-car! x5 (cdr x5)) 
(set-cdr! (last-pair y) (cdr y))


;Ex 3.19
;This takes successive cdrs and checks the previous pairs of the original list.
;It uses a counter to only check the previous pairs up to the current one
;It uses the original list so it doesn't create any new lists
(define (list-cycles-o1-space?-wrong list)
  ;(define pairs '())
  ;(define (add p) (set! pairs (cons p pairs)))
  (define (contains p n)
    (define (loop seq i)
      (if (>= i n) #f 
          (if (eq? p seq) #t
              (loop (cdr seq) (+ i 1)))))
    (loop list 0))
  (define (iter pair n)
    (if (not (pair? pair)) #f
        (if (contains pair n) #t
            (iter (cdr pair) (+ n 1)))))
  (iter list 0))

;Uses the rabbit and turtle algorithm, george floyd.
;Move the turtle once and the rabbit twice.
(define (list-cycles-o1-space? list)
  (define (loop slow fast)
    (cond ((not (pair? fast)) #f)
          ((not (pair? slow)) #f)
          ((eq? slow fast) #t)
          ((not (pair? (cdr fast))) #f);Check if there are two pairs to move to
          ((eq? slow (cdr fast)) #t);This might make the algorithm faster a bit.
          (else (loop (cdr slow) (cddr fast)))))
  (loop list (cdr list)))

(define (listmaker n) (if (= n 0) '() (cons n (listmaker (- n 1)))))
(define x6 (listmaker 100000))
(set-cdr! (last-pair x6) x6)

(define y1 '(1 2 3 4 5 6 7 8)) 
(set-cdr! (cdddr (cddddr y1)) (cdddr y1)) 
(define z '(1))
(set-cdr! z z)
;From CS-61A-Week10 solutions; Berkeley university
(define (cycle? lst)
  (define (subq? x list)
    (cond ((null? list) #f)
	  ((eq? x list) #t)
	  (else (subq? x (cdr list)))))
  (define (iter lst pairlist pairlist-tail)
    (cond ((not (pair? lst))
	   (set-cdr! pairlist-tail lst)
    	   #f)
	  ((subq? lst pairlist)
	   (set-cdr! pairlist-tail lst)
	   #t)
	  (else
	   (let ((oldcdr (cdr lst)))
	     (set-cdr! pairlist-tail lst)
	     (set-cdr! lst '())
	     (iter oldcdr pairlist lst) ))))
  (cond ((null? lst) #f)
	(else (let ((oldcdr (cdr lst)))
		(set-cdr! lst '())
		(iter oldcdr lst lst)))))



;From http://community.schemewiki.org/?sicp-ex-3.19
(define (cycles? x)
  (let ((rev (mystery x)))
    (if (eq? x rev) #t
        (begin
          (mystery rev)
          #f))))


;To support arbitrary list structures, we need to.... ???? \>-</ 

(define (has-cycle? tree) 
  ;; Helpers 
  (define (iterator value idx) 
    (cons value idx)) 
  (define (update-iterator it value idx) 
    (set-car! it value) 
    (set-cdr! it idx)) 
  (define (iterator-id it) 
    (cdr it)) 
  (define (iterator-value it) 
    (car it)) 
  (define (iterator-same-pos? it1 it2) 
    (eq? (iterator-id it1) (iterator-id it2))) 
  (define (iterator-eq? it1 it2) 
    (and (iterator-same-pos? it1 it2) 
         (eq? (iterator-value it1) (iterator-value it2)))) 
  
  ;; slow-it - tracks each node (1, 2, 3, 4...) 
  ;; fast-it - tracks only even nodes (2, 4...) 
  (let ((slow-it (iterator tree 0)) 
        (fast-it (iterator '() 0)) 
        (clock-cnt 0)) 
    (define (dfs root) 
      (if (not (pair? root)) 
          false 
          (begin 
            (set! clock-cnt (+ clock-cnt 1)) 
            (if (and (even? clock-cnt) 
                     (iterator-same-pos? slow-it fast-it)) 
                (update-iterator slow-it root clock-cnt)) 
            (if (even? clock-cnt) 
                (update-iterator fast-it root 
                                 (+ (iterator-id fast-it) 1))) 
            (if (iterator-eq? slow-it fast-it) 
                true 
                (or (dfs (car root)) 
                    (dfs (cdr root))))))) 
    (dfs tree)))
;This example breaks above algorithm
(define r-inf (list 'a 'b 'c 'd)) 
(set-cdr! (cdddr r-inf) r-inf) 




