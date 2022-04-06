#lang sicp

(define (list-ref list n)
  (if (= n 0)
      (car list)
      (list-ref (cdr list) (- n 1))))

;(define (length items)
;  (if (null? items)
;     0
;      (+ 1 (length (cdr items)))))

(define (length items)
  (define (length-iter list n)
    (if (null? list) n
        (length-iter (cdr list) (+ n 1))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1) list2
      (cons (car list1) (append (cdr list1) list2))))
; Assuming list1 has one element only
; > (cons (car list1) (append (cdr list1) list2))
; > (cons (car list1) list2)
; Assuming list1 has two elements only
; > (cons (car list1) (append (cdr list1) list2))
; > (cons (car list1) (cons (car (cdr list1)) (append (cdr (cdr list1)) list2)))
; > (cons (car list1) (cons (car (cdr list1)) list2))

;Ex 2.17
;(define (last-pair items)
;  (if (null? (cdr items))
;         (list (car items))
;         (last-pair (cdr items))))

;iterative implementation
(define (last-pair items)
  (define (last-pair-iter item)
    (if (null? (cdr item)) (list (car item))
        (last-pair-iter (cdr item))))
  (last-pair-iter items))
; (last-pair (list 1 2 3))
; (last-pair-iter (list 1 2 3))
; (last-pair-iter (list 2 3)
; (last-pair-iter (list 3))
; (cdr (list 3) = nil
; (list (car (list 3))) > (list 3)

;Ex 2.18

(define (reverse list)
  (let ((n (length list)))
    ;Brute force recursive approach
     (define (reverse-loop i)
       (if (< i 0) nil
       (cons (list-ref list i) (reverse-loop (- i 1)))))
    ;Brute force iterative approach
    (define (reverse-loop-iter item i)
       (if (= i n) item
       (reverse-loop-iter (cons (list-ref list i) item) (+ i 1))))
       ;(reverse-loop-iter (cons (last-pair item) nil) (- i 1))))
    (reverse-loop-iter nil 0)))

;Much faster iterative way
(define (reverse-2 list)
  (define (do-reverse list prev)
    (if (null? list) prev
    (do-reverse (cdr list) (cons (car list) prev))))
  (do-reverse (cdr list) (cons (car list) nil)))

;For benchmark
(define (make-list n)
  (define (make-list-iter item i)
    (if (= i 1) (cons i item)
        (make-list-iter (cons i item) (- i 1))))
  (make-list-iter nil n))

;Ex 2.19

(define (cc amount coin-values)
  (cond (
         ;If amound is 0 then we have found a solution (.i.e one way to change the original amount using one of the coin values) 
         (= amount 0) 1)
        ;If amount is negative or there isn't more possible coin values, then we have not found a way to change
        ((or (< amount 0) (no-more? coin-values)) 0)
        ;The number of ways to change amount x with n coin values is the number of ways to change x without using the first coin value
        ; plus the ways to change x using the first coin value
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? list) (null? list))
(define (except-first-denomination list) (cdr list))
(define (first-denomination list) (car list))
;Unnecessary expansion just to see how it works
;(cc 10 (list 10 5 1))
;(+ (cc 10 (list 5 1)) (cc (- 10 (car (list 10 5 1))) (list 10 5 1)))
;(+ (cc 10 (list 5 1)) (cc (- 10 10) (list 10 5 1)))
;(+ (cc 10 (list 5 1)) (cc 0 (list 10 5 1)))
;(+ (cc 10 (list 5 1)) 1) ;This 1 corresponds to one way to change 10 using 10 coin value(denomination)
;(+ (+ (cc 10 (list 1)) (cc (- 10 5) (list 5 1))) 1)
;(+ (+ (cc 10 (list 1)) (cc 5 (list 5 1))) 1)
;(+ (+ (+ (cc 10 nil) (cc (- 10 1) (list 1))) (+ (cc 5 (list 1)) (cc (- 5 1) (list 5 1)))) 1)
;(+ (+ (+ 0 (cc 9 (list 1))) (+ (cc 5 (list 1)) (cc 4 (list 5 1)))) 1)
;(+ (+ (+ 0 (+ (cc 9 nil) (cc (- 9 1) (list 1)))) (+ (+ (cc 5 nil) (cc 4 (list 1)))  (+ (cc 4 (list 1)) (cc -1 (list 5 1))))) 1)
;(+ (+ (+ 0 (+ 0 (cc 8 (list 1)))) (+ (+ 0 (+ (cc 4 nil) (cc 3 (list 1))))  (+ (+ (cc 4 nil) (cc 3 (list 1))) 0))) 1)
;(+ (+ (+ 0 (+ 0 (+ (cc 8 nil) (cc 7 (list 1))))) (+ (+ 0 (+ 0 (cc 3 (list 1))))  (+ (+ (cc 4 nil) (cc 3 (list 1))) 0))) 1)
;we can see that (cc 7 (list 1)) in the end will give us 1
; and (cc 3 (list 1)) will give us 1 also, so we get 2
; and we have 1 at the end so we get 4


;Ex 2.20



(define (same-parity . z)
  (define (same-parity-loop items results isEven)
    (cond ((null? items) results)
          ((= (remainder (car items) 2) isEven)
              (same-parity-loop (cdr items) (cons (car items) results) isEven))
          (else (same-parity-loop (cdr items) results isEven)))
    )
  (reverse-2 (same-parity-loop z nil (remainder (car z) 2))))

(define (same-parity-recursive . args) 
         (define (same-parity-step parity lst) 
                 (cond ((null? lst) nil) 
                       ((= parity (remainder (car lst) 2))
                        (cons (car lst)(same-parity-step parity (cdr lst)))) 
                       (else (same-parity-step parity (cdr lst))) 
                 ) 
         ) 
         (same-parity-step (remainder (car args) 2) args) 
 )

;Mapping Sequences

(define (map proc items)
  (if (null? items) nil
      (cons (proc (car items)) (map proc (cdr items)))))
;Ex 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))
(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

;Ex 2.22
;The first version produces a list in reverse because the 'answer' argument is used as the cons,
;so for every call to square-list, the result of the previous squares will be in the 'cdr' section of the cons.
;The other version also does not work; Because to create a list, the last pair cdr should be nil,
;but with this implementation, the first pair car is nil, and the last pair cdr is the last result, which does not create a list
;The solution would be to reverse the list after it's produced with the first version of square-list


;Ex 2.23
(define (foreach proc items)
  (cond ((null? items) true)
      (else (proc (car items)) (foreach proc (cdr items)))))
;Better way; Doesn't need cond for the block structure
(define (for-each proc items)
  (define (for-each-iter items evaluate)
    (if (null? items) #t
        (for-each-iter (cdr items) (proc (car items)))))
  (for-each-iter items #t))




;count leaves:
(define (count-leaves list)
  (cond ((null? list) 0)
        ((not (pair? list)) 1)
        (else (+ (count-leaves (car list)) (count-leaves (cdr list))))))

;Ex 2.24
; > (list 1 (list 2 (list 3 (list 4))))
; (1 (2 (3 (4))))
; _________
; | * , * | 
; ---------
;
;(cons 1 (cons (cons 2 (cons (cons 3 (cons (cons 4 nil) nil)) nil)) nil))

;Ex 2.25

;(define l1 (list 1 3 (list 5 7) 9))
;(cdr (car (cdr (cdr l1))))

;(car (car (list (list 7))))

;(define l2 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;(car(cdr (car (cdr (car (cdr (car ( cdr (car (cdr (car (cdr l2))))))))))))

;Ex 2.26
;(append x y)
;(1 2 3 4 5 6)
;(cons x y)
;((1 2 3) 4 5 6)
; (list x y)
;((1 2 3) (4 5 6))

;Ex 2.27

(define (deep-reverse list)
  (define (do-reverse list prev)
    (cond ((null? list) prev)
          ((pair? (car list)) (do-reverse (cdr list) (cons (deep-reverse (car list)) prev)))
          (else (do-reverse (cdr list) (cons (car list) prev)))))
  (do-reverse list nil))

(define l (list (list 1 2) (list 3 4)))

;Ex 2.28

(define (fring2 list)
  (define (deep-visit list)
    (cond ((null? list) list)
          ((not (pair? list)) list)
          ((pair? (car list)) (cons (deep-visit (car list)) (deep-visit (cdr list))))
          (else (cons (deep-visit (car list)) (deep-visit (cdr list))))))
  (deep-visit list))

(define (fring list)
  (define (deep-visit l r);list,result
    (cond
          ((null? l) r)
          ((not (pair? l)) (cons l r))
          ;((null? (cdr list)) (cons (car list) accu))
          ((pair? l) (deep-visit (car l) (deep-visit (cdr l) r)))
          ))
  (deep-visit list nil))
  

;(deep-visit (car (list 1 2)) (deep-visit (cdr (list 1 2)) nil))
;(deep-visit 1 (deep-visit 2))
;(cons 1 (deep-visit 2 nil))
;(cons 1 (cons 2 nil))
;(deep-visit (car l) (deep-visit (cdr l) nil))

;(deep-visit (list 1 2) (deep-visit (cons (list 3 4) nil) nil))
;(deep-visit 1 (deep-visit 2 (deep-visit (cons (list 3 4) nil) nil)))
;(cons 1 (deep-visit 2 (deep-visit (cons (list 3 4) nil) nil)))
;(cons 1 (cons 2 (deep-visit (cons (list 3 4) nil) nil))))
;(cons 1 (cons 2 (deep-visit (list 3 4) (deep-visit nil nil))))
;(cons 1 (cons 2 (cons 3 (cons 4 nil))))

;Took forever !!



;We can define a general fring which takes a procedure and returns

(define (deep-map tree proc base) 
  (define (deep-map-rec tree accu)
    (cond ((null? tree) accu)
          ((not (pair? tree)) (proc tree accu))
          ((pair? tree) (deep-map-rec (car tree) (deep-map-rec (cdr tree) accu)))
          ))
  (deep-map-rec tree base))

(define (deep-sum tree) (deep-map tree + 0))
;Ex 2.29:
;This will probably take a while
;It's basically about a balance
;a)
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch  m)  (car m))
(define (right-branch m)  (cdr m))

(define (branch-length b)    (car b))
(define (branch-structure b) (cdr b))

;The total weight procedure is similar to fring, except instead of appending to a list, we sum up the result


;b)
(define (total-weight mobile)
  (define (deep-sum b)
    (cond ((null? b) 0)
          ((not (pair? b)) b)
          ;((null? (branch-structure b)) (deep-sum (car b)))
          ;If the structure is not another mobile, then just return it, it will be the weight (one number)
          ((not (pair? (branch-structure b))) (branch-structure b))
          ;Otherwise, call total-weight on the mobile that is the branch structure
          ((pair? (branch-structure b)) (total-weight (branch-structure b)))
          ;((pair? b) (+ (deep-sum (car b)) (deep-sum (cdr b))))))
          ))
  (+ (deep-sum (left-branch mobile)) (deep-sum (right-branch mobile))))
;c)
(define (binary-mobile-balanced mobile)
  (define (torque b)
      (cond ((not (pair? (branch-structure b))) (* (branch-length b) (branch-structure b)))
            (else (* (branch-length b) (total-weight (branch-structure b))))
      ))
  (cond ((null? mobile) #f)
        ((not (pair? mobile)) #t)
        (else (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
              (binary-mobile-balanced (branch-structure (left-branch mobile)))
              (binary-mobile-balanced (branch-structure (right-branch mobile))))))) 
   ;(let ((sub-left  (pair? (branch-structure (left-branch mobile))))
    ;    (sub-right (pair? (branch-structure (right-branch mobile)))))
    
    ;(if (= (torque (left-branch mobile)) (torque (right-branch mobile)))
    ;  (cond ((and sub-left sub-right)
    ;         (and (binary-mobile-balanced (branch-structure (left-branch mobile)))
    ;              (binary-mobile-balanced (branch-structure (right-branch mobile)))))
    ;        (sub-left  (binary-mobile-balanced (branch-structure (left-branch mobile))))
    ;        (sub-right (binary-mobile-balanced (branch-structure (right-branch mobile))))
    ;        (else #t))
    ;  #f)))
;d)
  
;If we change the constructors, I will only need to change the branch-structure procedure
;I have changed them and they still work

;Branches and mobiles for testing

(define b1 (make-branch 2 10))
(define b2 (make-branch 4 5))

(define b3 (make-branch 2 6))
(define b4 (make-branch 3 4))
(define b0left (make-branch 2 (make-mobile b1 b2)))
(define b0right (make-branch 3 (make-mobile b3 b4)))
(define m (make-mobile b0left b0right))
(define m1 (make-mobile b3 b4))
(define m2 (make-mobile b0right b4))
(define b1left (make-branch 1 (make-mobile b0left b0right)))
(define b1right (make-branch 1 (make-mobile b3 b4)))
(define mboss (make-mobile b1left b1right))

;Ex 2.30
(define (square x) (* x x))
;Direct way
;(define (square-tree  tree)
;  (cond ((null? tree) nil)
;        ((not (pair? tree)) (cons (square tree) nil))
;        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))
(define (square-tree tree)
  (map (lambda (sub-tree) (if (not (pair? sub-tree)) (square sub-tree)
                   (square-tree sub-tree))) tree))

;Ex 2.31

(define (tree-map tree proc)
  (map (lambda (sub-tree) (if (not (pair? sub-tree)) (proc sub-tree) (tree-map sub-tree proc))) tree))

;Ex 2.32

;(define (map proc items)
;  (if (null? items) nil
;      (cons (proc (car items)) (map proc (cdr items)))))

(define (subsets s)
  (if (null? s)
      (list nil)
      ;The subsets of a list l is:
      ;1) the subsets of the list excluding the first item, plus
      ;2) the first item added  into the above subsets
      ;(cons (first item) (subsets (exclude first item)))
      
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (sub-tree) (cons (car s) sub-tree))
                      rest)))))

;(subsets (list 1 2))
;(let ((rest (subsets (list 2))))
;   > (subsets (list 2)) > (let ((rest (subsets nil))) > (llet ((rest (()))) (append rest (map proc rest
;   > (append (list nil) (map (lambda (sb) (cons (car s) sb) (list nil)))
;   > (append (list nil) (cons (car s) )) > (append (list nil) (cons 2 (cons nil nil))) > (list (list nil) (list 2 nil))

; > (append (list (list nil) (list 2 nil)) (map insert (list (list nil) (list 2 nil))))
; > (append (list (list nil) (list 2 nil)) (cons (insert (list nil) (map insert (list 2 nil))))
;(append (list (list nil) (list 2 nil)) (cons (cons 1 (list nil)) (cons (insert 2) (map insert (list nil)))))
;(append (list (list nil) (list 2 nil)) (cons (cons 1 (list nil)) (cons (cons 1 2) (cons (insert (list nil)) (map insert nil)))))
;(append (list (list nil) (list 2 nil)) (cons (cons 1 (list nil)) (cons (cons 1 2) (cons (cons 1 nil) nil))))

