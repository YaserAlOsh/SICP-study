#lang sicp
(define (accumulate op base seq)
  (cond ((null? seq) base)
        (else (op (car seq) (accumulate op base (cdr seq))))))

;Ex 2.33

(define (map func seq)
  (accumulate (lambda (x y) (cons (func x) y)) nil seq))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))


;Ex 2.34

(define (exp x n)
  (define (exp-iter a accu)
    (if (= a n) accu
        (exp-iter (+ a 1) (* accu x))))
  (exp-iter 1 x))

(define (horner-eval x coefficient-sequence)
  ; a0 + x(a1 + x(a2 + ... x(an-1 + xan))) > a0 + xa1 + x^2a2 + .. +x^(n-1)*(n-1) + x^(n)*n
 (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
   0
    coefficient-sequence))

;1+ 3x + 5x3+x5 at x = 2
;(horner-eval 2 (list 1 3 0 5 0 1))

;Ex 2.35
;One messy way to do it
(define (count-leaves tree)
  (accumulate (lambda (x y) (+ (if (not (pair? x)) 1 (count-leaves x)) y)) 0 tree))

(define (enumerate-tree tree)
 (cond ((null? tree) nil)
       ((not (pair? tree)) (list tree)) ;Use (list tree) here because we should always supply a sequence to the append procedure
       (else (append (enumerate-tree (car tree))
                     (enumerate-tree (cdr tree))))))
;Using enumerate-tree

(define (count-leaves-enumerate tree)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree tree)))) 
;Another recursion way
(define (count-leaves-map-recursion tree)
  (accumulate + 0 (map (lambda (node) (cond
                                     ((pair? node) (count-leaves-map-recursion node))
                                     (else 1)) tree))))



(define l (list (list 1 2) (list 3 4)))

;Ex 2.36

(define (accumulate-n op base seqs)
  (if (null? (car seqs)) nil
      (cons (accumulate op base (map car seqs))
            (accumulate-n op base (map cdr seqs)))))

(define l2 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))


;Ex 2.37

;Doing them without using the general map function

(define (dot a b) (accumulate + 0 (accumulate-n * 1 (list a b))))

(define (matrix-*-vector m v) (map (lambda (r) (dot r v)) m))

(define (transpose m) (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
      ;if we transpose the second matrix, then we just need to multiply the second matrix by every row of the first
        (map (lambda (row) (matrix-*-vector cols row)) m)))

(define m  (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define m2 (list (list 1 2 3) (list 4 4 5) (list 6 6 6) (list 7 8 9)))
(define v (car m))

;Ex 2.38
(define (fold-right op initial sequence)
 (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result) (cdr rest))))
  (iter initial sequence))

(define (fold-left op initial sequence)
 (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

;The property is that op should not depend on the order of the sequence
;For example, op + or * will produce the same result, due to the commutative laws
;in other words, (op a b) should be the same as (op b a)

;Ex 2.39

(define (reverse seq)
  (accumulate (lambda (x y) (append y (list x))) nil seq))

(define (reverse2 seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))




(define (enumerate-interval a n)
  (if (> a n) nil
        (cons a (enumerate-interval (+ a 1) n))))

(define (filter predicate seq)
  (accumulate (lambda (x y) (if (predicate x) (cons x y) y)) nil seq))

(define (remove item seq)
  (filter (lambda (x) (not (= item x))) seq))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;Ex 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime? x) 
   (define (test divisor) 
     (cond ((> (* divisor divisor) x) true) 
           ((= 0 (remainder x divisor)) false) 
           (else (test (+ divisor 1))))) 
   (test 2)) 

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))));Use cadr because the pair is a list

(define (make-pair-sum pair) (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
     (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

;Ex 2.41

(define (sum-equals pair s)
  (= (+ (car pair) (cadr pair)) s))

(define (specific-sum-pairs n s)
  (map make-pair-sum
       (filter (lambda (p) (sum-equals p s)) (unique-pairs n))))

;Ex 2.42

(define empty-board nil)

(define (adjoin-position new-row k position)
  ;(cons (list new-row k) position)
  ;(if (null? position) (list (list new-row k))
  ;  (append (list (list new-row k)) position))
  (cons (list new-row k) position)
  )

(define (row coordinate) (car coordinate))
(define (column coordinate) (cadr coordinate))
(define (safe? k positions)
  ;Check if the row of any queen is the same, or the difference between the row and the column are the same
  ;(let ((checks
         ;(flatmap (lambda (pos)
         ;   (map (lambda (pos2)
         ;       (cond ((= (car pos) (car pos2)) 1) ((= (- (car pos) (car pos2)) (- (cadr pos) (cadr pos2))) 1) (else 0)))
         ;    (cdr positions)))
         ;  positions)))
   ;      (map (lambda (prev-queen) (cond  ((= ( caar positions) (car prev-queen)) 1)
   ;                                       ((= (abs (- (caar positions) (car prev-queen))) (abs (- (cadar positions) (cadr prev-queen)))) 1)
   ;                                       (else 0)))
   ;                                       (cdr positions))))
   ; (= (accumulate + 0 checks) 0))
   (let ((k-queen (car positions)))
   (null? (filter (lambda (prev-queen) (cond
                                         ((= (row k-queen) (row prev-queen)) #t)
                                         ((= (abs (- (row k-queen) (row prev-queen)))
                                              (abs (- (column k-queen) (column prev-queen))))
                                           #t)
                                         (else #f)))
                  (cdr positions))))
  )

 
(define (queens board-size)
  (define (queen-cols k)
    (if (= 0 k)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap (lambda (rest-of-queens)
                    (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                       (enumerate-interval 1 board-size)))
           (queen-cols (- k 1))))))
  (queen-cols board-size))

;Ex 2.42
;In the original flatmap, the sequence will be boardsize^(k-1) in size. so the procedure of the flatmap will be called boardsize^(k-1) times
;Then for each flatmap procedure call, for each k, we will call a map with a sequence of length board-size, which is O(1) running time
;Then flatmap running complexity is O(boardsize^(k-1)) for each queen-cols call

;= O(boardsize^k) where k is the initial value boardsize
;Total running complexity (considering flatmap only):
;O(boardsize^(k-1) + boardsize^(k-2) + ... + boardsize)
;= O(boardsize^(k-1) approx

;In the new flatmap, we are calling it with a sequence of length 'boardsize' times, which is O(1),
;and for each call, we are calling the inner map with a sequence of length boardsize^(k-1)
;but then, to construct that sequence by calling queen-cols boardsize times, the flatmap gets called another boardsize times for each call,..
;.. making it get called boardsize^2. and each call of those will call it boardsize times again ..
;.. continuing this way, 'flatmap' gets called boardsize^k times..
;Then flatmap running complexity is

; .. each of those calls will call the inner map with a sequence of length that starts at boardsize^(k-1) and ends boardsize

;O(boardsize^k * (boardsize^(k-1) + boardsize^(k-2) + boardsize^(k-3) + ... boardsize)
; = O(boardsize^(k*(k-1))) approx

;For 8x8 queens puzzle, assuming the original implementation of flat map takes T time, the interchanged one will take:
;O(boardsize^k) * O(boardsize^(k-1)) = O(boardsize^k) * T = 8^8*T

(define (queens2 board-size)
  (define (queen-cols k)
    (if (= 0 k)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

