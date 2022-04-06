#lang sicp
(define (memo-proc procedure)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if already-run?
          result
          (begin
            (set! already-run? #t)
            (set! result (procedure))
            result)))))
;Streams Implementation
;(define (force p) (p))
;(define (delay p) (memo-proc (lambda () p)))

;(define (cons-stream x y)
 ; (cons x (delay y)))

(define (head stream)
  (car stream))
(define (tail stream)
  (force (cdr stream)))

(define the-empty-stream '())
(define (empty-stream? stream)
  (eq? stream the-empty-stream))

;Streams procedures
(define (stream-enumerate-interval low high)
  (if (> low high) the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (head s)
      (stream-ref (tail s) (- n 1))))

(define (stream-map proc stream)
  (if (empty-stream? stream)
      the-empty-stream
      (cons-stream
       (proc (head stream))
       (stream-map proc (tail stream)))))

(define (stream-filter pred stream)
  (cond
    ((empty-stream? stream) the-empty-stream)
    ((pred (head stream))
      (cons-stream (head stream)
       (stream-filter pred (tail stream))))
    (else
     (stream-filter pred (tail stream)))))

(define (display-stream s)
  (if (empty-stream? s)
      'done
      (begin
        (display (head s))
        (newline)
        (display-stream (tail s)))))
(define (display-stream-lim s max)
  (if (or
       (empty-stream? s)
       (= max 0))
      'done
      (begin
        (display (head s))
        (display " ");(newline)
        (display-stream-lim (tail s) (- max 1)))))
;Utilities
(define (square x) (* x x))

;Berkeley CS61A Week 11 Lab

(define (num-seq n)
  (cons-stream
   n
   (num-seq
    (if (= (remainder n 2) 0)
        (/ n 2)
        (+ (* 3 n) 1)))))
(define (seq-length seq-stream)
  (define (iter n seq-str)
    (if (or (empty-stream? seq-str)
            (= (head seq-str) 1))
        n
        (iter (+ n 1) (tail seq-str))))
  (iter 1 seq-stream))
                 
      
       

;Ex 3.50
(define (stream-map-gen proc . argstreams)
  (if (empty-stream? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map head argstreams))
       (apply stream-map-gen
              (cons proc (map tail argstreams))))))


;Ex 3.51
(define (show x)
  (display x)
  x)
;(define x
 ; (stream-map show (stream-enumerate-interval 0 10)))

;x will print 0
;it's value will be a stream with 0 as its head and a promise to map rest of the interval
;(stream-ref x 5)
;This will print the values from 1 to 5 and return 5.
;That's because it will request the stream x to compute its values each time it calls tail, and that call will trigger the (show) procedure.
;(stream-ref x 7)
;If we weren't using a memoized delay, then this will print values from 1 to 7
;But because we are using memoized delay, the x stream would have stored the result of calling (proc) on each of it's elements, so it won't call the print again.
;It will return 7 in either case.

;Ex 3.52
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)

;(define seq
 ; (stream-map accum
  ;            (stream-enumerate-interval 1 20)))

;sum = 1.


;(define y (stream-filter even? seq))

;This will call even? on the head of the stream which is 1.
;Since it returns false, it will call stream-filter on the tail of the stream
;(stream-filter even? (tail (1 #<promise>()))
;Which will force the seq stream to compute its second value, (+ sum i) = (+ 1 2)
;That in turn will change sum to (+ 2 1) = 3
;stream-filter will check 3 and again, will ask seq for the next element
;(stream-filter even? (tail (3 #<promise>()))
;Which will force the seq stream to compute its second value, (+ 3 3)
;sum becomes (+ 3 3) = 6
;stream-filter checks 6 and finds it's even,
;so y becomes a stream with 6 as its head and a promise to find the remaining even elements.
;(stream 6 (delay (stream-filter even? (stream 4 #<promise>))))

;(define z
 ; (stream-filter
  ; (lambda (x) (= (remainder x 5) 0))
   ;seq))

;stream-filter will check the first elements of seq 1,3,6 and none will satisfy its predicate, so it will ask for the next element.
;So far it hasn't changed the sum because the procedure accum has already been called on the first 3 elements.
;Now it forces the computation of the next element
;Sum becomes (+ 6 4) = 10,
;The new element 10 does satsify the predicate and z returns.
;(10 , #<promise>)
;(stream 10 (stream-filter lambda (stream (element i = 5) #<promise>)))

;(stream-ref y 7)

;y has only found one value so far, this will force the computation up to the 7th item
;1) it finds 6 is even
;it forces seq to accumulate 5, sum = 10 + 5 = 15
;15 is odd
;forces seq to accumulate 6, sum = 15 + 6 = 21
;21 is odd
;forces seq to accumulate 7, sum = 21 + 7 = 28
;2) 28 is even,
;forces seq to accumulate 8, sum = 28 + 8 = 36
;3) 36 is even,
;forces seq to accumulate 9, sum = 36 + 9 = 45
;forces seq to accumulate 10, sum = 45 + 10 = 55
;forces seq to accumulate 11, sum = 55 + 11 = 66
;4) 66 is even
;forces seq to accumulate 12, sum = 66 + 12 = 78
;5) 78 is even
;forces seq to accumulate 13, sum = 78 + 13 = 91
;forces seq to accumulate 13, sum = 91 + 14 = 105
;forces seq to accumulate 13, sum = 105 + 15 = 120
;6) 120 is even
;forces seq to accumulate 13, sum = 120 + 16 = 136
;7) 136 is even
;returns 136

;In effect, (stream-ref y) has added the values from 6 till 16 to the sum, which changed it from 15 to 136

;## Without Memoization ##
;sum = 15
;1) first element of y is 6
;tries to find next element
;We start with 4 (remember the promise of stream y)
;6 19 24 30 37 45 54 64 75 87 100 114 129 145 162 
;6 -  24 30 -  -  54 64 -  87 100 114 --- --- 162
;returns 162




;(display-stream z)
;It will force z to find all its values and will display all the values that are multiple of 5:
;10,15,45,55,105,120,190,210

;sum = 210

;## Without Memoization ##
;sum = 162
;1) first element of z is 10
;tries to find next element
;We start with adding 5 (remember the promise of stream z)
;     162  173 180 188 197 207 218 230 ... 288 305 323 342 362
;- 15  -    -  180 -   -   -   -  230 ...  -  305  -   -   -
;returns 305



;If we weren't using memoization, whenever we try to access the stream seq, the accum procedure will be called for the item
;So after defining seq,    sum = 1
;After defining y,         sum = 1 + 2 + 3 = 6
;After defining z,         sum = 6 + 2 + 3 + 4 = 15
;After (stream-ref y 7),   sum = ... (shown above)
;The response will be the  sum = 162

;After (display-stream z), sum = ... (shown above)
;305

;#### Infinite Streams ####

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map-gen + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))
(define (stream-scale s factor)
  (stream-map (lambda (x) (* x factor)) s))

;(define int (cons-stream 1 (cons-stream
 ;                           (+ (head ones) (head integers))
  ;                          (+ (head (tail ones)) (head (tail integers))))))            


;Ex 3.53
(define s (cons-stream 1 (add-streams s s)))
;This a stream of the powers of 2: 1 2 4 8 16 32...

;Ex 3.54
(define (mul-streams s1 s2) (stream-map-gen * s1 s2))

(define factorials (cons-stream
                    1
                    (mul-streams factorials (add-streams integers ones))))

(define (factorial n) (stream-ref factorials (- n 1)))

;Ex 3.55
(define (partial-sums s)
  (cons-stream
   0;(head s)
   (add-streams s (partial-sums s))))
   ;(partial-sums (add-streams s (tail s)))))
(define sums (cons-stream 0 (add-streams integers sums)))

;Ex 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (head s1))
               (s2car (head s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (tail s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (tail s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge
                    (tail s1)
                    (tail s2)))))))))

;Integers with no prime factors other than 2, 3 and 5
(define int-2-3-5 (cons-stream
                   1
                   (merge
                    (stream-scale int-2-3-5 2)
                    (merge (stream-scale int-2-3-5 3) (stream-scale int-2-3-5 5)))))

;Ex 3.57
;To compute the n-fibonacci using the following definition:
(define fibs
  (cons-stream
   0
   (cons-stream
    1
    (add-streams fibs (tail fibs)))))
;We need to do n-1 additions
;This is because to compute the n term, we need the previous term, which has already been momoized, and then we do the addition.
;Since the 0 and 1 terms are there, we need n - 1 additions for the nth fibonacci.

;If we weren't using memoization, to get the previous term, we would need to redo the computation for the previous term
;That will grow just like a recursive fibonacci implementation 
;Computing fibonacci will behave like a binary tree, where the number of additions is: the number of nodes - 1 - (leavenodes)
;For a binary tree, that will be 2^n - 1 - 1 - 2^n/2 = 2^n/2 - 2

;Ex 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;This will return stream whose elements are the fraction of dividing num by den, in base radix
;In otherwords, it's the inexact representation of (/ num den)


;Ex 3.59
(define (integrate-series series-stream)
  ;(define (iter i stream)
   ; (cons-stream
    ; (/ (head stream) i)
     ;(iter (+ i 1) (tail stream))))
  ;(iter 1 series-stream)
  (stream-map-gen / series-stream integers))

(define e^x (cons-stream 1 (integrate-series e^x)))
  ;(cons-stream 1 (cons-stream 1 (cons-stream (/ 1 2) (cons-stream (/ 1 6) the-empty-stream)))))
(define cosine-series (cons-stream 1 (stream-scale (integrate-series sine-series) -1)))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))


;Ex 3.60
(define (mul-series s1 s2)
  (cons-stream (* (head s1) (head s2))
               (add-streams
                (stream-scale (tail s1) (head s2))
                (mul-series s1 (tail s2)))))

;Ex 3.61
(define (invert-unit-series series)
  (define (invert)
    (define x
          (cons-stream
           1
           (mul-series
            (stream-scale (tail series) -1)
            x)))
    x)
  (if (not (= (head series) 1))
      (error "Series does not start with 1")
      (invert)))

;Ex 3.62
;(1 + Sr)*X = 1
;X + Sr*X = 1
;X = 1 - Sr*X

;S = C + Sr
;S/C = 1 + Sr/C
;(1+Sr/C)*X=1
;X + Sr*X/C = 1
;X = 1 - Sr*X/C
;Then:
;(C+Sr)*X = C
;(C+Sr)*(X/C) = 1 (Here assuming we already got X)
(define (div-series s1 s2)
  (define (divide)
    (mul-series
       s1
       (invert-unit-series s2)))
  (if (= (head s2) 0)
      (error "Denominator series has 0 constant term")
       (mul-series
        s1
        (stream-scale
         (invert-unit-series (stream-scale s2 (/ 1 (head s2))))
         (/ 1 (head s2))))))
      ;(divide)))


(define (approximate-series series x terms)
  (if (<= terms 0)
      0
      (+
       (head series)
       (approximate-series
        (stream-scale (tail series) x)
        x (- terms 1)))))

(define tan-series (div-series sine-series cosine-series))

;### Sec 3.5.3 ###


(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map
                  (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n) (stream-map-gen - (pi-summands (+ n 2)))))
(define pi-stream
  (stream-scale (partial-sums (pi-summands 1)) 4))

;Ex 3.64

(define (stream-limit stream tolerance)
  (cond
    ((or (empty-stream? stream) (empty-stream? (tail stream))) stream)
    ((< (abs (- (head stream) (head (tail stream)))) tolerance)
     (head (tail stream)))
    (else (stream-limit (tail stream) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


;Ex 3.65
(define (alt-reci-integers n)
  (cons-stream (/ 1 n)
               (stream-map-gen - (alt-reci-integers (+ n 1)))))
(define ln2-stream
  (partial-sums (alt-reci-integers 1)))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (* (- s2 s1) (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (tail s)))))

(define accelerated-ln2 (euler-transform ln2-stream))

;This is recursive acceleration
;We accelerte a sequence, then accelerate the accelerated sequence, and so on.
(define (make-tableau stream transform)
  (cons-stream stream (make-tableau (transform stream) transform)))

(define (accelerated-sequence stream transform)
  (stream-map head (make-tableau stream transform)))
(define ln2-double-acc (accelerated-sequence ln2-stream euler-transform))
;The original ln2-stream takes 200 elements to get 0.69, and doesn't reach 0.6931 even after 1000 elements (0.6926)
;The euler transform accelerated version needs 14 elements to get to 0.6931
;The recursively accelerated version reaches 0.6931 in the 4th element


;#### Representing infinite streams of pairs ####



(define (interleave s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (interleave s2 (tail s1)))))

(define (pairs s t)
  (cons-stream
   (list (head s) (head t))
   (interleave
    (stream-map (lambda (x) (list (head s) x))
                (tail t))
    (pairs (tail s) (tail t)))))

(define int-pairs (pairs integers integers))

;We first do the base base (k,k), for k = 1 that's (1,1)
;So we have an interleave that has a stream s1(1) from (k ,k ... n) pairs and another stream s2(1) that is itself a pairs stream
;We do the first of s1 stream and that's (k,j) for k=1 and j=2: (1,2) 
;We now generate the s2(1) stream which is the pairs of (k+1 ... n,k+1 ... n)
; Generate the base (k+1,k+1), for k=2 that is (2,2)
;Do s1(1) stream for k = 1, and j = 3 that's (1,3)
 ;Second interleave of the s2(1) stream,  with s1(2) stream that represents (k+1,k+2 ... n)
 ;Do S2(1) stream of for (k+1,j) for j=3 (2,3)
;Do s1(1) stream for j = 4 ,          that's (1,4)
   ;Generate the pairs of (k+2 ... n,k+2 ... n)
   ;Base (k+2,k+2) = (3,3)
;Do s1(1) stream for j = 5,           that's (1,5)
 ;Do s1(2) stream for (k+1,j) for j=4 (2,4)


;pairs of (1,k) are seen every other pair
;pairs of (2,k) are seen every 3 pairs
;pairs of (3,k) are seen every 7 pairs
;pairs of (4,k) are seen every 15 pairs

;pairs of (n,k) are seen every  r(n) pairs where r(n) = 2*r(n-1) + 1 and r(1) = 1


;pair of (k,k)   index = r(k)-1 or 2^k - 2
;pair of (n,k>n) index = 2^n*(k-n)+ 2^(n-1) - 2


;So the pair (1,100) is at index 2^1*(100-1)+2^(0)-2
;The pair (99,100) is at index 2^(99)*(100-99)+2^(98)-2 = 2^99+2^98 - 2


;Ex 3.67
(define (all-pairs s t)
  (cons-stream
   (list (head s) (head t))
   (interleave
    (stream-map (lambda (x) (list (head s) x))
                (tail t))
    (interleave
     (stream-map (lambda (x) (list x (head s)))
                (tail s))
     (all-pairs (tail s) (tail t))))))
    ;(all-pairs (tail s) t))));(tail t)))))

;Ex 3.68
(define (pairs2 s t)
  (interleave
   (stream-map (lambda (x) (list (head s) x))
               t)
   (pairs2 (tail s) (tail t))))
;This will go into an infinite loop because interleave is going to evaluate the call to pairs2, which will result in another call and so on.
;Interleave is not cons-stream, it won't delay the evaluation of its second argument
;Ex 3.69
(define (triples s t u)
  #|(define (pair-up s-loop pairs)
    ;(cons-stream
     ;(list (head s) (head t) (head u))
     (let ((first (head s-loop))
           (pair (head pairs)))
       (if (<= first (car pair))
           (cons-stream
            (cons first pair)
            (interleave
             (pair-up s (tail pairs))
             (pair-up (tail s-loop) pairs)))
           (pair-up (tail s-loop) pairs))))|#
  (define (pair-up s-loop pairs)
    (if
     (> (head s-loop) (car (head pairs)))
     (pair-up s-loop (tail pairs))
     (cons-stream
      (cons (head s-loop) (head pairs))
      (interleave
       (stream-map (lambda (x) (cons (head s-loop) x))
                   (stream-filter
                    (lambda (p) (<= (head s-loop) (car p)))
                    (tail pairs)))
       (pair-up (tail s-loop) pairs)))))
  (pair-up s (pairs t u)))
 
  #|This procedure computes (pairs t u) every recursive call which is redundant
    (cons-stream
     (list (head s) (head t) (head u))
     (interleave
      ;(stream-map
      ;(lambda (x) (list (head s) (head t) x))
      ;(tail u))
      (stream-map
       (lambda (x) (cons (head s) x))
       (pairs t (tail u)))
      (triples (tail s) (tail t) (tail u)))))|#

(define pythagorean-triples
  (stream-filter
   (lambda (p) (= (+ (square (car p)) (square (cadr p))) (square (caddr p))))
   (triples integers integers integers)))


;Ex 3.70

(define (merge-weighted s1 s2 weight)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (let ((s1car (head s1))
               (s2car (head s2))
               (s1pair-weight (weight (head s1)))
               (s2pair-weight (weight (head s2))))
           (cond ((< s1pair-weight s2pair-weight)
                  (cons-stream s1car (merge-weighted
                                      (tail s1) s2 weight)))
                 ((> s1pair-weight s2pair-weight)
                  (cons-stream s2car (merge-weighted
                                      s1 (tail s2) weight)))
                 (else
                  (cons-stream s1car
                               (cons-stream
                                s2car
                                (merge-weighted
                                 (tail s1)
                                 (tail s2)
                                 weight)))))))))

(define (weighted-pairs s1 s2 weight)
  (if (empty-stream? s1) s1
      (cons-stream
       (list (head s1) (head s2))
       (merge-weighted
        (stream-map (lambda (x) (list (head s1) x)) (tail s2))
        (weighted-pairs (tail s1) (tail s2) weight)
        weight))))

(define pairs-sum-ordered (weighted-pairs integers integers (lambda (pair) (apply + pair))))

(define not-divisible-by-2-3-5
  (stream-filter
   (lambda (n) (not (or (= (remainder n 2) 0) (= (remainder n 3) 0) (= (remainder n 5) 0))))
   integers))
(define pairs-of-2-3-5
  (weighted-pairs
   not-divisible-by-2-3-5
   not-divisible-by-2-3-5
   (lambda (pair) (+ (* 2 (car pair)) (* 3 (cadr pair)) (* 5 (car pair) (cadr pair))))))
                                       
;Ex 3.71
;Ramanujan
(define (cubic-sum x y) (+ (* x x x) (* y y y)))
(define (ramanujan-numbers)

  (define ordered-cubes (weighted-pairs integers integers cubic-sum))
  (define (search stream)
    (let ((sum (apply cubic-sum (head stream))))
    (if (= sum (apply cubic-sum (head (tail stream))))
        (cons-stream
         (list sum (head stream) (head (tail stream)))
         (search (tail (tail stream))))
        (search (tail stream)))))
  (search ordered-cubes))

;Ex 3.72
(define (square-sum pair)
  ((lambda (x y) (+ (* x x) (* y y)))
   (car pair) (cadr pair)))

(define (three-squares-numbers)
  (define ordered-squares (weighted-pairs integers integers square-sum))
  (define (search stream)
    (let ((sum (square-sum (head stream))))
      (if (= sum
           (square-sum (head (tail stream)))
           (square-sum (head (tail (tail stream)))))
          (cons-stream
           (list sum (head stream) (head (tail stream)) (head (tail (tail stream))))
           (search (tail (tail (tail stream)))))
          (search (tail stream)))))
  (search ordered-squares))


;Hanoi Stream
;Berkeley CS61A Week 11 Homework
(define (stream-append s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (stream-append (tail s1) s2))))
(define (make-hanoi-infinite prev-stream n)
  ;(define hanoi-infinite
   ; (cons-stream
    ; n
     ;(cons-stream (+ ;(stream-ref hanoi-infinite n) 1)
      ;            hanoi-infinite)));(make-hanoi-infinite (+ (* n 2) 0)))))
  ;hanoi-infinite)
  ;(1 2 1)
  ;(define hanoi-n
   ; (stream-append prev-stream
    ;               (cons-stream n
     ;                           prev-stream)))
  (define hanoi-n
    (cons-stream
     n
     (stream-append
      prev-stream
      (make-hanoi-infinite (stream-append prev-stream (cons-stream n prev-stream)) (+ n 1)))))
  hanoi-n)

;[Solutions from solutions manual https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/week13]
;Another clever way is to use interleave:
(define (stream-n n)
  (cons-stream n (stream-n n)))
(define (hanoi-stream n)
  (cons-stream
   n
   (interleave
    (hanoi-stream (+ n 1))
    (stream-n n))))
;Another even more celever way
(define hanoi-infinite
  (cons-stream 1
               (interleave
                (stream-map inc hanoi-infinite)
                ones)))








