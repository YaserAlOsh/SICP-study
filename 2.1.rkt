#lang sicp

(define (square x) (* x x))

(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))

;Ex:2.1

(define (make-rat n d) (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
                          (cons  (/ n g) (/ d g))))
(define (make-rat2 n d)
      (cons (if (< d 0) (* n -1) n) (if (< d 0) (* d -1) d)))

(define (numer x) (car x))
(define (denom y) (cdr y))

(define (add-rat a b)
  (make-rat (+ (* (numer a) (denom b))
                (* (numer b) (denom a)))
             (* (denom a) (denom b))))
(define (sub-rat a b)
  (make-rat (- (* (numer a) (denom b))
                (* (numer b) (denom a)))
             (* (denom a) (denom b))))

(define (mul-rat a b)
  (make-rat (* (numer a) (numer b))
             (* (denom a) (denom b))))

(define (div-rat a b)
  (make-rat (* (numer a) (denom b))
             (* (numer b) (denom a))))

(define (equal-rat? a b)
  (= ((* (numer a) (denom b))
      (* (numer b) (denom a)))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;Ex 2.2

(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (point x y) (cons x y))
(define (x-coord p) (car p))
(define (y-coord p) (cdr p))

(define (add-points a b)
  (point (+ (x-coord a) (x-coord b)) (+ (y-coord a) (y-coord b))))

(define (divide-point p divisor)
  (point (/ (x-coord p) divisor) (/ (y-coord p) divisor)))
(define (mult-point p multiple)
  (point (* (x-coord p) multiple) (* (y-coord p) multiple)))

(define (midpoint-segment s)
  (mult-point (add-points (start-segment s) (end-segment s)) 0.5))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-coord p))
  (display ",")
  (display (y-coord p))
  (display ")"))

;Ex 2.3

;First representation
;(define (rectangle vside hside) ;Vertical segment, horizontal segment
;  (cons vside hside))
;(define (horiz-side r) (car r))
;(define (vert-side r)  (cdr r))

(define (points-dist a b)
  (sqrt (+ (square (- (x-coord b) (x-coord a))) 
           (square (- (y-coord b) (y-coord a))))))

(define (segment-length s)
  (points-dist (start-segment s) (end-segment s)))
  
(define (rect-perimeter r)
  ( + (* 2 (segment-length (horiz-side r)))
      (* 2 (segment-length (vert-side r)))))
(define (rect-area r)
  (* (segment-length (horiz-side r)) (segment-length (vert-side r))))



(define (horiz-proj-seg p1 p2) (make-segment p1 (point (x-coord p2) (y-coord p1))))
(define (vert-proj-seg p1 p2) (make-segment p1 (point (x-coord p1) (y-coord p2))))

;New representation
(define (rectangle p1 p3) ;Bottom left point, Top right point
  (cons p1 p3))

(define (horiz-side r) (horiz-proj-seg (car r) (cdr r)))
(define (vert-side r) (vert-proj-seg (car r) (cdr r)))

;Ex 2.5

(define (exp b n)
  (define (exp-iter a n)
  (if (= n 1) a
      (exp-iter (* a b) (- n 1))))
  (exp-iter b n))

(define (log-b b a) (/ (log a) (log b)))


(define (ab-pair a b) (* (exp 2 a) (exp 3 b)))

(define (power-of-n a n)
  (if (= (remainder a n) 0) (power-of-n (/ a n) n)
      (= a 1)))

;Brute Force method
;0(n)
(define (get-a p)
  (define (iter n)
    (cond ((> n p) 0)
          (else (if (and (= (remainder p n) 0) (power-of-n (/ p n) 3)) (log-b 2 n)
                    (iter (+ n 1))))))
  (iter 1))

(define (get-b p)
  (log-b 3 (/ p (exp 2 (get-a p)))))

;Faster Brute force
;O(Log(n))
(define (first-non-0-remainder-of-exp p b)
  (define (iter check-exp)
    (if (= (remainder p (exp b check-exp)) 0) (iter (+ check-exp 1))
        (- check-exp 1)))
  (iter 1))

(define (pair-car p) (first-non-0-remainder-of-exp p 2))

(define (pair-cdr p) (first-non-0-remainder-of-exp p 3))

;Another way is to repeatedly divide the number by 2 then stop when the result is no longer even, this will give pair-car

;Ex 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero) > (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
; > (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
; > (lambda (f) (lambda (x) (f x))) -> one
(define one (lambda (f) (lambda (x) (f x))))
; > (add-1 one)
; > (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x)))))
; > (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; > (lambda (f) (lambda (x) (f (f x)))) -> two
(define two (lambda (f) (lambda (x) (f (f x)))))

; so for number n, the function f has to applied n times, i.e. (f (f (f x))) is for three, (f (f (f (f (f ... (f x)..))))
; To add two numbers, we apply the function f m times where m = (sum of the two numbers)

; > (add one one) -> (lambda (f) (lambda (x) ( (one f) ((one f) x))))
; - ((one f) x) -> (f x) 
; > (lambda (f) (lambda (x) ( (one f) (f x))))
; - ((one f) (f x)) -> (f (f x))
; > (lambda (f) (lambda (x) (f (f x)))) -> two !
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))


;Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  ;Fix intervals that span zero (Ex.2.10)
  (if (<= (* (upper-bound y) (lower-bound y)) 0)
      (error "Dividing by an interval that spans 0" y)
      (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))
;Ex 2.7
(define (make-interval a b) (cons a b))

(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))
;Ex 2.8
;To subtract intervals, the lower bound should be the smallest possible result, while the upper bound should be the largest
;Subtracting the upper bound of the second from the lower bound of the first interval will give the smallest possible bound (even for negative values)
;Likewise for the uppear bound, we subtract the lower bound of the second from the upper bound of the first.
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (print-interval i)
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i)))

;Ex 2.9

(define (width i) (* 0.5 (- (upper-bound i) (lower-bound i))))

;(width (add-interval (make-interval a b) (make-interval c d)))
; > (width (make-interval (+ a c) (+ b d)))
; > (* 0.5 (- (+ b d) (+ a c)))
; > (- (* 0.5 (+ b d)) (* 0.5 (+ a c))) [Communicative arithmatics rules]
; > (0.5 * ((b+d) - (a+c)))> (0.5 * (b - a + d - c)) > (width (make-interval a b)) + (width (make-interval c d))
;So the width of the sum of two intervals is the sum of the widths of the intervals
;For subtraction:
; > (width (add-interval (make-interval a b) (make-interval (- c) (- d))))
; > (* 0.5 (- (- b d) (- a c)))
; > (0.5 * ((b - d) - (a - c))) > (0.5 * (b-a + c-d)) > (0.5 * (b-a + (-d + c)) > (width (make-interval a b)) + (width (make-interval (- c) (- d)))
;So the width of the difference is also the sum of the widths
;For multiplcation, consider the case where the product of the upper bounds is always the largest
; > (width (mul-interval (make-interval a b) (make-interval c d)))
; > (width (make-interval (* a c) (* b d)))
; > (width (* 0.5 (- (* b d) (* a c))))
; > (0.5 * (b * d) - (a * c)) ; Which cannot be a function of the width of the two intervals

;Ex 2.11
;Since in my implementation, lower-bound is always less than upper-bound,
;If both upper and lower bounds of both intervals are the same signs,
;(a,b) * (c,d) > (a*c,b*d)
;If upper bound of i1 is positive and lower is negative, and i2 is all positive
; (a,b) * (c,d) -> (a*d,b*d)
; example (-6,8) (3 4) -> (-6*4,8*4)
;If upper bound of i1 is positive and both lower and upper bound of i2 are negative..
;(a,b) *(c,d) > (b*c,a*d)
;example: (6,8)  (-4,-3) > (8*-4,6*-3)
;   If lower bound of i1 is negative
;   (a,b) *(c,d) > (b*c,a*c)
;   example: (-6,8) (-4,-3) > (8*-4,-6*-4)
;
;  if lower bound of i2 is negative while upper bound is positive..
;  (a,b) * (c,d) > (b*c,b*d)
;  example: (6,8) (-4,3) > (8*-4,8*3)
;    If lower bound of i1 is negative
;    (a,b) *(c,d) > (min(a*d,b*c),max(a*c,b*d))
;    example: (-6,8) (-4,4) > (8*-4,8*4) (-32,32)
;    example: (-6,8) (-6,4) > (8*-6,-6*6) (32, 32)

;If upper bound of i1 is negative, then lower bound should be negative as well
;(a,b) *(c,d) > (b*d,a*d)
;example: (-12,-8) (3,4) > (-12*4و-8*3)
;Or we just call it in reverse:
; (-12,-8) * (3,4) = (3,4) * (-12,-8) > (4*-12,3*-8) (from the rule above)
;if lower bound of i2 is negative, we call it in reverse as well
; example :(-12,-8) * (-3,4) = (-3,4) * (-12,-8) > (4*-12,-3*-8)
;
;
(define (sign x) (> x 0))

(define (mul-interval x y)
  (let ((s1 (sign (lower-bound x))) (s2 (sign (upper-bound x)))
       (s3 (sign (lower-bound y))) (s4 (sign (upper-bound y)))
       (a (lower-bound x)) (b (upper-bound x))
       (c (lower-bound y)) (d (upper-bound y)))
       (cond ((and s1 s2 s3 s4) (make-interval (* a c) (* b d)));All positive
             ((and (not s1) (not s2) (not s3) (not s4)) (make-interval (* a c) (* b d)));All negative
             ((and (not s1) s2 s3 s4) (make-interval (* a d) (* b d)));First lower is negative, all else positive
             ((and s1 s2 (not s3) (not s4)) (make-interval (* b c) (* a d)));First upper is positive, second both are negative
             ((and (not s1) s2 (not s3) (not s4)) (make-interval (* b c) (* a c)));First lower negative, upper positive, second both negative
             ((and s1 s2 (not s3) s4) (make-interval (* b c) (* b d)));First upper and lower are positive, second lower negative and upper positive
             ;First lower is negative, upper positive, and the same for the second, this case requires multiple multiplication
             ((and (not s1) s2 (not s3) s4 (make-interval (min (* a d) (* b c)) (max (* a c) (* b d)))))
             ;First upper is negative(so lower is negative also), reverse the multiplication
             ((not s2) (mul-interval y x)))
    ))

(define (eql-interval? a b) 
   (and (= (upper-bound a) (upper-bound b)) 
        (= (lower-bound a) (lower-bound b)))) 
  
;; Fails if the new mult doesn't return the same answer as the old 
;; naive mult. 
(define (ensure-mult-works aH aL bH bL) 
   (let ((a (make-interval aL aH)) 
         (b (make-interval bL bH))) 
   (if (eql-interval? (old-mul-interval a b) 
                      (mul-interval a b)) 
       true 
       (error "new mult returns different value!"  
              a  
              b  
              (old-mul-interval a b) 
              (mul-interval a b))))) 
  


;Ex 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-interval (- c w) (+ c w))))

(define (percent i) (/ (width i) (center i)))

;Ex 2.13
;Assume a,b,c and d are all positive, and a < b , c < d
; > (percent (make-interval a b))
; > (/ (* 0.5 (- b a)) (* 0.5 (+ a b)))
; Algebraic manipulation:
; > (b-a)/(a+b)
; > (percent (make-interval c d))
; > (c-d)/(c+d)
;
; > (percent (mul-interval (make-interval a b) (make-interval c d)))
; > (percent (make-interval (* a c) (* b d)))
; > (/ (width (make-interval (* a c) (* b d))) (center (make-interval (* a c) (* b d)))
;From above:
; > (/ (0.5 * (b * d) - (a * c)) (0.5 * (a * c) + (b * d)))
; > ((b * d) - (a * c)) / ((a * c) + b * d) > (bd -ac)/(ac + bd) > 1/(ac/bd + 1) - 1/(bd/ac + 1)
;
; (b-a)/(a+b) + (c-d)/(c+d) > ((b-a)(c+d) + (c-d)(a+b)) / (a+b)(c+d) > (bc + bd - ac - ad) +(ca + cb - da -db) /(a+b)(c+d)
; > 2bc - 2ad /(a+b)(c+d) > 2(bc-ad)/(ac + ad + bc + bd)
; 2 / (a/b + ad/bc + 1 + d/c) - 2/(c/d + 1 + bc/ad + b/a)
; All useless
;
;
; p = w/c
; a = [ca * (1-pa), ca * (1+pa)]
; b = [cb * (1-pb), cb * (1+pb)]

; a * b = [ca*(1-pa)*cb*(1-pb),ca*(1+pa)*cb*(1+pb)]
; Percent[a*b] = (Ca*(1+pa)*Cb*(1+pb) - Ca*(1-pa)*Cb*(1-pb)) / (Ca*(1-pa)*Cb*(1-pb) + Ca*(1+pa)*Cb*(1+pb))
;  Ca*Cb((1+pa)(1+pb) - (1-pa)(1-pb)) = Ca*Cb((1+Pa+Pb+PaPb) - (1-Pa-Pb+PaPb)) =   Ca*Cb(2Pa+2Pb)   =  2Pa+2Pb    Pa+Pb
;  ----------------------------------   --------------------------------------   ------------------   --------- = ----- 
;  Ca*Cb((1+pa)(1+pb) + (1-pa)(1-pb)) = Ca*Cb((1+Pa+Pb+PaPb) + (1-Pa-Pb+PaPb)) =   Ca*Cb(2 + 2PaPb) =  2+2PaPb    1+PaPb
; For small percentages, PaPb is almost 0, so the fraction reduces to Pa+Pb

;Ex 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))




