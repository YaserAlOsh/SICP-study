#lang sicp
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
(define (display-stream-n s max)
  (if (or
       (empty-stream? s)
       (= max 0))
      'done
      (begin
        (display (head s))
        (newline)
        (display-stream-n (tail s) (- max 1)))))
(define (list->stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list)
                   (list->stream (cdr list)))))
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
                 
 
(define (stream-map-gen proc . argstreams)
  (if (empty-stream? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map head argstreams))
       (apply stream-map-gen
              (cons proc (map tail argstreams))))))

(define (add-streams s1 s2) (stream-map-gen + s1 s2))
(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))
;## Streams as signals ##
(define (integral integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

;Ex 3.73
(define (RC R C dt)
  (lambda (current v0)
    (add-streams
     (scale-stream current R)
     (integral
      (scale-stream current (/ 1 C))
      v0 dt))))
(define RC1 (RC 5 1 0.5))
(define currents (cons-stream 1 (scale-stream currents (exp (- 1)))));(cons-stream (exp (head currents)) (tail currents))))


;Ex 3.74
(define (sign x) (if (< x 0) -1 1))
(define (sign-change-detector f i)
  (cond ((= (sign i) (sign f)) 0)
        (else (sign f))))
(define sense-data (list->stream '(5 -8 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))


(define zero-crossings
  (stream-map-gen
   sign-change-detector
   sense-data
   (cons-stream 0 sense-data)))
;Ex 3.75
(define (make-zero-crossings input-stream prev-avg last-value)
  (let ((avpt (/ (+ (head input-stream) last-value)
                 2)))
    (cons-stream
     (sign-change-detector avpt prev-avg)
     (make-zero-crossings (tail input-stream) avpt (head input-stream)))))
;Ex 3.76
;A smoothed version that doesn't need a last value:
(define (average x y) (/ (+ x y) 2))
;Smooth:
(define (average-stream s) 
  (stream-map-gen average s (tail s)))

(define (smoothed-zero-crossings s)
  (define averaged (cons-stream (head s) (average-stream s)))
  (stream-map-gen
   sign-change-detector
   (tail averaged)
   averaged))
   
(define (make-zero-crossings-smoothed input-stream  last-value)
  (define (compare stream last-value)
    (cons-stream
     (sign-change-detector (head stream) last-value)
     (compare (tail stream) (head stream))))
  (compare (average-stream input-stream) last-value))


;######################## Section 3.5.4 Streams and Delayed Evaluation

