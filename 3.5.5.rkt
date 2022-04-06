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
;############## Section 3.5.4: Streams and Delayed Evaluation ##############

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt)
                    int))))
  int)

(define (solve f y0 dt)
  (let ((y  '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    ;(define y (integral (delay dy) y0 dt))
    ;(define dy (stream-map f y))
    y))

;Ex 3.77
(define (integral-dir delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral-dir (head integrand)
                   (+ (* dt (tail integrand))
                      initial-value)
                   dt)))))
;Ex 3.78 && Ex 3.79
;This models the differential equation:
;ddy/ddt - a(dy/dt) - by = 0
;
(define (solve-2nd a b y0 dy0 dt)
  (solve-2nd-gen (lambda (dy y)
                   (+
                    (* dy a)
                    (* y b)))
                 ;  (add-streams
                  ;  (scale-stream dy a)
                   ; (scale-stream y b)))
                 y0 dy0 dt))
(define (solve-2nd-gen f y0 dy0 dt)
  (let ((y   '*unassigned*)
        (dy  '*unassigned*)
        (ddy '*unassigned*))
    (set! y  (integral (delay dy) y0 dt))
    (set! dy (integral (delay ddy) dy0 dt))
    (set! ddy
          (stream-map-gen f dy y))
          ;(f dy y))
          ;(add-streams
           ;(scale-stream dy a)
           ;(scale-stream y b)))
    y))
;Ex 3.80
(define (RLC R L C dt)
  (lambda (vc0 il0)
    (let ((vc   '*unassigned*)
          (il   '*unassigned*)
          (dvc  '*unassigned*)
          (dil  '*unassigned*))
      (set! il (integral (delay dil) il0 dt))
      (set! vc (integral (delay dvc) vc0 dt))
      (set! dvc (scale-stream il (/ -1 C)))
      (set! dil (add-streams
                (scale-stream il (/ (- R) L))
                (scale-stream vc (/ 1 L))))
      (cons vc il))))

;##### Section 3.5.5 #####
(define (rand-update x)
  (modulo (+ (* 214013 x) 2531011) (expt 2 8)))
(define initial-random 5)
(define (generate-random-numbers initial)
  (cons-stream initial
               (stream-map rand-update random-numbers)))
(define random-numbers
  (cons-stream initial-random
               (stream-map rand-update random-numbers)))
;Ex 3.81

(define (random-numbers-generator requests-stream)

  (define (apply-procedure p)
    (cond ((eq? 'generate);(caar p) 'generate)
           (rand-update (cdr p)))
          ((eq? (caar p) 'reset)
           (cadar p))
          (else p)))
  ;(define (get-random requests)
   ; (define numbers
    ;  (cons-stream
     ;  initial-random
      ; (stream-map
       ; (head (stream-map get-procedure requests-stream))
        ;(get-random numbers)
                 
  (define random-numbers
    (cons-stream
     initial-random
     (stream-map
      apply-procedure
      (stream-map-gen cons requests-stream random-numbers))))
  random-numbers)


;Ex 3.82
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (tail experiment-stream) passed failed)))
  (if (head experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (random-stream-range low high)
  (define randoms
    (cons-stream
     (random-in-range low high)
     (random-stream-range low high)))
     ;(stream-map (lambda (x) (random-in-range low high)) randoms)))
  randoms)
(define (estimate-integral p x1 x2 y1 y2)
  (define area (* (- x2 x1) (- y2 y1)))
  (define predicate-stream
    (stream-map-gen
     p
     (random-stream-range x1 x2)
     (random-stream-range y1 y2)))
  (stream-map
   (lambda (x) (* x area))
   (monte-carlo predicate-stream 0 0)))
     
                               
  










