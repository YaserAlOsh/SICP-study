#lang sicp



;Queues Implementation
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "")
          (car front-ptr)))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else ;#f))))
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))
    (define (delete-queue!)
      (set! front-ptr (cdr front-ptr)))
    (define (print-queue)
      (display front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'insert!) insert-queue!)
            ((eq? m 'delete!) (delete-queue!))
            ((eq? m 'front) (front-queue))
            ((eq? m 'print) (print-queue))
            ((eq? m 'empty?) (empty-queue?))
            (else
             (error "No dispatch for message" m))))
            
    dispatch))

(define (insert-queue! q item)
  ((q 'insert!) item))
(define (delete-queue! q)
  (q 'delete!))
(define (empty-queue? q)
  (q 'empty?))
(define (front-queue q)
  (q 'front))




;Logic Circuit Simulation


(define (logical-not s)
  (cond ((= s 1) 0)
        ((= s 0) 1)
        (else (error "Invalid logical-not signal"))))
(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        (else 0)))
(define (logical-or s1 s2)
  (cond ((= s1 1) 1)
        ((= s2 1) 1)
        (else 0)))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value
           (logical-not (get-signal input))))
      (after-delay
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
'ok)


(define (or-gate s1 s2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal s1) (get-signal s2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! s1 or-action-procedure)
  (add-action! s2 or-action-procedure)
  'ok)

;Ex 3.29
;The delay of this or compound logic circuit is 2 inverter-delay + 1 and-gate-delay
(define (comp-or-gate s1 s2 out)
  (let ((s3 (make-wire)) (s4 (make-wire)) (s5 (make-wire)))
    (inverter s1 s3)
    (inverter s2 s4)
    (and-gate s3 s4 s5)
    (inverter s5 out)))


;Ex 3.30

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
'ok))

(define (full-adder a b cin cout sum)
  (let ((w (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b cin w c1)
    (half-adder a w sum c2)
    (or-gate c1 c2 cout)))

;Ripple Carry Adder
;Ex 3.30
(define (ripple-carry-adder a-bits b-bits sum c)
  (define (iter ak bk sk carry)
    (cond ((null? ak)
           'done)
          (else
           (let ((cn (make-wire)))
             (full-adder (car ak) (car bk)
                         carry cn (car sk))
             (iter (cdr ak) (cdr bk) (cdr sk) cn)))))
  (iter a-bits b-bits sum c))
;In the ripple-carry-adder logic circuit:
;Each Half-Adder circuit has (in total) two and-gate-delay, one or-gate-delay and one inverter-delay
;Since the and-gate and inverter run simultaneously with the or-gate, the result delay would be
;max(or-gate-delay, and-gate-delay+inverter-delay) + and-gate-delay
;Half-Adder(carry-in=>carry-out) = and-gate-delay

;Each full-adder circuit:
;full-adder(carry-in=>carry-out) = max(or-gate-delay, and-gate-delay+inverter-delay) + or-gate-delay + and-gate-delay
;full-adder(sum-in=>sum-out) = 2*max(or-gate-delay, and-gate-delay+inverter-delay) + 2*and-gate-delay


;N-ripple-carry-adder circuit takes n Full-Adder circuit, so:
;Carry-propogation: n * (max(or-gate-delay, and-gate-delay+inverter-delay) + or-gate-delay + and-gate-delay)
;Sum-out:           n * (2*max(or-gate-delay, and-gate-delay+inverter-delay) + 2*and-gate-delay)



;The simulation
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc)
      )
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
dispatch))

(define (call-each procedures)
  (if (null? procedures) 'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))


(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))




;Implementing the agenda

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))


(define (make-agenda) (list 0))
(define (current-time agenda)
  (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda)
  (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue!
         (segment-queue (car segments)) action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))


(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))


(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda) 'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))



;Sample Simulation
(define (probe name wire)
  (add-action!
   wire
   (lambda ()
     (newline)
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display " New-value = ")
     (display (get-signal wire))
     (newline))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)


(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(define (binary-digits n)
  (+ 1 (floor (log n 2))))

(define (decimal-to-binary n bit)
  (define (append-zeros z list)
    (if (<= z 0) list (cons 0 (append-zeros (- z 1) list))))
  (define (make-list x)
    (if (= x 0) (append-zeros (- bit (binary-digits n)) '())
        (cons (remainder x 2) (make-list (floor (/ x 2))))))
  (make-list n))

(define (binary-to-decimal bits)
  (define (iter base b-list)
    (if (null? b-list) 0
        (+ (* base (car b-list))
           (iter (* 2 base) (cdr b-list)))))
  (iter 1 bits))

      ;(cons (remainder x 2)
       ;     (make-list (floor (/ x 2))))))
  ;(append-zeros (- bit (binary-digits n))
   ;             (make-list n)))
  
(define (set-number-to-wires number wires)
  (define (iter p-wires bits)
    (if (not (null? bits))
        (begin
          (set-signal! (car p-wires) (car bits))
          (iter (cdr p-wires) (cdr bits)))))
  (iter wires (decimal-to-binary number (length wires))))

(define (make-wires count)
  (if (= count 0)'()
      (cons (make-wire)
            (make-wires (- count 1)))))

(define (add-with-ripple-carry a b )
  (let ((bits 16))
    (define bi-a (make-wires bits))
    (define bi-b (make-wires bits))
    (define bi-sum (make-wires bits))
    (ripple-carry-adder bi-a bi-b bi-sum (make-wire))
    (set-number-to-wires a bi-a)
    (set-number-to-wires b bi-b)
    (propagate)
    (binary-to-decimal (map get-signal bi-sum))))

;Ex 3.31
;We need to initialize the system to enforce the rules of the design by calling the action procedure as soon as it gets added.
;If we don't, the initial state won't resemble how we design our digital circuit system.

;In the particular example, the sum signal won't change after setting the input signal to 1 because the inverter won't be initialied to 1.

;Ex 3.32
;In the particular example mentioned, swapping the input of an and gate from 0,1 to 1,0 will, after an and-gate-delay, result in output 0 in the and gate
;If we had used a first in last out approach, then the output will be 1.
;That's because we will insert two actions to be done at time (and-gate-delay). The first will set the output to 1. The second will set it to 0
;If we use a queue, the first one will be executed before the second,
;But If we use an ordinary list, the second will be executed before the first and we will get a wrong result.

;In general, it's important to use a queue so that actions are performed in the order they were inserted, due to the nature of the designs involved



