#lang sicp



(#%require graphics/graphics)
(open-graphics)
(define width 800)
(define height 800)

(define vp (open-viewport "A Picture Language" width height))

(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

;need a wrapper function so that the graphics library works with my code...
(define (vector-to-posn v)
  (make-posn (car v) (cdr v)))


;Use racket graphics library
(define (compose-line a b) (line (vector-to-posn a) (vector-to-posn b)))

(#%require sicp-pict) 
;Ex 2.44
(define (upsplit painter n)
  (if (= n 0)
      painter
      (let ((smaller (upsplit painter (- n 1))))
        (below painter (beside smaller smaller)))))

;(define (beside p1 p2)
;  p1)
;(define (below p1 p2)
;  p1)

;Ex 2.45
(define (split first-comb second-comb)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split first-comb second-comb) painter (- n 1))))
          (first-comb painter (second-comb smaller smaller))))))



;Ex 2.46

(define (make-vector x y)
  (cons x y))
(define (xcor-vect v) (car v)) 
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2) (make-vector (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2) (make-vector (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s) (make-vector (* s  (xcor-vect v)) (* s (ycor-vect v))))

;Ex 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame-origin frame) (car frame))
(define (frame-edge1 frame)  (cadr frame))
(define (frame-edge2 frame)  (if (pair? (cddr frame)) (caddr frame) (cddr frame)))



;Ex 2.48

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (compose-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))
(define (make-segment v1 v2)
  (cons v1 v2))
(define (make-segmentxy x1 y1 x2 y2)
  (cons (make-vector x1 y1) (make-vector x2 y2)))
(define (start-segment sg) (car sg))
(define (end-segment sg)   (cdr sg))


;Ex 2.49

(define outline
  (segments->painter
   (list
    (make-segmentxy 0 0 1 0)
    (make-segmentxy 1 0 1 1)
    (make-segmentxy 1 1 0 1)
    (make-segmentxy 0 1 0 0))))

(define X
  (segments->painter
   (list
    (make-segmentxy 0 0 1 1)
    (make-segmentxy 1 0 0 1))))

(define diamond
  (segments->painter
   (list
    (make-segmentxy 0.5 0 1 0.5)
    (make-segmentxy 1 0.5 0.5 1)
    (make-segmentxy 0.5 1 0 0.5)
    (make-segmentxy 0 0.5 0.5 0))))




;Ex 2.50

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (frame-origin frame)
     (add-vect (scale-vect (frame-edge1 frame) (xcor-vect v))
               (scale-vect (frame-edge2 frame) (ycor-vect v))))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
  (painter (make-frame
            new-origin
            (sub-vect (m corner1) new-origin)
            (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vector 1.0 0.0) ;origin
                     (make-vector 0.0 0.0) ;new right edge
                     (make-vector 1.0 1.0) ;new left edge
                     ))
(define (flip-vert painter)
  (transform-painter painter
                     (make-vector 0.0 1.0) ;origin
                     (make-vector 1.0 1.0) ;new right edge
                     (make-vector 0.0 0.0) ;new left edge
                     ))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vector 1.0 0.0) ;origin
                     (make-vector 1.0 1.0) ;new right edge
                     (make-vector 0.0 0.0) ;new left edge
                     ))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vector 1.0 1.0) ;origin
                     (make-vector 0.0 1.0) ;new right edge
                     (make-vector 1.0 0.0) ;new left edge
                     ))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vector 0.0 1.0) ;origin
                     (make-vector 0.0 0.0) ;new right edge
                     (make-vector 1.0 1.0) ;new left edge
                     ))

;Ex 2.51

(define (beside painter1 painter2)
  (let ((split-point (make-vector 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vector 0.0 0.0)
            split-point
            (make-vector 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vector 1.0 0.0)
            (make-vector 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below p1 p2)
  (let ((p1bottoom
        (transform-painter p1
                           (make-vector 0.0 0.0)
                           (make-vector 1.0 0.0)
                           (make-vector 0.0 0.5)))
        (p2above
         (transform-painter p2
                            (make-vector 0.0 0.5)
                            (make-vector 1.0 0.5)
                            (make-vector 0.0 1.0))))
    (lambda (frame)
      (p1bottoom frame)
      (p2above frame))))

(define (below2 p1 p2)
  (rotate90 (beside (rotate270 p1) (rotate270 p2))))

;Defining more combinations

(define right-split (split beside below))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (upsplit painter (- n 1)))
            (right (right-split painter (- n 1))))

        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))

          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (upsplit painter (- n 1)))
            (right (right-split painter (- n 1))))

        (let ((corner (corner-split2 painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))
(define (corner-split3 painter n base-transform)
  (if (= n 0)
      (base-transform painter)
      (let ((up (upsplit painter (- n 1)))
            (right (right-split painter (- n 1))))

        (let ((corner (corner-split3 painter (- n 1) base-transform)))
          (beside (below painter up)
                  (below right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
; flip-vert rotate180 identity flip-horiz)
(define (square-limit-out painter n)
  (let ((flipped (flip-vert painter))
        (combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))




;Ex 2.52

(define arrow-painter
  (segments->painter
   (list
    (make-segmentxy 0.2 0.4 0.2 0.6); x1 y1 x2 y2
    (make-segmentxy 0.2 0.6 0.6 0.6)
    (make-segmentxy 0.6 0.6 0.6 0.7)
    (make-segmentxy 0.6 0.7 0.8 0.5)
    (make-segmentxy 0.8 0.5 0.6 0.3)
    (make-segmentxy 0.6 0.3 0.6 0.4)
    (make-segmentxy 0.6 0.4 0.2 0.4))))

(define wave-painter
  (segments->painter
   (list
    (make-segment (make-vector 0.5 0.4) ;;; leg triangle
                  (make-vector 0.6 0))
    (make-segment (make-vector 0.5 0.4)
                  (make-vector 0.4 0))
    (make-segment (make-vector 0.4 0.82)
                  (make-vector 0.42 0.8))
    (make-segment (make-vector 0.42 0.8)
                  (make-vector 0.46 0.8))
    (make-segment (make-vector 0.46 0.8)
                  (make-vector 0.47 0.82))
    (make-segment (make-vector 0.3 0)
                  (make-vector 0.35 0.4))
    (make-segment (make-vector 0.35 0.4)
                  (make-vector 0.3 0.7))
    (make-segment (make-vector 0.3 0.7)
                  (make-vector 0.2 0.6))
    (make-segment (make-vector 0.2 0.6)
                  (make-vector 0 0.8))
    (make-segment (make-vector 0 0.9)
                  (make-vector 0.2 0.7))
    (make-segment (make-vector 0.2 0.7)
                  (make-vector 0.3 0.75))
    (make-segment (make-vector 0.3 0.75)
                  (make-vector 0.4 0.75))
    (make-segment (make-vector 0.4 0.75)
                  (make-vector 0.35 0.9))
    (make-segment (make-vector 0.35 0.9)
                  (make-vector 0.4 1))
    (make-segment (make-vector 0.5 1)
                  (make-vector 0.55 0.9))
    (make-segment (make-vector 0.55 0.9)
                  (make-vector 0.5 0.75))
    (make-segment (make-vector 0.5 0.75)
                  (make-vector 0.6 0.75))
    (make-segment (make-vector 0.6 0.75)
                  (make-vector 1 0.45))
    (make-segment (make-vector 1 0.3)
                  (make-vector 0.6 0.5))
    (make-segment (make-vector 0.6 0.5)
                  (make-vector 0.7 0)))))


;Additional excercise from the lecture
;Create a function that takes one argument and calls the supplied func starting with that argument repeatedly n times
(define (repeated func n)
  (if (= n 1) func
      (lambda (x)
       (func ((repeated func (- n 1)) x)
               ))))

(define (push comb)
  (lambda (pict n)
    ((repeated (lambda (p) (comb pict p)) n) pict)))

(define right-push (push beside))
(define top-push (push below))
(define (top-right-push p n) (right-push (top-push p n) n))

(define (combine p1 p2)
  (lambda (frame) (p1 frame) (p2 frame)))

;Use fr with racket graphics library, because it uses the top left as 0,0
;(let ((fr (make-frame (make-vector 0 height) (make-vector width 0) (make-vector 0 (- height))))) (wave-painter fr))
(define fr (make-frame (make-vector 0 height) (make-vector width 0) (make-vector 0 (- height))))
;(wave-painter fr)

;((square-limit X 1) fr)
;((corner-split X 1) fr)
;(wave-painter (make-frame (make-vector 0 0) (make-vector width 0) (make-vector 0 height)))

(define ff  (make-frame (make-vector 0  height) (make-vector (/ width 2) 0) (make-vector 0 (- (/ height 2)))))
(define ff2 (make-frame (make-vector (/ width 2) (/ height 2)) (make-vector (/ width 2) 0) (make-vector 0 (- (/ height 2)))))
(define ff3 (make-frame (make-vector 0 (/ height 2)) (make-vector (/ width 2) 0) (make-vector 0 (- (/ height 2)))))
(define ff4 (make-frame (make-vector (/ width 2) height) (make-vector (/ width 2) 0) (make-vector 0 (- (/ height 2)))))


;((rotate180 (combine (corner-split X 5) (corner-split outline 5))) ff)
;((combine (corner-split X 5) (corner-split outline 5)) ff2)

;((square-limit wave-painter 5) ff3)
;((corner-split3 arrow-painter 5 rotate90) ff4)
;((below2 arrow-painter wave-painter) ff4)
;((top-right-push arrow-painter 5) ff4)
;((rotate180 (combine (corner-split X 1) (corner-split outline 1))) ff)

;((square-limit X 4) fr)

((combine X  outline) ff)
((rotate270 (combine (corner-split X 5) (corner-split outline 5))) ff4)

(wave-painter ff3)
((square-limit wave-painter 4) ff2)


