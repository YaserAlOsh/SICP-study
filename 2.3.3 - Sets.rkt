#lang sicp
(define (accumulate func base seq)
  (if (null? seq) base
      (func (car seq) (accumulate func base (cdr seq)))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;Representing Sets


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if #f ;(element-of-set? x set) ;Ex 2.60: Put #f here to allow duplicateds
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;Ex 2.59
(define (union-set set1 set2)
  
  (if (null? set1) set2
      ;Clearer way
      ;element of set is called n times, so  O(n^2)
      (if (element-of-set? (car set1) set2)
          (union-set (cdr set1) set2)
          (cons (car set1) (union-set (cdr set1) set2)))))
      
      ;One messy way (same performance, because element-of-set is called n times, so the time complexity is O(n^2)
      ;(let ((rem-set (union-set (cdr set1) set2)))
      ;  (if (element-of-set? (car set1) rem-set)
       ;     rem-set
       ;     (cons (car set1) rem-set)))))
      ;If we allow duplicates, then the problem reduces to O(n) times:
      ;(cons (car set1) (union-set (cdr set1) set2))))

;Ex 2.60
;If we allow duplicates:
; adjoin-set becomes O(1) operation
; union-set becomes O(n) operation
; intersection-set stays the same
; element-of-set stays the same
;It would be useful in cases where we need to add items to sets repeteadly, and where we union sets more than intersect them.
;But you should always try to use it actually, because non-repeatedness can be imposed by other means (such as the interface)

;Ordered Sets
;Ex 2.61
(define (element-of-ordered-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-ordered-set? x (cdr set)))))

(define (adjoin-ordered-set x set)
  (cond ((= (car set) x) set)
        ((> (car set) x) (cons x set))
        (else (cons (car set) (adjoin-ordered-set x (cdr set))))))

(define (intersect-ordered-set set1 set2)
  (if (or (null? set1) (null? set2)) '()
        (let ((x1 (car set1))
              (x2 (car set2)))
          (cond ((= x1 x2) (cons x1 (intersect-ordered-set (cdr set1) (cdr set2))))
                ((< x1 x2) (intersect-ordered-set (cdr set1) set2))
                (else (intersect-ordered-set set1 (cdr set2)))))))
                ;((< x1 x2)
                 ;(cons x1 (union-ordered-set (cdr set1) set2)))
                ;(else (cons x2 (union-ordered-set set1 (cdr set2)))))))))

(define (union-ordered-set set1 set2)
  (cond ((null? set1) set2)  ((null? set2) set1)
        (else (let ((x1 (car set1))
              (x2 (car set2)))
          (cond ((= x1 x2) (cons x1 (union-ordered-set (cdr set1) (cdr set2))))
                ((< x1 x2)
                 (cons x1 (union-ordered-set (cdr set1) set2)))
                (else (cons x2 (union-ordered-set set1 (cdr set2)))))))))



;Binary Tree

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-bt? x set)
  (cond ((null? set) #f)
        ((= (entry set) x) #t)
        ((< (entry set) x) (element-of-set-bt? x (left-branch set)))
        (else (element-of-set-bt? x (right-branch set)))))

(define (adjoin-set-bt x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (entry set) x) set)
        ((> (entry set) x)
         (make-tree (entry set)
                    (adjoin-set-bt x (left-branch set))
                    (right-branch set)))
        (else (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-bt x (right-branch set))))))


(define (tree->list1 tree)
  (if (null? tree) '()
        (append (tree->list1 (left-branch tree))
                (cons (entry tree)
                      (tree->list1 (right-branch tree))))))

(define (tree->list2 tree)
  (define (copy-tree-list tree result-list)
    (if (null? tree) result-list
        (copy-tree-list (left-branch tree)
                        (cons (entry tree)
                              (copy-tree-list
                               (right-branch tree)
                                result-list)))))
  (copy-tree-list tree '()))


;Ex 2.64

;Convert an ordered list to a balanced binary tree

;The idea is to first make a tree with the element in the middle as the node.
;  If the list length is even, the tree the middle will be the quotient of (n 2).

;Then, the left tree of the first node is basically another call to the function, with the elements before the middle.
;The right tree of the first node is also another call with the elements after the middle.

;The left elements count is (quotient (n - 1) 2);
;The right elements count is n - left-elements-count -1

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      ;Start with the left side of the tree
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left (cdr left-result)))
            (let ((mid-node (car non-left))
                  (right-size (- n (+ left-size 1)))
                  (right-elts (cdr non-left)))
              (let ((right-result
                     (partial-tree right-elts right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                (cons
                 (make-tree mid-node left-tree right-tree)
                 remaining-elts)
                 ))))))))
;This procedure uses the idea presented above.
;To divide the problem into two, it first finds the partial tree of only the elements to the left of the middle
; The car of the result is the tree containting the left elements, and the cdr are the remaining list elements
;The first of the remaining elements is used as the tree node, then the elements after it are used in constructing the right tree.
;The length of right-elts and right-size are always the same


;In the base case, when n = 1, the call to (parital-tree elts 1):
;The left-result returns '() in car and the remaining element in cdr
;and right-result returns '() in both car and cdr.
;Then the tree constructed becomes the first element of the list.
;This base case is repeated for both the left-result and right-result.

;For the list  1 3 5 7 9 11
;Left size is 2, so left-result will be (partial-tree (1 3 5 7 9 11) 2)
;This again will create left-result with left-size of 0, so it will return ('() (1 3 5 7 9 11)
;The mid element will be 1, right-size is 1 so (partial-tree (3 5 7 9 11) 1) will return ((3 '() '()) (5 7 9 11)) as discussed (base case)
;The construced left tree becomes (1 '() (3 '() '())) and rem-elts are (5 7 9 11)
;Then the mid element of the first call will be 5,
;right-size will be (6 - left-size - 1) = 3, -> right-tree will be (partial-tree (7 9 11) 3) which will simply return:
; (9 (7 '() '()) (11 '() '())
;The full tree becomes
'(5 (1 '() (3 '() '())) (9 (7 '() '()) (11 '() '())))

;The running complexity
;Assuming cons, car, cdr are all O(1). and because whether n is even or odd does is negligible:
;T(n)   = 2*T(n/2) + O(1)
;T(n/2) = 2*T(n/4) + O(1)
;T(n/4) = 2*T(n/8) + O(1)
;....
;T(1)   = O(1)
;So T(n) = O(n). list->tree calls length as well which is O(n) so the total is 2O(n) = O(n)

;Ex 2.65

(define (union-set-bt set1 set2)
  ;For ordered sets:
  ;Convert each set into a list. This is O(n + m)
  ;Union Each list using union-ordered-set, this is O(n)
  ;Convert the resultant list into a tree, this is also O(n) (use tree->list2 which is O(n)
  ;All in all it's almost O(4*n) which is actually O(n)
  (list->tree (union-ordered-set (tree->list2 set1) (tree->list2 set2))))
  ;(cond ((null? set1) set2)
   ;     ((null? set2) set1)
    ;    (else (let ((left-union (union-set-bt (left-branch set1) (left-branch set2)))
     ;         (right-union (union-set-bt (right-branch set1) (right-branch set2))))
      ;    (make-tree (entry set1) left-union right-union)))))
      
(define (intersect-set-bt set1 set2)
  (list->tree (intersect-ordered-set (tree->list2 set1) (tree->list2 set2))))

;Can we do it without using the results of exercises 2.63 and 2.64??


;I will leave this for later

;Ex 2.66
(define (make-tree-kp entry left right key)
  (cons (cons entry key) (cons left (cons right '()))))

(define (value tree) (car (entry tree)))
(define (key tree) (cdr (entry tree)))

(define (look-up-bt given-key tree)
  (cond ((null? tree) #f)
        ((= given-key (key tree)) (entry tree))
        ((< given-key (key tree)) (look-up-bt given-key (left-branch tree)))
        (else (look-up-bt given-key (right-branch tree)))))
      
  
;Example
(define yaser (make-tree-kp 'yaser '() '() 1))
(define tamir (make-tree-kp 'tamir '() '() 3))
(define samir (make-tree-kp 'samir yaser tamir 2))


(define baraa (make-tree-kp 'baraa '() '() 5))
(define ahmed (make-tree-kp 'ahmed '() '() 7))
(define omar (make-tree-kp 'omar baraa ahmed 6))

(define ibrahim (make-tree-kp 'ibrahim samir omar 4))