#lang sicp
(define (accumulate func base seq)
  (if (null? seq) base
      (func (car seq) (accumulate func base (cdr seq)))))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (contains seq s)
  (cond ((null? seq) #f)
        ;((not (list? seq)) (eq? seq s))
        ((eq? (car seq) s) #t)
        (else (contains (cdr seq) s))))

;Huffman incoding trees

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode-tree bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
    
  (decode-1 bits tree))

(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
        ((= bit 1) (right-branch tree))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        (;(or (= (weight x) (weight (car set)))
             (< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (adjoin-set-keep x set)
  (cond ((null? set) (list x))
        ((or (= (weight x) (weight (car set))) (< (weight x) (weight (car set)))) (cons x set))
        (else (cons (car set) (adjoin-set-keep x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))
;Ex 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;Ex 2.68
(define (encode message tree)
  (if (null? message) '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  ;This procedure uses an in-traversal method of finding the symbol
  ;Once it finds it, it builds it up, cons a 0 or 1 depending on the branch,
  ;It puts that in the cdr of the result
  ;In the worst case, this procedure  is O((n-1)) which is O(n) where n is the number of symbols
  (define (encode-1 current-branch isFirst)
    (cond ((null? current-branch) (cons #f '()))
          ((leaf? current-branch) (if (eq? (symbol-leaf current-branch) symbol)
                                      (cons #t '())
                                      (cons #f '())))
        (else
         (let ((left-res (encode-1 (left-branch current-branch) #f)))
          (if (car left-res) (cons #t (cons 0 (cdr left-res)))
              (let ((right-res (encode-1 (right-branch current-branch) #f)))
                (if (car right-res) (cons #t (cons 1 (cdr right-res)))
                    (if isFirst;If this is the first call, then it means we have not found the symbol at all
                      (error "bad smybol: encode-symbol" symbol)
                      (cons #f '()))
                    )))))))
  
  ;Another way is to check the symbols list on each non-leaf node (called tree or branch here
  ;This procedure is probably O(log(n)) * O(n + (n-1) + (n-2) + .. + 1) = O(nlog(n))
  ;Because we go down through the tree each step, so we do log(n) steps.
  ;In each step we go through the m symbols of the tree (worst case)
  ;At first step, they are n symbols, then n-1, n-2 .... 1. So it's O(n) + O(n-1) + .. O(1) = O(n)
  ;
  (define (encode-2 current-branch)
    (cond ((leaf? current-branch) (if (equal? symbol (symbol-leaf current-branch)) '()
                                      (error "bad symbol: encode-symbol" symbol)))
          
          ((contains (symbols (left-branch current-branch)) symbol)
           (cons 0 (encode-2 (left-branch current-branch))))
          (else ;(contains (symbols (right-branch current-branch)) symbol)
           (cons 1 (encode-2 (right-branch current-branch))))))
          ;(else (error "bad symbol: encode" symbol))))
  (cdr (encode-1 tree #t))
  ;(encode-2 tree)
  )
            
            

;Ex 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
        ((null? (cdr leaf-set)) (car leaf-set))
        ;((null? (cddr leaf-set)) (make-code-tree (car leaf-set) (cadr leaf-set)))
        (else  (successive-merge (adjoin-set
                                  ;(if (> (weight (cadr leaf-set)) (weight (car leaf-set)))
                                      ;(make-code-tree (cadr leaf-set) (car leaf-set))
                                      (make-code-tree (first-in-order-set leaf-set) (second-in-order-set leaf-set))
                                   (rest-of-set leaf-set))))))

(define (first-in-order-set set) (car set))
(define (second-in-order-set set) (cadr set))
(define (rest-of-set set) (cddr set))


;Using 'pairs' does not give me the same tree as the book has shown. probably because the alogrithm does not produce a unique tree
(define pairs '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
(define pairs2 '((A 8) (B 5) (C 3) (D 1) (E 1) (F 1) (G 1) (H 1)))

;Ex 2.70

(define rock-songs-alphabet '((a 2) (Get 2) (Sha 3) (Wah 1) (boom 1) (job 2) (na 16) (yip 9)))
(define rock-tree (generate-huffman-tree rock-songs-alphabet))
;The length of the encoded message is 84 bits
;With fixed length encoding, we would need 3 bits per symbol(log2(8)),
;so it would be 36 * 3 = 108
;22% compression ratio


;Ex 2.71
;Most frequent symbol: 1 bit
;Least frequent symbol: n-1 bits


;Ex 2.72

;The order of growth of the first procedure encode-1 is O(n-1) or O(n), where n is the number of symbols.
;Because I only call the function recursively on every non-leaf node, and there are n-1 of them
; (a complete binary tree of n nodes has n-1 non-leaf nodes: n/2 + n/4 + .. n/(n-1) + 1 = n - 1)

;For the second procedure encode-2, I check the symbols on each step, which is O(n) for the first step, and less for the second step..
;But for bigO notation, we assume it's O(n).
;And we do that log(n) times (the depth of the tree), so it's log(n)*O(n) = O(n*log(n))

;For the special case, to encode the most frequent bit, we can do O(1), assuming the most frequent symbol was placed in the left
;To encode the least frequent, using the second procedure encode-2:
;on each step we have O(1) to check the left branch, and O(n) to check the right branch symbols.
;And we do n-1 steps O(log2(2^n-1)) = O(log2(2^n)) = O(n)
;T(n) = T(n-1) + O(n) + O(1)
; = O(n) + O(n-1) + .. + O(1) + n*O(1) = O(n^2)



;Arabic Alphabet compression using huffman encoding tree

(define arabic-alphabet-pairs '(
                (ا 10)
                (أ 6)
                (ء 4)
                (ب 6)
                (ت 4)
                (ث 1)
                (ج 7)
                (ح 3)
                (خ 2)
                (د 4)
                (ذ 1)
                (ر 6)
                (ز 3)
                (س 7)
                (ش 5)
                (ص 6)
                (ض 2)
                (ط 3)
                (ظ 1)
                (ع 6)
                (غ 1)
                (ف 7)
                (ق 6)
                (ك 4)
                (ل 8)
                (م 8)
                (ن 7)
                (ه 9)
                (و 8)
                (ي 10)
                (ة 3)
                (ئ 2)
                (ى 5)
                (إ 2)
                ))

(define arabic-tree (generate-huffman-tree arabic-alphabet-pairs))

(define arabic-msg '(أ ه ل ا و س ه ل ا
                       ب ك ف ي ا ل ت ر م ي ز ب ا س ت خ د ا م أ
                       ش ج ا ر ه ف م ا ن ب ه ذ ه ا ل ط ر ي ق ة
                       ن س ت ط ي ع ت خ ف ي ف ح ج م ا ل ن ص ا ل م ك ت و ب
                       ب ا ل ل غ ة ا ل ع ر ب ي ة ب ن س ب ة ج ي د ة ت ص ل
                       إ ل ى خ م س ي ن ب ا ل م ئ ة))

(define arabic-msg2 '(أ ه ل ا و س ه ل ا
                       ب ك ف ي ا ل س ه ل ا
                       ب ك ف ي ا ل ت ر م ي ح ج م ا ل ن ص ا ل م ك ت و ب
                       ب ا ل ل غ ة ا ل ع ر ب ز ب ا س ت خ د ا م أ
                       ش ج ا ر ه ف م ا ن ب ه ذ ه ا ل ط ر ي ق ة
                       ن س ت ط ي ع م ي ز ب ا س ت خ د ا م أ
                       ش ج ا ر ه ف م ا ن ب ه ذ ه ا ل ط ر ي ق ة
                       ن س ت ط ي ع ت خ  ف م ا ن ب ه ذ ه ا ل ط ر ي ق ة
                       ن س ت ط ي ع ت ف ي ف ح ج م ا ل ن ص ا ل م ك ت و ب
                       ب ا ل ل غ ة ا ل ع ر ب ي ة ب ن س ب ة ج ي د ة ت ص ل
                       إ ل ى خ م س ي ن ب ا ل م ئ ة))

(define arabic-msg3 '(ه ذ ه ت ج ر ب ة أ خ ر ى ل م ع ر ف ة ن س ب ة ا ل ت و ف ي ر ع ن د ا س ت خ د ا م ه ذ ه ا ل خ و ا ر ز م ي ة
                        و أ ن ا أ ح ا و ل م ع ر ف ة ا ل ك ف ا ئ ة أ و ا ل ف ا ئ د ة م ن ه ا ه ن ا أ ر ي د ت ن و ي ع ا ل ح ر و ف
                        ا ل م س ت خ د م ة ب ش ك ل ع ش و ا ئ ي ل ك ي أ س ت ط ي ع ا ل ح ص و ل ع ل ى ن ت ي ج ة أ ك ث ر د ق ة و ش ف ا ف ي ة
                        م ل ع ب ح د ي ق ة م د ر س ة ا س ت ا د غ ر ف ة ل ا ب ت و ب ه ا ت ف م و ق ع ب ر م ج ة ر ي ا ض ي ا ت أ ل ع ا ب
                        ت ط و ي ر ك ل م ا ت ع ش و ا ئ ي ة ف ق ط ل م ع ر ف ة ن س ب ة ا ل ض غ ط غ ر ي ب أ ن ا ل ن س ب ة ل ا ت ت غ ي ر
                        ك ث ي ر ا ب ت غ ي ر ا ل أ ح ر ف ا ل م س ت خ د م ة م ع أ ن ن ي ا س ت خ د م ت ت ك ر ا ر ع ش و ا ئ ي س ت ا د غ ر ف ة ل ا ب ت و ب ه ا ت ف م و ق ع ب ر م ج ة ر ي ا ض ي ا ت أ ل ع ا ب
                        ت ط و ي ر ك ل م ا ت ع ش و ا ئ ي ة ف ق ط ل م ع ر ف ة ن س ب ة ا ل ض غ ط غ ر ي ب أ ن ا ل ن س ب ة ل ا ت ت غ ي ر
                        ك ث ي ر ا ب ت غ ي ر ا ل أ ح ر ف ا ل م))