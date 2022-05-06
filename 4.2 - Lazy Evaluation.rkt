#lang sicp
;SICP 4.2: Lazy Evaluation


;Ex 4.25
;In applicative-order, this will cause an infinite recursive call to factorial.
;In normal-order, it will work.


;Ex 4.26
(define (unless? expr)
  (tagged-list? expr 'unless))
(define (unless-if expr)
  (cadr expr))
(define (unless-usual expr)
  (caddr expr))
(define (unless-exceptional expr)
  (cadddr expr))
(define (unless->if expr)
  (list 'if
        (unless-if expr)
        (unless-exceptional expr)
        (unless-usual expr)))

;A situation where unless would be useful as a higher-order procedure.
(define (unless if-value usual-value exceptional-value)
  (if if-value exceptional-value usual-value))
(define nums   (list 2 4 2 4 3 4))
(define denoms (list 1 4 9 4 8 8))
;(map unless
 ;    (map (lambda (x) (= x 0)) denoms)
  ;   (map (lambda (a b) (/ a b)) nums denoms)
   ;  (map (lambda (x) 0) nums))


(define (factorial n)
  (unless->if
   '(unless (= n 1)
    (* n (factorial (- n 1)))
    1)))

(define apply-in-underlying-scheme apply)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (eval-quote->list (text-of-quotation exp) env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ;((unbind? exp) (eval-unbind exp env))
        ;((and? exp) (eval-and exp env))
        ;((or? exp) (eval-or exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ;((cond? exp) (eval (cond->if exp) env))
        ;Ex 4.5
        ;((cond? exp) (eval-cond exp env))
        ;Ex 4.6
        ;((let? exp) (eval (let->combination exp) env))
        ;Ex 4.7
        ;((let*? exp) (eval (let*->nested-lets exp) env))
        ;Ex 4.20
        ;((letrec? exp) (eval (letrec->let exp) env))
        ;Ex 4.26
        ((unless? exp) (eval (unless->if exp) env))
        ((pair-rep? exp) exp) ;(eval (pair-proc exp) env))
        ((application? exp)
         (apply-custom
          (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(define (apply-custom procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        
        ((compound-procedure? procedure)
         ;Exercise 4.17
         #|
          (let ((names (internal-definitions-names (procedure-body procedure))))
             ;(has-internal-definitions? (procedure-body procedure))
             (if (not (null? names))
               (eval-sequence
                (transform-internal-definitions (procedure-body procedure))
                (extend-environment
                 (append (procedure-parameters procedure) names)
                 (append (list-of-delayed-args arguments env) (map (lambda (n) (list 'quote '*unassigned*)) names))
                 (procedure-environment procedure)))|#
               (eval-sequence
                (procedure-body procedure)
                (extend-environment
                 (procedure-parameters procedure)
                 (list-of-delayed-args arguments (procedure-parameters-pairs procedure) env)
                 (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
;Lazy Evaluation:
(define (actual-value exp env)
  (force-it (eval exp env)))



(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

#|(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))|#
;Ex 4.31
(define (list-of-delayed-args exps params env)
  (if (no-operands? exps)
      '()
      (cons
       (if (pair? (first-operand params))
           ;(delay-it (first-operand exps) env (eq? (param-mode (first-operand params)) 'lazy-memo))
           (actual-value (first-operand exps) env)
           (delay-it (first-operand exps) env true))
            (list-of-delayed-args (rest-operands exps) (rest-operands params) env))))

(define (param-mode param-pair)
  (cadr param-pair))

                ;#### Representing Thunks ####
(define (delay-it exp env memoize)
  (list 'thunk memoize exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (caddr thunk))
(define (thunk-env thunk) (cadddr thunk))
(define (memoize? thunk)
  (eq? (cadr thunk) true))
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (caddr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (if (not (memoize? obj))
               result
               (begin
                 (set-car! obj 'evaluated-thunk)
                 (set-car! (cddr obj) result)
                 ; replace exp with its value
                 (set-cdr! (cddr obj) '())
                 ; forget unneeded env
                 result))))
        ((evaluated-thunk? obj) (thunk-value obj))
         ;(actual-value (thunk-exp obj) (thunk-env obj))
        (else obj)))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true?  (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;### Representing Expressions ###
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
  ;(list 'procedure parameters (scan-out-defines body) env)) ; scan-out-defines from exercise 4.16. Install it here to avoid re-processing it every time the body is accessed.
   ;For Exercise 4.17
  ;(list 'procedure
   ;     (append parameters (internal-definitions-names body))
    ;    (transform-internal-definitions body)
     ;   env))
       
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) ;(cadr p))
  ;For Ex. 4.31
  (map (lambda (a) (if (pair? a) (car a) a)) (cadr p)))
(define (procedure-parameters-pairs p)
  (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())


;Ex 4.11
(define (make-frame variables values)
  (if (or (null? variables) (null? values))
      '()
      (cons
       'frame
       (map cons variables values))))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (frame-pairs frame) (cdr frame))
;For the representation of Ex 4.11 to work, we need to modify add-binding-to-frame!

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


;#############################################
;Exercise 4.12 - Abstracting common operations
(define (assoc-frame frame var)
  (define (scan-frame pairs)
    (cond ((null? pairs) false)             ;(proc env))
        ((eq? var (caar pairs))          ; car gives you the pair. car of that gives you the variable name. 
             (car pairs))                    ; return the pair.
        (else (scan-frame (cdr pairs)))))
  (if (null? frame)
      false
      (scan-frame (frame-pairs frame))))
  
(define (env-var-loop var env)
  
  #|(define (scan-frame frame)
    (cond ((null? frame)
           (env-var-loop var (enclosing-environment env)))
             ;(proc env))
            ((eq? var (caar frame))          ; car gives you the pair. car of that gives you the variable name. 
             (car frame))                    ; return the pair.
            (else
             (scan-frame (cdr frame)))))|#
  (if (eq? env the-empty-environment)
      false
      (let ((res (assoc-frame (first-frame env) var)))
        (if res
            res
            (env-var-loop var (enclosing-environment env))))
      ;(let ((frame (frame-pairs (first-frame env))))
       ;   (scan-frame frame))))
      ))
            

(define (set-variable-value! var val env)
  (let ((res (env-var-loop var env)))
      (if res
          (set-cdr! res val)
          (error "Unbound variable -- SET!" var))))

(define (lookup-variable-value var env)
  (let ((res (env-var-loop var env)))
    (cond
      ((eq? res false)
       (error "Unbound variable" var))
      ;Exercise 4.16a
      ((eq? (car res) '*unassigned*)
       (error "Unassigned variable" var))
      (else (cdr res)))))

(define (define-variable! var val env)
  (let ((res (env-var-loop var env)))
      (if res
          (set-cdr! res val)
          (add-binding-to-frame! var val (first-frame env))))) ; Empty environment - add variable definition


;------------------- Primitive Handling ------------------------

(define primitive-procedures
  (list (list 'cons-scheme cons)
        (list 'car-scheme car)
        (list 'cdr-scheme cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '< <)
        (list '<= <=)
        (list '> >)
        (list '>= >=)
        (list 'inc inc)
        (list 'assoc assoc)
        (list 'display display)
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))



(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;-------------------- Running the evaluator --------------------

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))
(define glb the-global-environment)
;-------------------- Read-Eval Loop ---------------------

(define input-prompt ";;; Lazy-Eval input:")
(define output-prompt ";;; Lazy-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (let ((readable-output (if (pair-rep? output) (print-pairs output the-global-environment) output)))
        (announce-output output-prompt)
        (user-print readable-output))))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))



(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (filter predicate seq)
  (cond ((null? seq)
         '())
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))


(define (internal-definitions procedure-body)
  (filter (lambda (e) (definition? e)) procedure-body))
(define (exclude-internal-definitions procedure-body)
  (filter (lambda (e) (not (definition? e))) procedure-body))
(define (internal-definitions-names proc-body)
  (map cadr (internal-definitions proc-body)))

(define (internal-definitions-values proc-body)
  (map caddr (internal-definitions proc-body)))

(define (transform-define-to-set definitions)
  (map (lambda (n) (cons 'set! (cdr n))) definitions))

(define (transform-internal-definitions proc-body)
  (append
   (transform-define-to-set (internal-definitions proc-body))
   (exclude-internal-definitions proc-body)))



;Ex 4.27
;(define count 0)
;(define (id x) (set! count (+ count 1)) x)

;(define w (id (id 10)))
;;; L-Eval input:
;=> count
;;; L-Eval value:
; 1
;;; L-Eval input:
; w
;;; L-Eval value:
;procedure
;;; L-Eval input:
;count
;;; L-Eval value:
;2

;Ex 4.28
;(define (m f x) (f x))


;Ex 4.29
;Any recursive procedure is a good example. Fib(n) = Fib(n-1) + Fib(n-2)

(define (square x) (* x x))
;;; L-Eval input:
;(square (id 10))
;;; L-Eval value:
;100
;;; L-Eval input:
;count
;;; L-Eval value:
;2


;Ex 4.30
;a) It works because in lazy evaluation, all expressions are still evaluated and thus procedures are still called.
;The "proc" argument will need to be forced when (proc (car items)) is called.
;(This is another example for Ex 4.28)

;b) In the original eval-sequence:
;   (p1 1) => (1 . (2))
;   (p2 1) => 1
;   With the proposed change:
;   (p1 1) => (1 . (2))
;   (p2 1) => (1 . (2))

;c) because in part a) we were evaluating procedure calls so the operator gets forced even without the proposed change.

;d) The point of lazy evaluation is to defer evaluating arguments until they are absolutely needed.
;   Using Cy's approach does not interfere with that. It simply accounts for corner-cases.
;   Another approach would be to force values returned by lookup-variable-name, but that would interfere with other cases where it is not needed to evaluate the value.
;   Cy's approach seems good to me.


;Ex 4.33
(define (eval-quote->list quotation env)
  (define (create-list q)
    (if (null? q) '()
        (list 'cons (car q)
                  (create-list (cdr q) env))))
  (cond
      ((null? quotation) '())
      ((not (pair? quotation)) quotation)
      (else
       (eval
        (create-list quotation)
        env))))
;Ex 4.34
;(define-variable! 'cons (make-procedure '(x y) '(*pair* (lambda (m) (m x y))) glb) glb)
;(define-variable! 'car (make-procedure '(z)    '((car-scheme (cdr-scheme z)) (lambda (p q) p)) glb) glb)
;(define-variable! 'cdr (make-procedure '(z)    '((car-scheme (cdr-scheme z)) (lambda (p q) q)) glb) glb)
(eval '(define (cons x y) (cons-scheme '*pair* (lambda (m) (m x y)))) glb)
(eval '(define (car z) ((cdr-scheme z) (lambda (p q) p))) glb)
(eval '(define (cdr z) ((cdr-scheme z) (lambda (p q) q))) glb)

(define (pair-rep? exp)
  (tagged-list? exp '*pair*))
(define (pair-proc exp)
  (cdr exp))
(define (print-pairs pairs env)
  ;(define (loop hpair tpair)
    
  (let* ((first-val (actual-value (list 'car pairs) env))
         (first-print-val (if (pair-rep? first-val) (print-pairs first-val env) first-val))
         (rest-vals (actual-value (list 'cdr pairs) env)))
    (cond
      ((eq? rest-vals '()) (cons first-print-val '()))
      ((pair-rep? rest-vals) 
        (if (eq? (actual-value (list 'cdr rest-vals) env) pairs) ; check if first = first->cdr>cdr
            (cons first-print-val '*#inf*)
            (cons first-print-val (print-pairs rest-vals env))))
      (else (cons first-print-val (cons rest-vals '()))))))
     
    

