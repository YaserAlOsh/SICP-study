#lang sicp
;#lang planet neil/sicp
;SICP Chapter 4

;Berkeley Labs
;Lab 12
;1.
;apply
;list-of-values
;eval-if
;eval-sequence
;eval-assignment
;eval-definition

;2.
;eval

;3.
;Because make-procedure gives you an expression that is a procedure.
;This procedure can then be applied by using apply, which will evaluate the operators and operands

(define apply-in-underlying-scheme apply)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((unbind? exp) (eval-unbind exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ;((cond? exp) (eval (cond->if exp) env))
        ;Ex 4.5
        ((cond? exp) (eval-cond exp env))
        ;Ex 4.6
        ((let? exp) (eval (let->combination exp) env))
        ;Ex 4.7
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((application? exp)
         (apply-custom (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(define (apply-custom procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        
        ((compound-procedure? procedure)
         ;Exercise 4.17
         (let ((names (internal-definitions-names (procedure-body procedure))))
             ;(has-internal-definitions? (procedure-body procedure))
             (if (not (null? names))
               (eval-sequence
                (transform-internal-definitions (procedure-body procedure))
                (extend-environment
                 (append (procedure-parameters procedure) names)
                 (append arguments (map (lambda (n) (list 'quote '*unassigned*)) names))
                 (procedure-environment procedure)))
               (eval-sequence
                (procedure-body procedure)
                (extend-environment
                 (procedure-parameters procedure)
                 arguments
                 (procedure-environment procedure))))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
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
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

#|(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))|#

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




#|(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (define (scan-frame frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))          ; car gives you the pair. car of that gives you the variable name. 
             (caar frame))                   ; sets the variable value which is the cdr of the pair.
            (else
             (scan-frame (cdr frame)))))
    
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan-frame frame))))
          ;(scan (frame-variables frame)
           ;     (frame-values frame)))))

  (env-loop env))|#

#|(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (define (scan-frame frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))          ; car gives you the pair. car of that gives you the variable name. 
             (set-cdr! (car frame) val))      ; sets the variable value which is the cdr of the pair
            (else
             (scan-frame (cdr frame)))))
    
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan-frame frame))))
          ;(scan (frame-variables frame)
           ;     (frame-values frame)))))
  (env-loop env))|#

#|(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame)))) |#


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
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '< <)
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

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
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



;### Book exercises ###



;Ex 4.2
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;(define (application? exp)
  ;(tagged-list? exp 'call))
;(define (operator exp) (cadr exp))
;(define (operands exp) (cddr exp))

;Ex 4.3
(define table '())
(define (get op tag)
  (assq table op))

(define (eval-directed exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((op (get 'eval (car exp))))
           (cond
             (op (op (cdr exp)))
             ((application? exp)
              (apply
               (eval-directed (car exp) env)
               (list-of-values (cdr exp) env)))
             (else
              (error "Unknown expression type -- EVAL" exp)))))))
                    
        
  
;Ex 4.4
(define (and? exp)
  (tagged-list? exp 'and))
(define (or? exp)
  (tagged-list? exp 'or))
(define (and-expressions exp)
  (cdr exp))
(define (or-expressions exp)
  (cdr exp))


(define (eval-and exp env)
  (define (eval-exps exp)
    (if (last-exp? exp)
        (eval (first-exp exp) env)
        (if (true? (eval (first-exp exp) env))
            (eval-exps (rest-exps exp))
            false)))
  (eval-exps (and-expressions exp)))
(define (eval-or exp env)
  (define (eval-exps exp)
    (if (last-exp? exp) (eval (car exp) env)
        (if (true? (eval (first-exp exp) env))
            true
            (eval-exps (rest-exps exp)))))
  (eval-exps (or-expressions exp)))


;Implementing 'and' and 'or' as derived expressions

(define (and->if exp)
  (expand-and (cdr exp)))
(define (or->if exp)
  (expand-or (cdr exp)))
(define (expand-and exps)
  (if (last-exp? exps)
      (car exps)
      (make-if
       (first-exp exps)
       (expand-and (rest-exps exps))
       false)))
(define (expand-or exps)
  (if (last-exp? exps)
      (car exps)
      (make-if
       (first-exp exps)
       true
       (expand-or (rest-exps exps)))))

;Ex 4.5
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;(test) => (receipent) syntax
(define receipent-symbol '=>)
(define (cond-action-receipent actions)
  (cadr actions))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (let ((actions (cond-actions first)))
              (make-if (cond-predicate first)
                         (if (eq? (first-exp actions) receipent-symbol)
                             (list (cond-action-receipent actions) (cond-predicate first))
                             (sequence->exp (cond-actions first)))
                         (expand-clauses rest))
                )))))
(define (eval-cond exp env)
  (define (expand-clauses clauses)
    (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (let ((actions (cond-actions first)))
              ;(if (eq? (first-exp actions) receipent-symbol)
               ;   (make-lambda '() (
              (let ((evaluated (eval (cond-predicate first) env)))
                (if (true? evaluated)
                    (if (eq? (first-exp actions) receipent-symbol)
                        (apply-custom
                         (eval (cond-action-receipent actions) env)
                         (list evaluated))
                         ;(list (cond-action-receipent actions) evaluated) env)
                        (eval (sequence->exp (cond-actions first)) env))
                    (expand-clauses rest)))
              )))))
  (expand-clauses (cond-clauses exp)))
;Ex 4.6

(define (let? exp)
  (tagged-list? exp 'let))
(define (let-expression exp)
  (cdr exp))

(define (let-body exp)
  (cddr exp))

(define (let->combination exp)
  ;Ex 4.8
  (if (symbol? (cadr exp))          ; Named Let?
      (sequence->exp
       (list
       (list
         'define               ; Define a procedure with the name of the let
         ;(cons
          (cons
          (cadr exp)                ; Procedure name
          (map car (caddr exp)))    ; Let variables names
          (cadddr exp))               ; Let body
        (cons
         (cadr exp)                 ; Call the named let procedure..
         (map cadr (caddr exp)))))  ; With the let vars expressions as arguments
      (cons
       (make-lambda
        (map car (cadr exp))
        (cddr exp))
       (map cadr (cadr exp)))))


;Ex 4.7
(define (make-let params-bindings body)
  ;(list 'let  params-bindings body)
  (append (list 'let params-bindings) body)
  )

(define (let*? exp)
  (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  (define (make-lets vars-bindings)
    (if (null? vars-bindings)
        (let-body exp) ;car because let-body will give a list
        (make-let
          (list (car vars-bindings))
          (list (make-lets (cdr vars-bindings))))))
  (make-lets (car (let-expression exp))))


;Ex 4.9
;For iteration construct

(define (for? exp)
  (tagged-list? exp 'for))
;(for (i 0) (< i 5) (inc i)
;   ;<body>
;)
;Transform into:
;(define (for-i i cond inc) (if (cond i) (begin <body> (for-i (inc i) cond inc)) '()))
;Or:
;(let *for* ((i 0) (cond <exp>) (modifier (inc i)))
;  (if (eval (lambda (x) <exp>) env)
;        <body>
;        (*for* (eval (lambda (x) modifier) env) <exp> (inc i)))
(define (for-var exp)
  (caadr exp))
(define (for-initial exp)
  (cadr exp))
(define (for-condition exp)
  (caddr exp))
(define (for-accumulator exp)
  (cadddr exp))
(define (for-body exp)
  (cddddr exp))

(define (for-exp exp)
  (list
   'let '*for*
   (list (for-initial exp)
         (list 'condition (make-lambda
                      (list (for-var exp))
                      (list (for-condition exp))))
         (list 'accu (make-lambda (list (for-var exp))
                      (list (for-accumulator exp)))))
   (make-if
     (list 'condition (for-var exp))
     (sequence->exp
      (list
       (sequence->exp (for-body exp))
       (list '*for* 
                    (list 'accu (for-var exp)) ;New i value
              'condition
              'accu)))
     ''())))


;(while (< i 5)
(define (while? exp)
  (tagged-list? exp 'while))
(define (while-condition exp)
  (cadr exp))
(define (while-var exp)
  (cadr exp))
(define (while-body exp)
  (cddr exp))
(define (while-exp exp)
  (list
   'let '*while*
   
    '()
    ;(for-initial exp)
         ;(list 'condition (make-lambda
        ;              '()
         ;             (list (while-condition exp)))))
         ;(list 'accu (make-lambda (list (for-var exp))
                      ;(list (for-accumulator exp)))))
   (make-if
     (while-condition exp)
     (sequence->exp
      (list
       (sequence->exp (while-body exp))
       (list '*while* )))
     ''())))
;Ex 4.10
;The above exercise does just that;


;4.11 and 4.12 are implemented above

;#############################################
;Exercise 4.13
(define (unbind? exp)
  (tagged-list? exp 'make-unbound!))
(define (unbind-variable exp)
  (cadr exp))
(define (eval-unbind exp env)
  (make-unbound! (unbind-variable exp) env))
(define (make-unbound! var env)
  (define (env-loop env)
    (define (remove-binding frame prev-frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar frame))
             (begin
               (set-cdr! prev-frame (cdr frame))         ; Unbind the variable in this frame.
               (env-loop (enclosing-environment env))))  ; Scan the next frame to unbind if it's bound there as well.
            (else
             (remove-binding (cdr frame) frame))))
    (if (eq? env the-empty-environment)
        ;(error "Unbound Variable -- MAKE-UNBOUND!" var)
        'done
        (remove-binding
         (frame-pairs (first-frame env))
         (first-frame env))))
  (env-loop env))
      


;Exercise 4.16
(define (filter predicate seq)
  (cond ((null? seq)
         '())
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))
(define (filter-both predicate seq)
  (cons
   (filter predicate seq)
   (filter (lambda (x) (not (predicate x))) seq)))
(define (internal-definitions procedure-body)
  (filter (lambda (e) (definition? e)) procedure-body))
(define (exclude-internal-definitions procedure-body)
  (filter (lambda (e) (not (definition? e))) procedure-body))

(define (scan-out-defines body)
  (define (transform-definitions definitions seqs)
    (let ((names (map cadr definitions))
          (def-exps (map caddr definitions)))
      (cons
       'let
       (cons
        (map (lambda (n) (list n (list 'quote '*unassigned*))) names)
        (append
         (map (lambda (n v) (list 'set! n v)) names def-exps)
         seqs)))))
  
  ;(let ((exps (filter-both (lambda (e) (definition? e)) body))
  (let ((definitions (internal-definitions body))
        (seqs (exclude-internal-definitions body)))
      (if (null? definitions)
          seqs
          (list (transform-definitions definitions seqs)))))
  

;Exercise 4.17
;There will be an additional frame because the let expression is transformed into a lambda expression.
;When that is evaluated, it creates a new frame by extending the environment of the original procedure.
;It will behave correctly because the let expression is guranteed to be evaluated correctly ??

;To make it work without creating an additional frame, add the internally defined variables to the procedure parameters, and use *unassigned* as an argument for each new parameter.
;Then transform internal definitions to sets

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


;Exercise 4.18
;This alternative strategy won't work for the solve procedure from section 3.5.4
;Because when the procedure (the lambda) is called, the inner lets will both be called..
;The second let, which will be translated to a lambda, will have to evaluate its arguments, and calling stream-map on '*unassigned* will result in an error.
;
;The original version on the other hand will have y set to a stream before calling stream-map, which will prevent an error from occuring.
;
;Notice that calling (delay dy), will work even if dy has the value '*unassigned*


(define unassigned (list 'quote '*unassigned*))

;Exercise 4.19

#|
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))|#

#|
(let ((a 1))
  (define (f x)
    (let
      ((b '*unassigned*)
       (a '*unassigned*))
      (set! b (+ a x))
      (set! a 5)
      (+ a b)))
  (f 10))
|#

;It is possible to make the interpreter handle simple cases in the third way Eva prefers.
;Basically reorder the defines, put the functions definitions first, then independent non-functions definitions, then dependent non-functions definition

;Exercise 4.20
;part a)
(define (let-vars exp)
  (map car (cadr exp)))
(define (let-vars-exps exp)
  (map cadr (cadr exp)))

(define (letrec->let exp)
  (let
      ((vars (let-vars exp))
       (exps (let-vars-exps exp)))
    
  (make-let
   (map (lambda (var) (list var unassigned))
        vars)
   (append
    (map (lambda (var val) (list 'set! var val))
         vars exps)
    (let-body exp)))))

;part b)
(define (f x)
  (letrec ((even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
           (odd? (lambda (n) (if (= n 0) false (even? (- n 1))))))
    (if (even? 2) (display "yess!!") (display "no!!!"))))
