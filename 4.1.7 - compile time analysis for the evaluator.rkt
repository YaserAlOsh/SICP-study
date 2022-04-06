#lang sicp
(define (eval exp env) ((analyze exp) env))

(define apply-in-underlying-scheme apply)
(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((let? exp) (analyze (let->combination exp)))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))



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



(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

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
 
  (if (eq? env the-empty-environment)
      false
      (let ((res (assoc-frame (first-frame env) var)))
        (if res
            res
            (env-var-loop var (enclosing-environment env))))
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
        (list '> >)
        (list '>= >=)
        (list '<= <=)
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

;########################################################


(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))




;Ex 4.22
;From Ex 4.6
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



