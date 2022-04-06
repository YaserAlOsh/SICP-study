#lang sicp
(#%require racket/base)
;SICP
;Ex 2.74
;We will have a system that has procedures which work on any division
;For each division, we will interface its procedures to the system
;The type of each procedure will be the division name
;Each division's individual file should have the type supplied, and the strucutre,
;probably as a pair where the car is the type and the cdr is the structure

;Hashtable:
(define *the-table* (make-hash));make the table 
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define (attach-tag type-tag content) (cons type-tag content))
(define (exrt-tag item) (car item))
(define (exrt-content item) (cdr item))

(define (install-division-sales)
  
  (define (key record) (car record))
  (define (record-items record) (cdr record))
  ;Suppose this division uses a simple list keyed by the name of the employee
  (define (look-up-list list itemKey keyFunc contentFunc null)
    (cond ((null? list) null)
          ((equal? (keyFunc (car list)) itemKey) (contentFunc (car list)))
          (else (look-up-list (cdr list) itemKey keyFunc contentFunc null))))
          
  (define (get-record employee-name division-file)
    (attach-tag 'sales (look-up-list (cdr division-file) employee-name key identity '())))
  
  
  ;Assuming records here are implemented as lists
  (define (item-identifier record)
    (car record))
  (define (item-value record)
    (cdr record))
  
  (define (get-salary employee-record)
    (look-up-list (record-items (exrt-content employee-record)) 'salary item-identifier item-value 0))
    ;(look-up-list (cdr (get-record (employee-name)
     ;                              'salary item-identifier item-value 0)))
    
  ;Part c)
  (define (has-record employee-name file)
    (let ((record (get-record employee-name file)))
      (cons (not (null? (exrt-content record))) record)))
  ;Interface to the headquarters:
  (put 'extract-type 'sales car)
  (put 'get-record 'sales get-record)
  (put 'get-salary 'sales get-salary)
  (put 'has-record 'sales has-record)
  'done)

(define (install-division-qr)
  
  (define (entry tree) (car tree))
  (define (key record) (car (entry record)))
  (define (content record) (cdr (entry record)))
  (define (left-branch tree)  (cadr tree))
  (define (right-branch tree) (caddr tree))
  ;Suppose this division uses a binary tree keyed by the name of the employee
  (define (look-up-tree tree itemKey)
     (cond ((null? tree) (cons #f '()))       
          ((equal? (key tree) itemKey) (cons #t (entry tree)))
   
          (else (let ((left (look-up-tree (left-branch tree) itemKey)))
                  (if (car left) left
                      (look-up-tree (right-branch tree) itemKey))))))
          
  (define (get-record employee-name division-file)
    (attach-tag 'qr (cdr (look-up-tree (exrt-content division-file) employee-name))))
  
  ;Part b)
  ;Assuming records here are implemented as lists as well
  (define (item-identifier record)
    (car record))
  (define (item-value record)
    (cdr record))
  
  (define (get-record-identifer record identifier null)
    (cond ((null? record) null)
          ((equal? (item-identifier (car record)) identifier)
           (item-value (car record)))
          (else (get-record-identifer (cdr record) identifier null))))
  
  (define (get-salary employee-record)
    (get-record-identifer (cdr (exrt-tag employee-record)) 'salary 0))

  ;Part c)
  (define (has-record employee-name file)
    (let ((record (get-record employee-name  file)))
      (if (not (null? (exrt-content record))) (cons #t record)
          (cons #f '()))))
  
  ;Interface to the headquarters system
  (put 'extract-type 'qr car)
  (put 'get-record 'qr get-record)
  (put 'get-salary 'qr get-salary)
  (put 'has-record 'qr has-record)
  'done)

(install-division-sales)
(install-division-qr)


(define (get-record employee-name file)
  ((get 'get-record (exrt-tag file)) employee-name file))

;b)
(define (get-salary employee-record)
  ((get 'get-salary (exrt-tag employee-record)) employee-record));(get-record employee-name file)))
;Any structure for the record would work. 
;But it should be consistent with what the get-record procedure returns.
;It's up to the implementation of each division to structure them as they see fit.

;Part c)
(define (find-employee-record employee-name divisions-files)
  (cond ((null? divisions-files) '())
        (else (let ((rec-res
                     ((get 'has-record (exrt-tag (car divisions-files)))
                      employee-name (car divisions-files))))
                (if (car rec-res) (cdr rec-res);Found record
                    ;Otherwise
                    (find-employee-record employee-name (cdr divisions-files)))))))


;d)
;They just need to have their own implementations of get-record, get-salary and has-record that comply with how the headquarters system uses them:
;get-record should return the employee's record with a tag of the division attached in the fronted (cons tag record)
;get-salary takes an employee's record
;has-record returns a compound object with #t in the first pair if the record was found and the record itself in the second pair

;Data for Testing
(define sales (cons 'sales (list (cons 'ahmed (list (cons 'address 'egypt) (cons 'salary 7000)))
                                 (cons 'yaser (list (cons 'address 'syria) (cons 'salary 50000))))))
(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))
(define qr (cons 'qr (make-tree (cons 'nadir (list (cons 'address 'pakistan) (cons 'salary 20000)))
                                (make-tree (cons 'amr (list (cons 'address 'egypt) (cons 'salary 7000)))   '() '())
                                (make-tree (cons 'moath (list (cons 'address 'syria) (cons 'salary 50000))) '() '()))))



;Ex 2.75:
(define (make-from-mag-angle mag angle)
  (define (dispatch op)
    (cond ((equal? op 'magnitude)   mag)
          ((equal? op 'angle) angle)
          ((equal? op 'real-part) (* mag (cos angle)))
          ((equal? op 'imag-part) (* mag (sin angle)))))
  dispatch)
(define (apply-generic-msg-passing op arg) (arg op))

;Ex 2.76:

;For the explicit generic dispatch, when we add a new type we need to define the representation's selectors and procedures for that type,
;then we need to add them under the generic selectors
;When we add new operations to the system, we only need to write them once for all the types (or representations)

;For the data-directed dispatch, when we add a new type, we have to install it in the system by providing all the required selectors and procedures for that type (or representation)
;We don't need to modify the generic selectors or operations.
;When we add a new operation, we need to add the procedure (or selector) for every type installed. This can be done anywhere. Calling that procedure simply uses (get 'procedure-name 'type)

;In the message-passing approach, when we add a new type we need to define the dispatch which has all the operations required, without changing any code already written
;When we add a new operation, we need to add it under every type's dispatch system

;For systems that require adding new types often, both the data-directed and message passing approaches would be suitable.
;For systems that require adding new operations often, we may use the explicit generic dispatch system.