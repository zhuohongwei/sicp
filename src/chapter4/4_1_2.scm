#lang sicp

;ex 4.2 b

;(define (application? exp) (tagged-list? exp 'call))
;(define (operator exp) (cadr exp))
;(define (operands exp) (cddr exp))

;ex 4.3

;(define (eval exp env)
;  (cond ((self-evaluating? exp) exp)
;        ((variable? exp) (lookup-variable-value exp env))
;        (else ((get 'eval (type exp)) (body exp) env))))

;ex 4.4

(define (primitive-procedure? procedure)
  (error "Unimplemented: PRIMITIVE-PROCEDURE?"))

(define (apply-primitive-procedure procedure arguments)
  (error "Unimplemented: APPLY-PRIMITIVE-PROCEDURE"))

(define (compound-procedure? procedure)
  (error "Unimplemented: COMPOUND-PROCEDURE?"))

(define (procedure-body procedure)
  (error "Unimplemented: PRIMITIVE-BODY"))

(define (extend-environment parameters arguments environment)
  (error "Unimplemented: EXTEND-ENVIRONMENT"))

(define (procedure-parameters procedure)
  (error "Unimplemented: PROCEDURE-PARAMETERS"))

(define (procedure-environment procedure)
  (error "Unimplemented: PROCEDURE-ENVIRONMENT"))

(define (set-variable-value! variable value environment)
  (error "Unimplemented: SET-VARIABLE-VALUE!"))

(define (define-variable! variable value environment)
  (error "Unimplemented: DEFINE-VARIABLE!"))

(define (true? boolean-value)
  (error "Unimplemented: TRUE?"))

(define (lookup-variable-value variable environment)
  (error "Unimplemented: LOOKUP-VARIABLE-VALUE"))

(define (make-procedure parameters body environment)
  (error "Unimplemented: MAKE-PROCEDURE"))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure) (extend-environment
                                      (procedure-parameters procedure) arguments
                                      (procedure-environment procedure))))
        (else (error
               "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env) env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env) env)
  'ok)
                                    
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
        (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (make-cond-clause predicate actions)
  (list predicate actions))

(define (make-cond-else-clause actions)
  (make-cond-clause 'else actions))

(define (make-cond clauses)
  (list 'cond clauses))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (if (alternative-cond-clause? first) ; ex 4.5
                (make-if (cond-test first)
                         (list (cond-recipient first) (cond-test first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((or? exp) (eval (or->cond exp) env))
        ((and? exp) (eval (and->cond exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((application? exp)
         (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


; ex 4.4

(define (or? exp)
  (tagged-list? exp 'or))

(define (or-predicates exp)
  (cdr exp))

(define (no-predicates? predicates)
  (null? predicates))

(define (first-predicate predicates)
  (car predicates))

(define (rest-predicates predicates)
  (cdr predicates))
  
(define (or->cond exp)
  (make-cond (convert-or-predicates-to-cond-clauses (or-predicates exp))))

(define (convert-or-predicates-to-cond-clauses predicates)
  (if (no-predicates? predicates)
      (list (make-cond-else-clause 'false))
      (cons (make-cond-clause (first-predicate predicates) 'true)
            (convert-or-predicates-to-cond-clauses (rest-predicates predicates)))))

(define (and? exp)
  (tagged-list? exp 'and))

(define (and-predicates exp)
  (cdr exp))
  
(define (and->cond exp)
  (make-cond (convert-and-predicates-to-cond-clauses (and-predicates exp))))

(define (invert-predicate predicate)
  (make-if predicate 'false 'true))

(define (convert-and-predicates-to-cond-clauses predicates)
  (if (no-predicates? predicates)
      (list (make-cond-else-clause 'true))
      (cons (make-cond-clause (invert-predicate (first-predicate predicates)) 'false)
            (convert-or-predicates-to-cond-clauses (rest-predicates predicates)))))

; ex 4.5

(define (cond-test clause)
  (car clause))

(define (cond-recipient clause)
  (caddr clause))

(define (alternative-cond-clause? clause)
  (eq? (cadr clause) '=>))

; ex 4.6

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-bindings exp)
  (cadr exp))

(define (let-binding-variable binding)
  (car binding))

(define (let-binding-value binding)
  (cdr binding))

(define (let-body exp)
  (caddr exp))

(define (let-binding-variables bindings)
  (map let-binding-variable bindings))

(define (let-binding-values bindings)
  (map let-binding-value bindings))

;(define (let->combination exp)
 ; (cons (make-lambda (let-binding-variables (let-bindings exp)) (let-body exp))
  ;      (let-binding-values (let-bindings exp))))

; ex 4.7

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (make-let bindings body)
  (list 'let bindings body))

;(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
 ; (* x z))

;(let ((x 3))
 ; (let ((y (+ x 2)))
  ;  (let ((z (+ x y 5)))
   ;   (* x z))))

(define (first-binding bindings)
  (car bindings))

(define (rest-bindings bindings)
  (cdr bindings))

(define (let*->nested-lets exp)
  (expand-bindings (let-bindings exp) (let-body exp)))

(define (expand-bindings bindings body)
  (if (null? bindings)
      body
      (let ((first (first-binding bindings))
            (rest (rest-bindings bindings)))
        (make-let (list first)
                  (expand-bindings rest-bindings body)))))

; ex 4.8

(define (named-let? exp)
  (symbol? (cadr exp)))

(define (named-let-name exp)
  (cadr exp))

(define (named-let-bindings exp)
  (caddr exp))

(define (named-let-body exp)
  (cadddr exp))

(define (make-binding variable value)
  (cons variable value))

(define (let->combination exp)
  (cond ((named-let? exp)
         (make-let (list (make-binding (named-let-name exp)
                                       (make-lambda (let-binding-variables (named-let-bindings exp))
                                                    (named-let-body exp))))
                   (cons (named-let-name exp) (let-binding-values (named-let-bindings exp)))))
        (else
         (cons (make-lambda (let-binding-variables (let-bindings exp)) (let-body exp))
               (let-binding-values (let-bindings exp))))))
                 