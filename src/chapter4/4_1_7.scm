#lang sicp

;;; save a reference to original apply
(define apply-in-underlying-scheme apply)

;;; environment operations
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (filter predicate? items)
  (cond ((null? items) '())
        ((predicate? (car items) (cons (car items) (filter predicate? (cdr items)))))
        (else (filter predicate? (cdr items)))))

(define (definition->unassigned-binding exp) (make-binding (definition-variable exp) '*unassigned*))

(define (definition->assignment exp) (make-assignment (definition-variable exp) (definition-value exp)))

(define (scan-out-defines procedure-body)
  (make-let (map definition->unassigned-binding (filter definition? procedure-body))
            (append 
             (map definition->assignment (filter definition? procedure-body))
             (filter (lambda (exp) (if (not (definition? exp)) true false)) procedure-body))))

;;; ex 4.17
;;; another way to simulate simultaneous scope is to reuse the procedure's current environment instead of a `let`
(define (definition-variables procedure-body)
  (map definition-variable (filter definition? procedure-body)))

(define (definition-values procedure-body)
  (map (lambda (definition) '*unassigned*) (filter definition? procedure-body)))

(define (definitions->assignments procedure-body)
  (append 
   (map definition->assignment (filter definition? procedure-body))
   (filter (lambda (exp) (if (not (definition? exp)) true false)) procedure-body)))

;(define (apply-procedure procedure arguments)
;  (cond ((primitive-procedure? procedure)
;         (apply-primitive-procedure procedure arguments))
;        ((compound-procedure? procedure)
;         (eval-sequence
;          (definitions->assignments (procedure-body procedure))
;          (extend-environment (append (procedure-parameters procedure) (definition-variables procedure-body))
;                              (append arguments (definition-values procedure-body)) 
;                               (procedure-environment procedure))))
;        (else (error
;               "Unknown procedure type: APPLY" procedure arguments))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

;;; primitive procedures
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list '> >)
        (list '< <)
        (list '= =)
        (list '<= <=)
        (list '>= >=)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '/ /)
        (list '% remainder)
        (list 'print display)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure? procedure)
  (tagged-list? procedure 'primitive))

(define (primitive-implementation procedure) (cadr procedure))

(define (apply-primitive-procedure procedure args)
  (apply-in-underlying-scheme
   (primitive-implementation procedure) args))

;;; compound procedures
(define (compound-procedure? procedure)
  (tagged-list? procedure 'compound))

(define (procedure-body procedure)
  (caddr procedure))

(define (procedure-parameters procedure)
  (cadr procedure))

(define (procedure-environment procedure)
  (cadddr procedure))

(define (true? boolean-value)
  (eq? boolean-value true))

;(define (make-procedure parameters body environment)
;  (list 'compound parameters body environment))

;;; ex 4.16 c
(define (make-procedure parameters body environment)
  (list 'compound parameters (scan-out-defines body) environment))

;;; setup global environment
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;;; eval
(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ;;; ex 4.22
        ((let? exp) (analyze (let->combination exp)))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

;;; analyze- subprocedures

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
      (define-variable! var (vproc env) env) 'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
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
    (if (null? rest-procs) first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc) args
                              (procedure-environment proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

;;; execution procedures

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (spy fn)
  (lambda (first-arg . rest-args)
    (let ((all-args (cons first-arg rest-args)))
      (let ((result (apply-in-underlying-scheme fn all-args)))
        (newline)
        (display "ARGS: ")
        (display all-args)
        (newline)
        (display "RESULT: ")
        (display result)
        result))))

;;; self-evaluating?
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;;; variable?
(define (variable? exp)
  (symbol? exp))

;;; `quote` support
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

;;; `set!` support
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env) env)
  'ok)

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (make-assignment variable value)
  (list 'set! variable value))

;;; `define` support
(define (definition? exp)
  (tagged-list? exp 'define))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env) env)
  'ok)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

;;; `if` support
(define (if? exp)
  (tagged-list? exp 'if))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

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

;;; `lambda` support
(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;; `begin` support
(define (begin? exp)
  (tagged-list? exp 'begin))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (begin-actions exp)
  (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (first-exp seq)
  (car seq))

(define (rest-exps seq)
  (cdr seq))

(define (make-begin seq)
  (cons 'begin seq))

;;; `cond` support
(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

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
            (if (alternative-cond-clause? first)
                (make-if (cond-test first)
                         (list (cond-recipient first) (cond-test first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define (cond-clauses exp)
  (cdr exp))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause)
  (car clause))

(define (cond-actions clause)
  (cdr clause))

(define (make-cond-clause predicate actions)
  (list predicate actions))

(define (make-cond-else-clause actions)
  (make-cond-clause 'else actions))

(define (make-cond clauses)
  (cons 'cond clauses))

;;; `cond =>` support
(define (cond-test clause)
  (car clause))

(define (cond-recipient clause)
  (caddr clause))

(define (alternative-cond-clause? clause)
  (eq? (cadr clause) '=>))

;;; `or` support
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

;;; `and` support
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

;;; `let` support
(define (let? exp)
  (tagged-list? exp 'let))

(define (let->combination exp)
  (cond ((named-let? exp)
         (make-let (list (make-binding (named-let-name exp)
                                       (make-lambda (let-binding-variables (named-let-bindings exp))
                                                    (named-let-body exp))))
                   (cons (named-let-name exp) (let-binding-values (named-let-bindings exp)))))
        (else
         (cons (make-lambda (let-binding-variables (let-bindings exp)) (let-body exp))
               (let-binding-values (let-bindings exp))))))

(define (let-bindings exp)
  (cadr exp))

(define (let-binding-variable binding)
  (car binding))

(define (let-binding-value binding)
  (cadr binding))

(define (let-body exp)
  (cddr exp))

(define (let-binding-variables bindings)
  (map let-binding-variable bindings))

(define (let-binding-values bindings)
  (map let-binding-value bindings))

(define (make-let bindings body)
  (list 'let bindings body))

;;; `let*` support
(define (let*? exp)
  (tagged-list? exp 'let*))

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
                  (expand-bindings rest body)))))

;;; named `let` support
(define (named-let? exp)
  (symbol? (cadr exp)))

(define (named-let-name exp)
  (cadr exp))

(define (named-let-bindings exp)
  (caddr exp))

(define (named-let-body exp)
  (cadddr exp))

(define (make-binding variable value)
  (list variable value))

;;; ex 4.20
;;; `letrec` support

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (letrec-bindings exp)
  (cadr exp))

(define (letrec-body exp)
  (cddr exp))

(define (letrec-binding-variable binding)
  (car binding))

(define (letrec-binding-value binding)
  (cadr binding))

(define (letrec-binding->unassigned-binding binding)
  (make-binding (letrec-binding-variable binding) '*unassigned*))

(define (letrec-binding->assignment binding)
  (make-assignment (letrec-binding-variable binding) (letrec-binding-value binding)))

(define (letrec->combination exp)
  (make-let (map letrec-binding->unassigned-binding (letrec-bindings exp))
            (append 
             (map letrec-binding->assignment (letrec-bindings exp))
             (letrec-body exp))))

;;; `while` support
(define (while? exp)
  (tagged-list? exp 'while))

(define (while-predicate exp)
  (cadr exp))

(define (while-body exp)
  (caddr exp))

(define (while->combination exp)
  (make-begin (list (list 'define (list 'fn) (make-if (while-predicate exp)
                                                     (make-begin (list (while-body exp) (list 'fn)))
                                                     'false))
                    (list 'fn))))

;;; `apply` support
(define (application? exp)
  (pair? exp))

(define (apply-procedure procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure) (extend-environment
                                      (procedure-parameters procedure) arguments
                                      (procedure-environment procedure))))
        (else (error
               "Unknown procedure type: APPLY" procedure arguments))))

(define (operator exp)
  (car exp))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
  
(define (operands exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

;;; driver loop
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

;(driver-loop)