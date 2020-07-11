#lang sicp

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;(define (make-frame variables values)
 ; (cons variables values))

;(define (frame-variables frame)
 ; (car frame))

;(define (frame-values frame)
 ; (cdr frame))

;(define (add-binding-to-frame! var val frame)
 ; (set-car! frame (cons var (car frame)))
  ;(set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;(define (lookup-variable-value var env)
 ; (define (env-loop env)
  ;  (define (scan vars vals)
   ;   (cond ((null? vars)
    ;         (env-loop (enclosing-environment env)))
     ;       ((eq? var (car vars))
      ;       (car vals))
      ;      (else (scan (cdr vars) (cdr vals)))))
    ;(if (eq? env the-empty-environment)
     ;   (error "Unbound variable" var)
      ;  (let ((frame (first-frame env)))
       ;   (scan (frame-variables frame) (frame-values frame)))))
  ;(env-loop env))

;(define (set-variable-value! var val env)
 ; (define (env-loop env)
  ;  (define (scan vars vals)
   ;   (cond ((null? vars)
    ;         (env-loop (enclosing-environment env)))
     ;       ((eq? var (car vars))
      ;       (set-car! vals val))
       ;     (else (scan (cdr vars) (cdr vals)))))
    ;(if (eq? env the-empty-environment)
     ;   (error "Unbound variable: SET!" var)
      ;  (let ((frame (first-frame env)))
       ;   (scan (frame-variables frame) (frame-values frame)))))
  ;(env-loop env))

;(define (define-variable! var val env)
 ; (let ((frame (first-frame env)))
  ;  (define (scan vars vals)
   ;   (cond ((null? vars)
    ;         (add-binding-to-frame! var val frame))
     ;       ((eq? var (car vars))
      ;       (set-car! vals val))
       ;     (else (scan (cdr vars) (cdr vals)))))
    ;(scan (frame-variables frame) (frame-values frame))))


; ex 4.11, 4.12

(define empty-frame '())

(define (make-frame variables values)
  (if (or (null? variables) (null? values))
      empty-frame
      (cons (cons (car variables) (car values)) (make-frame (cdr variables) (cdr values)))))

(define (add-binding-to-frame! var val frame)
  (cons (cons var val) frame))

(define (first-binding frame)
  (car frame))

(define (rest-bindings frame)
  (cdr frame))

(define (bound-variable binding)
  (car binding))

(define (bound-value binding)
  (cdr binding))

(define (set-bound-value! binding value)
  (set-cdr! binding value))

(define (find-binding var frame)
  (cond ((eq? frame empty-frame)
             false)
        ((eq? var (bound-variable (first-binding frame)))
         (first-binding frame))
        (else (find-binding var (rest-bindings frame)))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((binding (find-binding var (first-frame env))))
          (if binding
              (bound-value binding)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((binding (find-binding var (first-frame env))))
          (if binding
              (set-bound-value! binding val)
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((binding (find-binding var frame)))
          (if binding
              (set-bound-value! binding val)
              (add-binding-to-frame! var val frame)))))

; ex 4.13

(define (remove-binding! var frame)
  (cond ((eq? frame empty-frame)
             false)
        ((eq? var (bound-variable (first-binding frame)))
         (rest-bindings frame) true)
        (else (cons (first-binding frame) (remove-binding! var (rest-bindings frame))))))

; tries to remove binding in the current frame. If found and removed, it stops. Else it will look at the enclosing environment.
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (if (remove-binding! var frame)
        'done
        (make-unbound! var (enclosing-environment env)))))
