#lang sicp

; ex 3.7
(define (make-account balance initial-password)
  (define (withdraw amount)
    (if (< balance amount)
        (error "Insufficient funds")
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (make-joint new-password)
    (lambda (m supplied-password)
      (if (eq? supplied-password new-password)
            (dispatch m initial-password)
            (lambda (amount . rest) "Incorrect password"))))
  (define (dispatch m password)
    (if (eq? password initial-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'make-joint) make-joint)
              (else (error "Unsupported operation" m)))
        (lambda (amount . rest) "Incorrect password")))
  dispatch)

(define (make-joint account password another-password)
  ((account 'make-joint password) another-password)) 

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'withdraw 'some-secret) 1)
((paul-acc 'withdraw 'rosebud) 1)
((peter-acc 'withdraw 'open-sesame) 1)

; ex 3.8
(define f
  (let ((old-value 0))
    (lambda (new-value)
      (let ((temp-value old-value))
        (set! old-value new-value)
        temp-value))))

(f 1)
(f 0)
