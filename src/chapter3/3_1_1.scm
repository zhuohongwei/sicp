#lang sicp

; ex 3.1
(define (make-accumulator sum)
  (lambda (x)
    (begin (set! sum (+ x sum))
           sum)))
(define A (make-accumulator 5))
(A 10)
(A 15)

; ex 3.2
(define (make-monitored f)
  (let ((invocations 0))
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) invocations)
            ((eq? input 'reset-count) (set! invocations 0))
            (else (begin (set! invocations (+ 1 invocations)) (f input)))))))

(define s (make-monitored sqrt))
(s 'how-many-calls?)
(s 100)
(s 'how-many-calls?)
(s 400)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)

; ex 3.3
(define (make-account balance initial-password)
  (define (withdraw amount)
    (if (< balance amount)
        (error "Insufficient funds")
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (dispatch m password)
    (if (eq? password initial-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unsupported operation" m)))
        (lambda (args) "Incorrect password")))
  dispatch)

(define account-a (make-account 100 'secret))
((account-a 'withdraw 'some-secret) 1)
((account-a 'withdraw 'secret) 1)

; ex 3.4
(define (make-monitored-account balance initial-password)
  (let ((wrong-attempts 0))
    (define (withdraw amount)
      (if (< balance amount)
          (error "Insufficient funds")
          (begin (set! balance (- balance amount))
                 balance)))
    (define (deposit amount)
      (begin (set! balance (+ balance amount))
             balance))
    (define (call-the-cops)
      (begin (set! wrong-attempts 0)
      "Calling the cops now.."))
    (define (dispatch m password)
      (if (eq? password initial-password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unsupported operation" m)))
          (begin (set! wrong-attempts (+ 1 wrong-attempts))
                 (lambda (args)
                   (if (> wrong-attempts 7)
                       (call-the-cops)
                       "Incorrect password")))))
    dispatch))

(define account-b (make-monitored-account 100 'secret))
((account-b 'withdraw 'some-secret) 1)
((account-b 'withdraw 'some-secret) 1)
((account-b 'withdraw 'some-secret) 1)
((account-b 'withdraw 'some-secret) 1)
((account-b 'withdraw 'some-secret) 1)
((account-b 'withdraw 'some-secret) 1)
((account-b 'withdraw 'some-secret) 1)
((account-b 'withdraw 'some-secret) 1)