#lang sicp

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release)
             (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true) false)))

; ex 3.47 a

(define (make-semaphore n)
  (let ((mutex (make-mutex)) (count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (= count n)
                 (begin (mutex 'release) (the-semaphore 'acquire))
                 (begin (set! count (+ count 1)) (mutex 'release))))
            ((eq? m 'release)
             (mutex 'acquire)
             (set! count (- count 1))
             (mutex 'release))))
    the-semaphore))

; ex 3.47 b

(define (make-another-semaphore n)
  (let ((count 0))
    (define (test-and-set!)
      (if (= count n)
           true
           (begin (set! count (+ 1 count)) false)))
    (define (clear!)
      (set! count (- count 1)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set!)
                 (the-semaphore 'acquire))) ; retry
            ((eq? m 'release)
             (clear!))))
    the-semaphore))
                 
; ex 3.48

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val)) serialized-p)))

(define make-account-and-serializer
  (let ((sequence 0))
    (lambda (balance)
      (set! sequence (+ sequence 1))
      (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
      (define (deposit amount)
        (set! balance (+ balance amount)) balance)
      (let ((balance-serializer (make-serializer)) (account-number sequence))
        (define (dispatch m)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'balance) balance)
                ((eq? m 'serializer) balance-serializer)
                ((eq? m 'account-number) account-number)
                (else (error "Unknown request: MAKE-ACCOUNT" m))))
        dispatch))))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (account-number1 (account1 'account-number))
        (serializer2 (account2 'serializer))
        (account-number2 (account2 'account-number)))
    (if (< account-number1 account-number2) 
        ((serializer1 (serializer2 exchange)) account1 account2)
        ((serializer2 (serializer1 exchange)) account1 account2))))

(define account1 (make-account-and-serializer 100))
(define account2 (make-account-and-serializer 200))

(account1 'account-number)
(account2 'account-number)



