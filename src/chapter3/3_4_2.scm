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
                 
    
          
    
    
