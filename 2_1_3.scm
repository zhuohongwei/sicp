#lang sicp
;ex 2.4
(define (alt-cons x y)
  (lambda (selector) (selector x y)))
(define (alt-car pair)
  (pair (lambda (x y) x)))
(define (alt-cdr pair)
  (pair (lambda (x y) y)))

(define p (alt-cons 1 2))
(alt-car p)
(alt-cdr p)

;ex 2.5
(define (divisible? x y)
    (= (remainder x y) 0))

(define (log-base-three x)
  (/ (log x) (log 3)))

(define (base-three? x)
    (let ((power (log-base-three x)))
      (= (truncate power) power)))

(define (nonneg-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (nonneg-car pair)   
  (define (correct? a-guess)
    (let ((base-two-number (expt 2 a-guess)))
      (and (divisible? pair base-two-number) (base-three? (/ pair base-two-number)))))
  (define (iter a-guess)
    (if (correct? a-guess)
        a-guess
        (iter (+ a-guess 1))))
  (iter 0))

(define p2 (nonneg-cons 10 3))
(nonneg-car p2)

;ex 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))
