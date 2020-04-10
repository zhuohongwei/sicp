#lang sicp
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? first second)
    (< (abs (- first second)) tolerance))
  (define (try-guess guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try-guess next))))
  (try-guess first-guess))

(define (average x y) (/ (+ x y) 2))

;Ex 1.36
(define (fixed-point-printed f first-guess)
  (define (close-enough? first second)
    (< (abs (- first second)) tolerance))
  (define (try-guess guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try-guess next))))
  (try-guess first-guess))

(newline)
(display "Finding solution for x^x = 1000")
(newline)
(fixed-point-printed (lambda (x) (/ (log 1000) (log x))) 2)
(fixed-point-printed (lambda (x) (average x (/ (log 1000) (log x)))) 2)

;Ex 1.37
;a
(define (cont-frac n d k)
  (define (term i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (term (+ i 1))))))
  (term 1))

(newline)
(display "Estimating golden ratio")
(newline)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           3)

;b
(define (cont-frac-iterative n d k)
  (define (cont-frac-iteration result i)
    (if (= i 1)
        result
        (cont-frac-iteration (/ (n (- i 1)) (+ (d (- i 1)) result)) (- i 1))))
  (cont-frac-iteration (/ (n k) (d k)) k))

(cont-frac-iterative (lambda (i) 1.0)
                     (lambda (i) 1.0)
                     3)


;Ex 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) i (* i i)))
             (lambda (i) (- (* i 2) 1))
             k))
(define (tan-cf-iterative x k)
  (cont-frac-iterative (lambda (i) (if (= i 1) i (* i i)))
             (lambda (i) (- (* i 2) 1))
             k))

(tan-cf 1 10)
(tan-cf-iterative 1 10)
      