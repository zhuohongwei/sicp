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

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newtons-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))

;Ex 1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;solving x^3 + 12x^2 + 3x + 15 = 0
(newtons-method (cubic 12 3 15) 1)


;Ex 1.41
(define (inc x) (+ x 1))

(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)

;Ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose (lambda (x) (* x x)) inc) 6)

;Ex 1.43
(define (repeated f times)
  (if (= times 1)
      f
      (repeated (compose f f) (- times 1))))

(define (square x) (* x x))
((repeated square 2) 5)

;Ex 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (n-smooth f n)
  (repeated (smooth f) n))

;Ex 1.46
(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (define (iteration guess)
      (if (good-enough? guess)
          guess
          (iteration (improve-guess guess))))
    (iteration guess)))

(define (fixed-point-alt f guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve-guess guess)
    (f guess))
  ((iterative-improve good-enough? improve-guess) guess))

(fixed-point cos 1.0)
(fixed-point-alt cos 1.0)
