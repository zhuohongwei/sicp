#lang sicp

;ex 2.1

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (define (neg? x) (< x 0))
    (cond ((and (neg? n) (neg? d)) (cons (/ (abs n) g) (/ (abs d) g)))
          ((neg? n) (cons (/ n g) (/ d g)))
          ((neg? d) (cons (* (/ n g) -1) (/ (abs d) g)))
          (else (cons (/ n g) (/ d g))))))

(define (print-rat x)
  (newline)
  (display (car x))
  (display "/")
  (display (cdr x)))

(print-rat (make-rat -1 3))
(print-rat (make-rat 1 -3))
(print-rat (make-rat -1 -3))
(print-rat (make-rat 1 3))
(print-rat (make-rat -2 6))
(print-rat (make-rat 2 -6))
(print-rat (make-rat -2 -6))