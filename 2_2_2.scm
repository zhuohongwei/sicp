#lang sicp

;ex 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;ex 2.27
(define (deep-reverse x)
  (define (deep-reverse-iter x result)
    (cond ((null? x) result)
          ((not (pair? x)) x)
          (else (deep-reverse-iter (cdr x) (cons (deep-reverse-iter (car x) nil) result)))))
  (deep-reverse-iter x nil))

(define z (list (list 1 2) (list 3 4)))
(deep-reverse z)

;ex 2.28
(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) x)
        (else (cons (fringe (car x)) (fringe (cdr x))))))

(fringe z)
