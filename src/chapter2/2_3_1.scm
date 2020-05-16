#lang sicp

;ex 2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

;ex 2.54
(define (equal? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((and (not (pair? x)) (not (pair? y))) (eq? x y))
        ((and (pair? x) (pair? y)) (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
        (else #f)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

;ex 2.55
(car ''abracadabra)