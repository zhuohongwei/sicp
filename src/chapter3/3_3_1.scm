#lang sicp

; ex 3.12

(define (append! x y)
  (set-cdr! (last-pair x) y) x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z
(cdr x)

(define w (append! x y))
w
(cdr x)

; ex 3.14

(define (mystery x)
  (define (loop x y)
    (newline)
    (display x)
    (display "|")
    (display y)
    (if (null? x) y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define e (mystery v))
v
e

; ex 3.16

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x)) 1)))

(define three-pairs (cons 'a (cons 'b (list 'c))))
(count-pairs three-pairs)

(define k (list 'b))
(define four-pairs (cons 'a (cons k k)))
(count-pairs four-pairs)

(define j (list 'c))
(define m (cons j j))
(define n (cons m m))  
(define seven-pairs n)
(count-pairs seven-pairs)

; ex 3.17

(define count-pairs-correctly
  (let ((seen? (lambda (p) false)))
    (lambda (x)
      (if (seen? x)
          0
          (begin
            (let ((old-seen? seen?))
              (set! seen? (lambda (p)
                               (if (eq? p x)
                                   true
                                   (old-seen? p)))))
            (if (not (pair? x))
                0
                (+ (count-pairs-correctly (car x))
                   (count-pairs-correctly (cdr x)) 1)))))))

(count-pairs-correctly three-pairs)
(count-pairs-correctly four-pairs)
(count-pairs-correctly seven-pairs)

; ex 3.18

(define (cycle? x)
  (define seen '())
  (define (contains? seen-list element)
    (if (null? seen-list)
        false
        (if (eq? (car seen-list) element)
            true
            (contains? (cdr seen-list) element))))
  (define (cycle-iter? x)
    (if (contains? seen x)
        true
        (begin (set! seen (cons x seen))
               (if (pair? x)
                   (cycle-iter? (cdr x))
                   false))))
  (cycle-iter? x))

(cycle? seven-pairs)

(set-cdr! seven-pairs seven-pairs)

(cycle? seven-pairs)

; ex 3.19

(define (cycle-constant-space? x)
  (cond ((not (pair? x)) false)
        ((eq? (car x) 'seen) true)
        (else
         (begin (set-car! x 'seen)
                (cycle-constant-space? (cdr x))))))

(cycle-constant-space? four-pairs)
(cycle-constant-space? seven-pairs)
      



