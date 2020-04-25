#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; ex 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set '(1 2 3 4 5) '(3 4 5 6 7))

; ex 2.60
(define (adjoin-set-alt x set)
  (cons x set))

(define (union-set-alt set1 set2)
  (append set1 set2))

; ex 2.61
(define (adjoin-set-ordered x set)
  (if (or (null? set) (< x (car set)))
      (cons x set)
      (cons (car set) (adjoin-set-ordered x (cdr set)))))

(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-set-ordered (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set-ordered (cdr set1) set2)))
        ((> (car set1) (car set2)) (cons (car set2) (union-set-ordered set1 (cdr set2))))))

(adjoin-set-ordered 5 '(1 2 3 4 5 7 8 10))
(union-set-ordered '(1 2 3 4 5) '(3 4 5 6 7 8))