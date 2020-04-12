#lang sicp
;ex 2.17
(define (last-pair list)
  (define (last-pair-iter previous list)
    (if (null? list)
        previous
        (last-pair-iter (car list) (cdr list))))
  (last-pair-iter nil list))

(last-pair (list 1 2 3 4 5))
(last-pair nil)

;ex 2.18
(define (reverse list)
  (define (reverse-iter list result)
    (if (null? list)
        result
        (reverse-iter (cdr list) (cons (car list) result))))
  (reverse-iter list nil))

(reverse (list 1 2 3 4 5))

;ex 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coin-values)
  (null? coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (first-denomination coin-values)
  (car coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(cc 100 us-coins)

;ex 2.20
(define (same-parity first . rest)
  (define (even? x) (= (remainder x 2) 0))
  (define (odd? x) (not (even? x)))
  (define (both-odd-or-even? x y) (or (and (odd? x) (odd? y)) (and (even? x) (even? y))))
  (define (same-parity-iter rest)
    (cond ((null? rest) nil)
          ((both-odd-or-even? first (car rest)) (cons (car rest) (same-parity-iter (cdr rest))))
          (else (same-parity-iter (cdr rest)))))
  (cons first (same-parity-iter rest)))

(same-parity 1 2 3 4 5 6 7 8 9)

;ex 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (expt (car items) 2) (square-list (cdr items)))))

(define (square-list-alt items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4 5))
(square-list-alt (list 1 2 3 4 5))

;ex 2.22
(define (square-list-iterative items)
  (define (iter items answer)
    (if (null? items)
        answer
        (let ((term (expt (car items) 2))
              (rest (cdr items)))
          (iter rest (cons term answer)))))
  (reverse (iter items nil)))

(square-list-iterative (list 1 2 3 4 5))

;ex 2.23
(define (for-each f elements)
  (if (null? elements)
      nil
      (let ((unused-ret-value (f (car elements))))
        (for-each f (cdr elements)))))


(for-each (lambda (x) (newline)
            (display x))
          (list 57 321 88))
