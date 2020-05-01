#lang sicp

(define variable? symbol?)
(define same-variable? eq?)

(define (get op type)
  (lambda (operands var) (error "Unsupported Operation Get" operands var)))

(define (put op type proc)
  (error "Unsupported Operation Put"))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; ex 2.73
; b
(define (install-deriv-sum)
  (define addend car)
  (define augend cadr)
  (define (make-sum first second) (error "Unimplemented Operation Sum" first second))
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (put 'deriv '+ deriv-sum)
  (put 'deriv 'make-sum make-sum)
  'done)

(define (install-deriv-product)
  (define multiplier car)
  (define multiplicand cadr)
  (define (make-product first second) (error "Unimplemented Operation Product" first second))
  (define (deriv-product operands var)
    ((get 'deriv 'make-sum) (make-product
                             (multiplier operands)
                             (deriv (multiplicand operands) var))
                            (make-product
                             (deriv (multiplier operands) var)
                             (multiplicand operands))))
  (put 'deriv '* deriv-product)
  (put 'deriv 'make-product make-product)
  'done)

;c
(define (install-deriv-exponentiation)
  (define base car)
  (define exponent cadr)
  (define (make-exponentiation base exponent) (error "Unimplemented Operation Exponentiation" base exponent))
  (define (deriv-exponentiation operands var)
    (let ((make-product (get 'deriv 'make-product))) 
      (make-product (make-product (exponent operands)
                                  (make-exponentiation (base operands) (- (exponent operands) 1)))
                    (deriv (base operands) var))))
  (put 'deriv '** deriv-exponentiation)
  (put 'deriv 'make-exponentiation make-exponentiation)
  'done)
    