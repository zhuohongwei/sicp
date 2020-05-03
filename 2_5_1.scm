#lang sicp

;ex 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

;ex 2.79, 2.80

(define (put op type proc) (error "Unimplemented Operation PUT"))
(define (apply-generic op . args) (error "Unimplemented Operation APPLY-GENERIC"))

(define (install-scheme-number-equ)
  (define equ? =)
  (define (=zero? n) (= n 0))
  (put 'equ? '(scheme-number scheme-number) equ?)
  (put '=zero? '(scheme-number scheme-number) =zero?)
  'done)

(define (install-rational-number-equ)
  (define (numer rational) (car rational))
  (define (denom rational) (cdr rational))
  (define (make-rat numerator denominator) (cons numerator denominator)) 
  (define (equ? first second)
    (= (/ (numer first) (denom first))
       (/ (numer second) (denom second))))
  (define (=zero? n) (equ? n (make-rat 0 0)))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational rational) =zero?)
  'done)

(define (install-complex-number-equ)
  (define (make-from-real-imag x y) (error "Unimplemented Operation MAKE-FROM-REAL-IMAG"))
  (define (equ? first second)
    (let ((real-first (apply-generic 'real-part first))
          (real-second (apply-generic 'real-part second))
          (img-first (apply-generic 'img-part first))
          (img-second (apply-generic 'img-part second)))
      (and (= real-first real-second) (= img-first img-second))))
  (define (=zero? n) (equ? n (make-from-real-imag 0 0)))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex complex) =zero?)
  'done)
          
(define (equ? first second) (apply-generic 'equ? first second))
(define (=zero? n) (apply-generic '=zero? n))