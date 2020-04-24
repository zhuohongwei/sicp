#lang sicp

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

;ex 2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation base exponent)
  (cond
    ((=number? exponent 0) 1)
    ((=number? exponent 1) base)
    ((and (number? base) (number? exponent)) (expt base exponent))
    (else (list '** base exponent))))
        
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv (multiplicand exp) var))
                         (make-product (deriv (multiplier exp) var)
                                       (multiplicand exp))))
        
        ((exponentiation? exp) (make-product (make-product (exponent exp)
                                                           (make-exponentiation (base exp) (- (exponent exp) 1)))
                                             (deriv (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))


(deriv '(** x 10) 'x)

;ex 2.57
(define (augend-alt s)
  (let ((rest (cddr s)))
    (cond ((null? rest) 0)
          ((pair? rest) (make-sum (car rest) (augend-alt  (append '(+) rest))))
          (else rest))))

(define (multiplicand-alt s)
  (let ((rest (cddr s)))
    (cond ((null? rest) 1)
          ((pair? rest) (make-product (car rest) (multiplicand-alt (append '(*) rest))))
          (else rest))))

(define (deriv-alt exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv-alt (addend exp) var)
                              (deriv-alt (augend-alt exp) var)))
        ((product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv-alt (multiplicand-alt exp) var))
                         (make-product (deriv-alt (multiplier exp) var)
                                       (multiplicand-alt exp))))
        
        ((exponentiation? exp) (make-product (make-product (exponent exp)
                                                           (make-exponentiation (base exp) (- (exponent exp) 1)))
                                             (deriv-alt (base exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

(deriv-alt '(* x y (+ x 3)) 'x)
(deriv-alt '(* (* x y) (+ x 3)) 'x)

;ex 2.58

;a

(define (make-sum-infix a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product-infix m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum-infix? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend-infix s) (car s))

(define (product-infix? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier-infix p) (car p))

(define (exponentiation-infix? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base-infix x)
  (car x))

(define (make-exponentiation-infix base exponent)
  (cond
    ((=number? exponent 0) 1)
    ((=number? exponent 1) base)
    ((and (number? base) (number? exponent)) (expt base exponent))
    (else (list base '**  exponent))))

(define (deriv-infix exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum-infix? exp) (make-sum-infix (deriv-infix (addend-infix exp) var)
                              (deriv-infix (augend exp) var)))
        ((product-infix? exp) (make-sum-infix
                         (make-product-infix (multiplier-infix exp)
                                       (deriv-infix (multiplicand exp) var))
                         (make-product-infix (deriv-infix (multiplier-infix exp) var)
                                       (multiplicand exp))))
        ((exponentiation-infix? exp) (make-product-infix (make-product-infix (exponent exp)
                                                           (make-exponentiation-infix (base-infix exp) (- (exponent exp) 1)))
                                             (deriv-infix (base-infix exp) var)))
        (else (error "unknown expression type: DERIV" exp))))

(deriv-infix '(x + (3 * (x + (y + 2)))) 'x)