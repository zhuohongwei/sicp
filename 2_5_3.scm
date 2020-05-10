#lang sicp

;(define (put op tag proc) (error "Unimplemented Operation PUT"))
;(define (get op tag) (error "Unimplemented Operation GET"))
;(define (apply-generic op . args) (error "Unimplemented Operation APPLY-GENERIC"))

(define lookup-table '())

(define (eq-key? key1 key2)
  (cond ((and (symbol? key1) (symbol? key2)) (eq? key1 key2))
        ((and (null? key1) (null? key2)) true)
        ((and (pair? key1) (pair? key2)) (and (eq-key? (car key1) (car key2)) (eq-key? (cdr key1) (cdr key2))))
        (else false)))

(define (get-value key)
  (define (get-value-iter remaining-rows)
    (cond ((null? remaining-rows) false)
          (else (let ((row (car remaining-rows)))
                  (cond ((null? row) (error "Invalid Row"))
                        ((eq-key? (car row) key) (cdr row))
                        (else (get-value-iter (cdr remaining-rows))))))))
  (get-value-iter lookup-table))

(define (put-value key value)
  (define (put-value-iter remaining-rows)
    (cond ((null? remaining-rows) (list (cons key value)))
          (else (let ((row (car remaining-rows)))
                  (cond ((null? row) (error "Invalid Row"))
                        ((eq-key? (car row) key) (cons (cons key value) (cdr remaining-rows)))
                        (else (cons row (put-value-iter (cdr remaining-rows)))))))))
  (set! lookup-table (put-value-iter lookup-table)))

(define (get op type)
  (let ((proc
         (if (pair? type)
             (get-value (append (list op) type))
             (get-value (list op type)))))
    (if proc
        proc
        (error "Operation Not Supported" op type))))

(define (put op type proc)
  (if (pair? type)
      (put-value (append (list op) type) proc) 
      (put-value (list op type) proc)))

(define (apply-generic op . args)
  (let ((arg-types (map car args))
        (arg-values (map cdr args)))    
    (apply (get op arg-types) arg-values)))

(define (=zero? x)
  (apply-generic '=zero? x))

(define (add first second)
  (apply-generic 'add first second))

(define (sub first second)
  (apply-generic 'sub first second))

(define (mul first second)
  (apply-generic 'mul first second))

(define (div first second)
  (apply-generic 'div first second))

(define (negate x)
  (apply-generic 'negate x))

(define (attach-tag tag object)
  (cons tag object))

(define (type-tag tagged)
  (car tagged))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define same-variable? eq?)
  (define variable? symbol?)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else (adjoin-term (make-term (order t1)
                                                 (add (coeff t1) (coeff t2)))
                                      (add-terms (rest-terms L1) (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2) (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (+ (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))

  ; ex 2.87
  (define (=zero? polynomial)
    (null? (term-list polynomial)))

  ; ex 2.88
  (define (negate-term term)
    (make-term (order term) (negate (coeff term))))
  
  (define (negate-poly polynomial)
    (make-poly (variable polynomial)
               (map negate-term (term-list polynomial))))
  
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  ; ex 2.94
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (map (lambda (term-list) (make-poly (variable p1) term-list))
             (div-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))

  (define (sub-terms L1 L2)
    (let ((result-terms (add-terms L1 (map negate-term L2))))
      (define (filter-zero-terms terms)
        (cond ((null? terms) '())
              ((apply-generic '=zero? (coeff (car terms))) (filter-zero-terms (cdr terms)))
              (else (cons (car terms) (filter-zero-terms (cdr terms))))))
      (filter-zero-terms result-terms)))
     
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((new-term (make-term new-o new-c)))
                  (let ((multiplied (mul-term-by-all-terms new-term L2)))
                    (let ((new-dividend (sub-terms L1 multiplied))) 
                      (let ((rest-of-result 
                           (div-terms
                            new-dividend
                            L2)))
                        (list (cons new-term (car rest-of-result)) (cadr rest-of-result)))))))))))

  
  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))

  (define (gcd-terms L1 L2)
    (if (empty-termlist? L2)
        L1
        (gcd-terms L2 (remainder-terms L1 L2))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: GCD-POLY" (list p1 p2))))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? 'polynomial =zero?)
  (put 'negate 'polynomial
       (lambda (p) (tag (negate-poly p))))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  'done)

; ex 2.89
(define (install-dense-package)
  ; internal procedures
  (define (coeff-dense term) (cadr term))
  (define (order-dense term) (cdr term))
  (define (adjoin-term-dense term term-list)
    (cons (coeff-dense term) term-list))
  (define (the-empty-termlist-dense) '())
  (define (first-term-dense term-list) (make-term-dense (- (length term-list) 1) (car term-list)))
  (define (rest-terms-dense term-list) (cdr term-list))
  (define (empty-termlist-dense? term-list)
    (null? term-list))
  (define (make-term-dense order coeff) (list order coeff))
  ;; interface to the rest of the system
  (define (tag t) (attach-tag 'dense t))
  (put 'coeff 'dense coeff-dense)
  (put 'order 'dense order-dense)
  (put 'adjoin-term '(dense dense)
       (lambda (term term-list) (tag (adjoin-term-dense term term-list))))
  (put 'the-empty-termlist 'dense
       (lambda () (tag (the-empty-termlist-dense))))
  (put 'first-term 'dense
       (lambda (L) (tag (first-term-dense L))))
  (put 'rest-terms 'dense
       (lambda (L) (tag (rest-terms-dense L))))
  (put 'empty-termlist? 'dense empty-termlist-dense?)
  (put 'make-term 'dense
       (lambda (order coeff) (tag (make-term-dense order coeff))))
  'done)

(define (install-sparse-package)
  ; internal procedures
  (define (adjoin-term-sparse term term-list)
    (if (=zero? (coeff-sparse term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist-sparse) '())
  (define (first-term-sparse term-list)
    (car term-list))
  (define (rest-terms-sparse term-list)
    (cdr term-list))
  (define (empty-termlist-sparse? term-list)
    (null? term-list))
  (define (make-term-sparse order coeff)
    (list order coeff))
  (define (order-sparse term) (car term))
  (define (coeff-sparse term) (cadr term))
  
  ;; interface to the rest of the system
  (define (tag t) (attach-tag 'sparse t))
  (put 'coeff 'sparse coeff-sparse)
  (put 'order 'sparse order-sparse)
  (put 'adjoin-term '(sparse sparse)
       (lambda (term term-list) (tag (adjoin-term-sparse term term-list))))
  (put 'the-empty-termlist 'sparse
       (lambda () (tag (the-empty-termlist-sparse))))
  (put 'first-term 'sparse
       (lambda (L) (tag (first-term-sparse L))))
  (put 'rest-terms 'sparse
       (lambda (L) (tag (rest-terms-sparse L))))
  (put 'empty-termlist? 'sparse empty-termlist-sparse?)
  (put 'make-term 'sparse
       (lambda (order coeff) (tag (make-term-sparse order coeff))))
  'done)

; external procedures to compose term list for a polynomial
; either with dense or sparse representations
(define (make-term-sparse order coeff)
  ((get 'make-term 'sparse) order coeff))
(define (make-term-dense order coeff)
  ((get 'make-term 'sparse) order coeff))

(define (the-empty-termlist-sparse)
  ((get 'the-empty-termlist 'sparse)))
(define (the-empty-termlist-dense)
  ((get 'the-empty-termlist 'dense)))

; modified package to support mixed term list representations
(define (install-polynomial-package-with-multiple-termlist-representations)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define same-variable? eq?)
  (define variable? symbol?)
  
  (define (coeff term)
    (apply-generic 'coeff term))
  (define (order term)
    (apply-generic 'order term))
  (define (adjoin-term term term-list)
    (apply-generic 'adjoin-term term term-list))

  (define (first-term L)
    (apply-generic 'first-term L))
  (define (rest-terms L)
    (apply-generic 'rest-terms L))

  (define (empty-termlist? L)
    (apply-generic 'empty-termlist? L))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))

  (define (add-terms L1 L2)
    (let ((make-term (get 'make-term (type-tag L1))))
      (cond ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
             (let ((t1 (first-term L1))
                   (t2 (first-term L2)))
               (cond ((> (order t1) (order t2))
                      (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                     ((< (order t1) (order t2))
                      (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                     (else (adjoin-term (make-term (order t1)
                                                   (add (coeff t1) (coeff t2)))
                                        (add-terms (rest-terms L1) (rest-terms L2))))))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        ((get 'the-empty-termlist (type-tag L1)))
        (add-terms (mul-term-by-all-terms (first-term L1) L2) (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (let ((make-term (get 'make-term (type-tag t1)))
          (the-empty-termlist (get 'the-empty-termlist (type-tag t1))))
      (if (empty-termlist? L)
          (the-empty-termlist)
          (let ((t2 (first-term L)))
            (adjoin-term (make-term (+ (order t1) (order t2))
                                    (mul (coeff t1) (coeff t2)))
                         (mul-term-by-all-terms t1 (rest-terms L)))))))

  (define (=zero? polynomial)
    (let ((coefficients (map coeff (term-list polynomial))))
      (define (all-zero? numbers)
        (if (null? numbers)
            true
            (and (=zero? (car numbers)) (all-zero? (cdr numbers)))))
      (all-zero? coefficients)))

  (define (negate-poly polynomial)
    (make-poly (variable polynomial)
               (map (lambda (term)
                      (let ((make-term (get 'make-term (type-tag term))))
                        (make-term (order term) (negate (coeff term)))))
                    (term-list polynomial))))
  
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  ; ex 2.91
  (define (div-poly p1 p2)
    (if (same-variable? p1 p2)
        (make-poly (variable p1)
                   (div-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))

  (define (negate-term term)
    (let ((make-term (get 'make-term (type-tag term))))
      (make-term (order term) (negate (coeff term)))))
  
  (define (sub-terms L1 L2)
    (add-terms L1 (map negate-term L2)))
  
  (define (div-terms L1 L2)
    (let ((the-empty-termlist (get 'the-empty-termlist (type-tag L1)))
          (make-term (get 'make-term (type-tag L1))))
      (if (empty-termlist? L1)
          (list (the-empty-termlist) (the-empty-termlist))
          (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (if (> (order t2) (order t1))
                (list (the-empty-termlist) L1)
                (let ((new-c (div (coeff t1) (coeff t2)))
                      (new-o (- (order t1) (order t2))))
                  (let ((new-term (make-term new-o new-c)))
                    (let ((new-dividend (sub-terms L1 (mul-term-by-all-terms new-term L2))))
                      (display new-dividend)
                      (newline)
                      (let ((rest-of-result
                             (div-terms
                              new-dividend
                              L2)))
                        (list (cons new-term (car rest-of-result)) (cdr rest-of-result)))))))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? 'polynomial =zero?)
  (put 'negate 'polynomial
       (lambda (p) (tag (negate-poly p))))
  'done)

(define (make-polynomial var terms) ((get 'make 'polynomial) var terms))

; ex 2.93

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cons n d))
    ;(let ((g (gcd n d)))
    ;  (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (add (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'negate 'scheme-number
       (lambda (x) (tag (* -1 x))))
  (put '=zero? 'scheme-number
       (lambda (x) (= 0 x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)
(install-rational-package)
(install-polynomial-package)
 
(define p1 (make-polynomial 'x (list (list 2 (make-scheme-number 1)) (list 0 (make-scheme-number 1)))))
(define p2 (make-polynomial 'x (list (list 3 (make-scheme-number 1)) (list 0 (make-scheme-number 1)))))
(define rf (make-rational p2 p1))
(add rf rf)

; ex 2.94 
(define p3 (make-polynomial 'x (list (list 4 (make-scheme-number 1)) (list 3 (make-scheme-number -1)) (list 2 (make-scheme-number -2)) (list 1 (make-scheme-number 2)))))
(define p4 (make-polynomial 'x (list (list 3 (make-scheme-number 1)) (list 1 (make-scheme-number -1)))))
(apply-generic 'greatest-common-divisor p3 p4)