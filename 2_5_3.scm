#lang sicp

(define (put op tag proc) (error "Unimplemented Operation PUT"))
(define (get op tag) (error "Unimplemented Operation GET"))
(define (apply-generic op . args) (error "Unimplemented Operation APPLY-GENERIC"))

(define (=zero? x)
  (apply-generic '=zero? x))

(define (add first second)
  (apply-generic 'add first second))

(define (mul first second)
  (apply-generic 'mul first second))

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
    (let ((coefficients (map coeff (term-list polynomial))))
      (define (all-zero? numbers)
        (if (null? numbers)
            true
            (and (=zero? (car numbers)) (all-zero? (cdr numbers)))))
      (all-zero? coefficients)))

  ; ex 2.88
  (define (negate-poly polynomial)
    (make-poly (variable polynomial)
               (map (lambda (term) (make-term (order term) (negate (coeff term)))) (term-list polynomial))))
  
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? 'polynomial =zero?)
  (put 'negate 'polynomial
       (lambda (p) (tag (negate-poly p))))
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
  (put 'adjoin-term ('dense 'dense)
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

(define (install-dense-sparse)
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
  (put 'adjoin-term ('sparse 'sparse)
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

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? 'polynomial =zero?)
  (put 'negate 'polynomial
       (lambda (p) (tag (negate-poly p))))
  'done)

  