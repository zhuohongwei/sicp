#lang sicp

; ex 2.81
; c

(define type-tag car)
(define contents cdr)

(define (get op type) (error "Unimplemented Operation GET"))
(define (get-coercion type1 type2) (error "Unimplemented Operation GET"))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags)))) 
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))

; ex 2.83

(define (make-rational n d) (error "Unimplemented Operation MAKE-RATIONAL"))
(define (make-real x) (error "Unimplemented Operation MAKE-REAL"))
(define (make-from-real-imag r i) (error "Unimplemented Operation MAKE-FROM-REAL-IMAG")) 
(define (put op type proc) (error "Unimplemented Operation PUT"))

(define (install-raise-integer)
  (define (raise x)
    (make-rational x 1))
  (put 'raise '(scheme-number) raise)
  'done)

(define (install-raise-rational)
  (define (raise x)
    (make-real x))
  (put 'raise '(rational) raise)
  'done)

(define (install-raise-real)
  (define (raise x)
    (make-from-real-imag x 0))
  (put 'raise '(real) raise)
  'done)

(define (raise x)
  (apply-generic 'raise x))

; ex 2.84

(define (raise-to x type)
  (let ((x-type (type-tag x)))
    (let ((raise-x (get 'raise x-type)))
      (cond ((eq? x-type type) x)
            (raise-x (raise-to (raise-x x) type))
            (else false)))))

(define (apply-generic-with-raise op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags)))) 
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((a1-raised (raise-to a1 type2))
                      (a2-raised (raise-to a2 type1)))
                  (cond (a1-raised
                         (apply-generic-with-raise op a1-raised a2))
                        (a2-raised
                         (apply-generic-with-raise op a1 a2-raised))
                        (else (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))

; ex 2.85

(define (make-scheme-number x) (error "Unimplemented Operation MAKE-SCHEME-NUMBER"))
(define numer car)
(define denom cdr)

(define (install-project-complex)
  (define (project x)
    (make-real (real-part x)))
  (put 'project '(complex) project)
  'done)

(define (install-project-real)
  (define (project x)
    (let ((sign (if (< x 0) -1 1)))
      (let ((numer (* sign x 10000)) ;assuming a precison of 4 decimal places
            (denom 10000))
        (make-rational (numer denom)))))
  (put 'project '(real) project)
  'done)

(define (install-project-rational)
  (define (project x)
    (make-scheme-number (quotient (numer x) (denom x))))
  (put 'project '(rational) project)
  'done)

(define (project x)
  (apply-generic-with-raise 'project x))

(define (equ? x y)
  (apply-generic-with-raise 'equ? x y))

(define (drop x)
  (let ((projected (project x)))
    (if (equ? (raise projected) x)
        (drop projected)
        x)))







