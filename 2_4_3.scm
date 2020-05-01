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

; c
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

; ex 2.74
; a, b
(define (attach-division division object)
  (cons division object))

(define (put-division-op op division proc)
  (error "Unimplemented Operation Put"))

(define (get-division-op op division)
  (error "Unimplemented Operation Get"))
 
(define (install-sales-division-package)
  (define (save-record record) (error "Unimplemented Operation Save" record))
  (define (all-records) (error "Unimplemented Operation"))
  (define (make-record name address salary)
    (list name address salary))
  (define record-name car)
  (define record-address cadr)
  (define record-salary caddr)
  (define (find-record records name)
    (cond ((null? records) #f)
          ((= (record-name (car records)) name) (car records))
          (else (find-record (cdr records) name))))
  (define (tag object) (attach-division 'sales object))
  (put-division-op 'add-record 'sales
       (lambda (name address salary)
         (let ((record (make-record name address salary)))
           (save-record record)
           (tag record))))
  (put-division-op 'find-record 'sales
       (lambda (records name)
         (let ((record (find-record records name)))
           (tag record))))
  (put-division-op 'get-name 'sales record-name)
  (put-division-op 'get-address 'sales record-address)
  (put-division-op 'get-salary 'sales record-salary)
  (put-division-op 'get-records-file 'sales
                   (lambda () (tag (all-records))))
  'done)

; c
(define divisions '(sales operations engineering))
;(define division-files (map (lambda (division) ((get-division-op 'get-records-file division))) divisions))

(define (apply-division-op op . args)
  (let ((division-object (car args)))
    (let ((division (car division-object)))
      (let ((proc (get-division-op op division)))
        (if proc
            (apply proc (cons (cdr division-object) (cdr args)))
            (error "Operation Not Found" op))))))

(define (find-employee-record division-files name)
  (if (null? division-files)
      #f
      (let ((record (apply-division-op 'find-record (car division-files) name)))
        (if record
            record
            (find-employee-record (cdr division-files) name)))))


; ex 2.75

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

    