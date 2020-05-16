#lang sicp

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

;(put 'hello 'world (lambda () (display "Hello World")))
;(put 'hello 'universe (lambda () (display "Hello Universe")))
;(put 'hello 'galaxy (lambda () (display "Hello Galaxy")))
;(put 'hello '(world universe galaxy) (lambda () (display "Hello World Universe Galaxy")))

;(newline)
;((get 'hello 'world))
;(newline)
;((get 'hello '(world universe galaxy)))
;(put 'hello 'world (lambda () (display "Hello World Modified")))
;(newline)
;((get 'hello 'world))
;(newline)
;lookup-table


