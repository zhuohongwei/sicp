#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;(define (stream-map proc s)
 ; (if (stream-null? s)
  ;    the-empty-stream
   ;   (cons-stream (proc (stream-car s))
    ;               (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

; ex 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


; ex 3.51

(define (display-line x) (newline) (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))
                  
(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
; prints 0

(stream-ref x 5)
; prints 1
; prints 2
; prints 3
; prints 4
; prints 5

(stream-ref x 7)
; prints 6
; prints 7

; ex 3.52

(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define sum 0)

(define (accum x) (set! sum (+ x sum)) sum)

(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
; seq is 1 . promise

(newline)
(display "Sum:")
(display-line sum)
(newline)
; sum is 1

(define y (stream-filter even? seq))
; y is 6 . promise

; 1 3 6 . promise

(newline)
(display "Sum:")
(display-line sum)
(newline)
; sum is 6

(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
; 1 3 6 10 . promise

(newline)
(display "Sum:")
(display-line sum)
(newline)
; sum is 10

(stream-ref y 7)
; prints 136

; 6 10 15 21 28 36 45 55 66 78 91 105 120 136 . promise 

(newline)
(display "Sum:")
(display-line sum)
(newline)
; sum is 136

(display-stream z)
; prints 10 15 45 55 105 120 190 210

; 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210 

(newline)
(display "Sum:")
(display-line sum)
(newline)
; sum is 210
