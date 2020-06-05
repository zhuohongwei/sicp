#lang sicp

(define (display-line x) (newline) (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

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

(define (add-streams s1 s2) (stream-map + s1 s2))

; ex 3.53

(define (display-n-elements n s)
  (cond ((= n 0) 'done)
        (else
         (display-line (stream-car s))     
         (display-n-elements (- n 1) (stream-cdr s)))))

(define s (cons-stream 1 (add-streams s s)))

(display-n-elements 10 s)

; ex 3.54

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

(display-n-elements 5 factorials)

; ex 3.55

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(display-n-elements 5 (partial-sums integers))


