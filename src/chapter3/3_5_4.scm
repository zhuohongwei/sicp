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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (display-n-elements n s)
  (cond ((= n 0) 'done)
        (else
         (display-line (stream-car s))     
         (display-n-elements (- n 1) (stream-cdr s)))))

; ex 3.77

(define (integral delayed-integrand initial-value dt)
  (cons-stream
   initial-value
   (let ((integrand (force delayed-integrand)))
     (if (stream-null? integrand)
         the-empty-stream
         (integral (delay (stream-cdr integrand))
                   (+ (* dt (stream-car integrand)) initial-value)
                   dt)))))

; ex 3.78

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

; ex 3.78

(define (generalized-solve-2nd dt y0 dy0 f)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (f y dy))
  y)

; ex 3.79

(define (RLC R L C dt)
  (lambda (initial-capacitor-voltage initial-inductor-current)
    (define capacitor-voltage (integral
                               (delay (scale-stream inductor-current (/ -1 C)))
                               initial-capacitor-voltage dt))
    (define inductor-current (integral
                              (delay (add-streams
                                      (scale-stream inductor-current (/ (* -1 R) L))
                                      (scale-stream capacitor-voltage (/ 1 L))))
                              initial-inductor-current dt))
    (cons capacitor-voltage inductor-current)))
    
(define rlc-circuit (RLC 1 1 0.2 0.1))
(define vi-streams (rlc-circuit 10 0))

(display-n-elements 10 (car vi-streams))
(display-n-elements 10 (cdr vi-streams))
