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

; ex 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1)) (s2car (stream-car s2)))
           (cond ((< s1car s2car) (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car) (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

(display-n-elements 10 S)

; ex 3.58

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(display-n-elements 20 (expand 1 7 10))
(display-n-elements 20 (expand 3 8 10))

; ex 3.59

;a
(define (div-streams s1 s2) (stream-map / s1 s2))

(define (integrate-series power-series)
  (div-streams power-series integers))

(display-n-elements 20 (integrate-series ones))

;b

(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

; ex 3.60

(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) (mul-series (stream-cdr s1) s2))))

(define sine-*-sine-plus-cosine-*-cosine (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))

(display-n-elements 1 sine-*-sine-plus-cosine-*-cosine)

; ex 3.61

(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1)))

(display-n-elements 10 (invert-unit-series integers))

; ex 3.62

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "DENOM FIRST TERM IS ZERO" (stream-car s2))
      (mul-series s1 (invert-unit-series s2))))

(define tangent-series (div-series sine-series cosine-series))

(display-n-elements 10 tangent-series)
