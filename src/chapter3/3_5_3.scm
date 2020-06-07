#lang sicp

(define (display-line x) (newline) (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

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

(define (display-n-elements n s)
  (cond ((= n 0) 'done)
        (else
         (display-line (stream-car s))     
         (display-n-elements (- n 1) (stream-cdr s)))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(define (square n)
  (* n n))

; ex 3.64

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit stream tolerance)
  (define (stream-limit-iter previous stream)
    (cond ((and previous (not (stream-null? stream)))
           (if (< (abs (- previous (stream-car stream))) tolerance)
               (stream-car stream)
               (stream-limit-iter (stream-car stream) (stream-cdr stream))))
          ((stream-null? stream)
           (error "END OF STREAM: NO VALUE WITHIN TOLERANCE"))
          ((not previous) 
           (stream-limit-iter (stream-car stream) (stream-cdr stream)))))
  (stream-limit-iter false stream))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
          
(sqrt 10 0.1)

; ex 3.65

(define (natural-log-two-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (natural-log-two-summands (+ n 1)))))

(define natural-log-two-stream
  (partial-sums (natural-log-two-summands 1)))


(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(display-n-elements 10
 (accelerated-sequence euler-transform natural-log-two-stream))

; ex 3.66

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(display-n-elements 20 (pairs integers integers))

; (1, 1) . promise
; (1, 1), (1, 2) . promise
; (1, 1), (1, 2), (2, 2) . promise
; (1, 1), (1, 2), (2, 2), (1, 3) . promise
; ....

; ex 3.67

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
     (all-pairs (stream-cdr s) t))))

(display-n-elements 10 (all-pairs integers integers))
