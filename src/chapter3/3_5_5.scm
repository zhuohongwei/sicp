#lang sicp

(define (display-line x) (newline) (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (display-n-elements n s)
  (cond ((= n 0) 'done)
        (else
         (display-line (stream-car s))     
         (display-n-elements (- n 1) (stream-cdr s)))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; ex 3.81

(define (random-numbers requests)
  (define (random-numbers-iter requests starting-value)
    (define (random-update value)
      (random-in-range value (+ value 10)))
    (if (stream-null? requests)
        'end-of-stream
        (let ((request (stream-car requests)))
          (cond ((eq? request 'generate)
                 (let ((generated-value (random-update starting-value))) 
                   (cons-stream generated-value (random-numbers-iter (stream-cdr requests) generated-value))))
                ((and (pair? request) (eq? (car request) 'reset)) 
                 (random-numbers-iter (stream-cdr requests) (cdr request)))))))
  (random-numbers-iter requests 0))

(define (request-stream)
  (define (make-requests-iter n) 
    (cons-stream
     (if (= (remainder n 10) 0)
         (cons 'reset 0)
         'generate)
     (make-requests-iter (+ 1 n))))
  (make-requests-iter 1))

(display-n-elements 100 (random-numbers (request-stream)))

; ex 3.82

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral predicate x1 x2 y1 y2)
  (let ((total-area (* (abs (- y2 y1)) (abs (- x2 x1)))))
    (define (experiment)
      (let ((randX (random-in-range x1 x2))
            (randY (random-in-range y1 y2)))
        (predicate randX randY)))
    (define (monte-carlo-iter trials)
      (cons-stream (* (monte-carlo trials experiment) total-area)
                   (monte-carlo-iter (+ 1 trials))))
    (monte-carlo-iter 1)))

(define (estimate-pi cx cy radius)
  (let ((x1 (- cx radius))
        (x2 (+ cx radius))
        (y1 (- cy radius))
        (y2 (+ cy radius))
        (predicate (lambda (x y) (not (> (+ (expt (- x cx) 2) (expt (- y cy) 2)) (expt radius 2))))))
    (let ((estimated-area (estimate-integral predicate x1 x2 y1 y2)))
      (stream-map (lambda (a) (/ a (expt radius 2.0))) estimated-area))))

(display-n-elements 1000 (estimate-pi 5 5 10))
         
   
   





