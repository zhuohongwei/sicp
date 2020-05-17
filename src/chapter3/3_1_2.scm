#lang sicp

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

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

; ex 3.5
(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (let ((total-area (* (abs (- y2 y1)) (abs (- x2 x1)))))
    (define (experiment)
      (let ((randX (random-in-range x1 x2))
            (randY (random-in-range y1 y2)))
        (predicate randX randY)))
    (* (monte-carlo trials experiment) total-area)))

(define (estimate-pi cx cy radius trials)
  (let ((x1 (- cx radius))
        (x2 (+ cx radius))
        (y1 (- cy radius))
        (y2 (+ cy radius))
        (predicate (lambda (x y) (not (> (+ (expt (- x cx) 2) (expt (- y cy) 2)) (expt radius 2))))))
    (let ((estimated-area (estimate-integral predicate x1 x2 y1 y2 trials)))
      (/ estimated-area (expt radius 2)))))

;(estimate-pi 5 5 10 2000)

; ex 3.6
(define (rand m)
  (define (make-rand base)
    (define (generate)
      (random-in-range base (+ base 1000)))
    (define (reset new-base)
      (set! rand (make-rand new-base)))
    (define (dispatch m)
      (cond ((eq? 'generate m) (generate))
            ((eq? 'reset m) reset)
            (else (error "Unsupported Operation" m))))
    dispatch)
  ((make-rand 0) m))

(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 1000)
(rand 'generate)
(rand 'generate)
(rand 'generate)
    

  
