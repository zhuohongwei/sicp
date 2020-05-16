#lang sicp
(define (make-interval a b)
  (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

;ex 2.7
(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;ex 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

;ex 2.10
(define (div-interval x y)
  (if (= (lower-bound y) (upper-bound y))
      (error "trying to divide by interval spanning zero")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

;ex 2.11
(define (negative? n)
  (< n 0))
(define (positive? n)
  (not (negative? n)))
(define (mul-interval-alt x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    ;we know that x1 < x2 and y1 < y2

    ; x1  x2  y1  y2
    ; -   -   -   -  -2 -1 -2 -1   x2*y2  x1*y1
    ; -   -   -   +  -2 -1 -2  1   x1*y2  x1*y1
    ; -   -   +   +  -2 -1  1  2   x1*y2  x2*y1
    ; -   +   -   -  -2  1 -2 -1   x2*y1  x1*y1
    ; -   +   -   +  -2  1 -2  1   min (x1*y2, x2*y1) max(x1*y1, x2*y2)
    ; -   +   +   +  -2  1  1  2   x1*y2  x2*y2
    ; +   +   -   -   1  2 -2 -1   x2*y1  x1*y2 
    ; +   +   -   +   1  2 -2  1   x2*y1  x2*y2
    ; +   +   +   +   1  2  1  2   x1*y1  x2*y2  

    (cond ((and (negative? x1) (negative? x2) (negative? y1) (negative? y2)) (make-interval (* x2 y2) (* x1 y1)))
          ((and (negative? x1) (negative? x2) (negative? y1) (positive? y2)) (make-interval (* x1 y2) (* x1 y1)))
          ((and (negative? x1) (negative? x2) (positive? y1) (positive? y2)) (make-interval (* x1 y2) (* x2 y1)))
          ((and (negative? x1) (positive? x2) (negative? y1) (negative? y2)) (make-interval (* x2 y1) (* x1 y1)))
          ((and (negative? x1) (positive? x2) (negative? y1) (positive? y2)) (make-interval (min (* x1 y2) (* x2 y1)) (min (* x1 y1) (* x2 y2))))
          ((and (negative? x1) (positive? x2) (positive? y1) (positive? y2)) (make-interval (* x1 y2) (* x2 y2)))
          ((and (positive? x1) (positive? x2) (negative? y1) (negative? y2)) (make-interval (* x2 y1) (* x1 y2)))
          ((and (positive? x1) (positive? x2) (negative? y1) (positive? y2)) (make-interval (* x2 y1) (* x2 y2)))
          ((and (positive? x1) (positive? x2) (positive? y1) (positive? y2)) (make-interval (* x1 y1) (* x2 y2))))))


;ex 2.12
(define (make-center-width c w) (make-interval (- c w) (+ c w)))
(define (center i)
(/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
(/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (* (/ p 100) c)))
    (make-center-width c w)))
(define (percent interval)
  (let ((w (width interval))
        (c (center interval)))
    (* (/ w c) 100)))
    
(define i (make-center-percent 20 10))
(percent i)


