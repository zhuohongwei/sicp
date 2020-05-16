#lang sicp

;ex 2.2
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (midpoint first second)
  (make-point (/ (+ (x-point first) (x-point second)) 2) (/ (+ (y-point first) (y-point second)) 2)))
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (midpoint-segment segment)
  (midpoint (start-segment segment) (end-segment segment)))

(define (print-point point)
  (newline)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (display ")"))

(define some-segment (make-segment (make-point 0 0) (make-point 8 8)))
(print-point (midpoint-segment some-segment))

;ex 2.3

;representation 1
(define (make-rectangle width height) (cons width height))
(define (width-rectangle rectangle) (car rectangle))
(define (height-rectangle rectangle) (cdr rectangle))

;representation 2
(define (make-rectangle-alt top-left bottom-right) (cons top-left bottom-right))
(define (width-rectangle-alt rectangle)
  (let ((top-left (car rectangle))
        (bottom-right (cdr rectangle)))
    (abs (- (x-point bottom-right) (x-point top-left)))))

(define (height-rectangle-alt rectangle)
  (let ((top-left (car rectangle))
        (bottom-right (cdr rectangle)))
    (abs (- (y-point bottom-right) (y-point top-left)))))

 
(define (area-rectangle rectangle width-function height-function) (* (width-function rectangle) (height-function rectangle)))
(define (perimeter-rectangle rectangle width-function height-function) (* (+ (width-function rectangle) (height-function rectangle)) 2))  

(define rectangle-one (make-rectangle 10 15))
(define rectangle-two (make-rectangle-alt (make-point 0 15) (make-point 10 0)))

(newline)
(display (area-rectangle rectangle-one width-rectangle height-rectangle))
(newline)
(display (perimeter-rectangle rectangle-one width-rectangle height-rectangle))

(newline)
(display (area-rectangle rectangle-two width-rectangle-alt height-rectangle-alt))
(newline)
(display (perimeter-rectangle rectangle-two width-rectangle-alt height-rectangle-alt))