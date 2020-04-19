#lang sicp
(#%require sicp-pict)

;ex 2.44
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit (load-painter "mask.png") 3) #:width 400 #:height 400)

;ex 2.45
(define (split first second)
  (lambda (painter)
    (first painter (second painter painter))))

(define right-split-alt (split beside below))
(define up-split-alt (split below beside))

(paint (right-split-alt einstein))
(paint (up-split-alt einstein))

;ex 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (op-vect op)
  (lambda (v1 v2)
    (make-vect (op (xcor-vect v1) (xcor-vect v2)) (op (ycor-vect v1) (ycor-vect v2)))))

(define add-vect (op-vect +))

(add-vect (make-vect 1 2) (make-vect 3 4))

(define sub-vect (op-vect -))

(sub-vect (make-vect 1 2) (make-vect 3 4))

(define (scale-vect s v)
  ((op-vect *) (make-vect s s) v))

(scale-vect 2 (make-vect 3 4))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define some-frame (make-frame (make-vect 0 0) (make-vect 200 0) (make-vect 0 200)))

(origin-frame some-frame)
(edge1-frame some-frame)
(edge2-frame some-frame)
  
(define (make-frame-alt origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define other-frame (make-frame-alt (make-vect 0 0) (make-vect 600 0) (make-vect 0 600)))

(define origin-frame-alt origin-frame)
(define edge1-frame-alt edge1-frame)
(define (edge2-frame-alt f)
  (cddr f))

(origin-frame-alt other-frame)
(edge1-frame-alt other-frame)
(edge2-frame-alt other-frame)
