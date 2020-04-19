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

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                        (scale-vect (ycor-vect v) (edge2-frame frame))))))

;ex 2.48

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (print-vector v)
  (display "(")
  (display (xcor-vect v))
  (display ", ")
  (display (ycor-vect v))
  (display ")"))

(define (draw-line from to)
  (display "line from ")
  (display from)
  (display " to ")
  (display to)
  (newline))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line ((frame-coord-map frame)
                   (start-segment segment))
                  ((frame-coord-map frame)
                   (end-segment segment))))
     segment-list)))

;ex 2.49
;a
(define outline-frame
  (segments->painter (let ((bl (make-vect 0 0))
                           (br (make-vect 1 0))
                           (tl (make-vect 0 1))
                           (tr (make-vect 1 1)))
                       (list
                        (make-segment bl tl)
                        (make-segment bl br)
                        (make-segment br tr)
                        (make-segment tl tr)))))

(outline-frame some-frame)

;b
(define draw-x
  (segments->painter (let ((bl (make-vect 0 0))
                           (br (make-vect 1 0))
                           (tl (make-vect 0 1))
                           (tr (make-vect 1 1)))
                       (list
                        (make-segment bl tr)
                        (make-segment tl br)))))

(draw-x some-frame)

;c
(define draw-diamond
  (segments->painter (let ((n (make-vect 0.5 1))
                           (e (make-vect 1 0.5))
                           (s (make-vect 0.5 0))
                           (w (make-vect 0 0.5)))
                       (list
                        (make-segment n e)
                        (make-segment e s)
                        (make-segment s w)
                        (make-segment w n)))))

(draw-diamond some-frame)