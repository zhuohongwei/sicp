#lang sicp
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;ex 2.33
(define (map-alt p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map-alt (lambda (x) (* x x)) (list 1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 2 3))

;ex 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (cond ((null? x) 0)
                               ((not (pair? x)) 1)
                               (else (count-leaves x)))) t)))

(count-leaves (cons (list 1 2) (list 3 4)))

;ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs)) nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6)))

;ex 2.37
(define (dot-product v w)
      (accumulate + 0 (map * v w)))

(dot-product '(1 2 3) '(4 5 6))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(matrix-*-vector (list (list 0 3 5) (list 5 5 2)) (list 3 4 3))

(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose (list (list 1 2 3) (list 4 5 6)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

(matrix-*-matrix (list (list 0 3 5) (list 5 5 2)) (list (list 3 4) (list 3 -2) (list 4 -2)))

