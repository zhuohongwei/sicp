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

;ex 2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

;in order for fold-right and fold-left to produce the same results,
;op must be associative (x * y) * z == x * (y * z)
;for example,
(fold-right * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))

;ex 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(reverse (list 1 2 3 4))

(define (reverse-alt sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))

(reverse-alt (list 1 2 3 4))


;ex 2.40
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval a b)
  (if (> a b)
      nil
      (cons a (enumerate-interval (+ a 1) b))))

(enumerate-interval 1 10)

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                    (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 5)

;ex 2.41
(define (unique-triplets n)
  (flatmap (lambda (p)
             (map (lambda (k) (cons k p))
                  (enumerate-interval 1 (- (car p) 1))))
           (unique-pairs n)))

(unique-triplets 5)

(define (filter f seq)
  (cond ((null? seq) nil)
        ((f (car seq)) (cons (car seq) (filter f (cdr seq))))
        (else (filter f (cdr seq)))))

(define (triplets-matching-sum s n)
  (filter (lambda (t) (= (accumulate + 0 t) s)) (unique-triplets n)))

(triplets-matching-sum 10 5)

