#lang sicp

;ex 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;ex 2.27
(define (deep-reverse x)
  (define (deep-reverse-iter x result)
    (cond ((null? x) result)
          ((not (pair? x)) x)
          (else (deep-reverse-iter (cdr x) (cons (deep-reverse-iter (car x) nil) result)))))
  (deep-reverse-iter x nil))

(define z (list (list 1 2) (list 3 4)))
(deep-reverse z)

;ex 2.28
(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) x)
        (else (cons (fringe (car x)) (fringe (cdr x))))))

(fringe z)

;ex 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-weight structure)
  (cond ((null? structure) 0)
        ((not (pair? structure)) structure)
        (else (+ (total-weight (branch-structure (left-branch structure))) (total-weight (branch-structure (right-branch structure)))))))

(define (branch-torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? structure)
  (cond ((null? structure) #t)
        ((not (pair? structure)) #t)
        (else (and (= (branch-torque (left-branch structure)) (branch-torque (right-branch structure)))
                   (balanced? (branch-structure (left-branch structure)))
                   (balanced? (branch-structure (right-branch structure)))))))

;ex 2.30
(define (square-tree list)
  (cond ((null? list) nil)
        ((not (pair? list)) (* list list))
        (else (cons (square-tree (car list)) (square-tree (cdr list))))))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree-alt tree)
  (map (lambda (tree)
         (cond ((not (pair? tree)) (* tree tree))
               (else (square-tree-alt tree))))
       tree))
  
(square-tree-alt (list 1 (list 2 (list 3 4) 5) (list 6 7)))
  
;ex 2.31
(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map f (car tree)) (tree-map f (cdr tree))))))


(tree-map (lambda (n) (* n n)) (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;ex 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset)) rest)))))

(subsets (list 1 2 3))

        
