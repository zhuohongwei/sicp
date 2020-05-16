#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; ex 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(union-set '(1 2 3 4 5) '(3 4 5 6 7))

; ex 2.60
(define (adjoin-set-alt x set)
  (cons x set))

(define (union-set-alt set1 set2)
  (append set1 set2))

; ex 2.61
(define (adjoin-set-ordered x set)
  (if (or (null? set) (< x (car set)))
      (cons x set)
      (cons (car set) (adjoin-set-ordered x (cdr set)))))

(adjoin-set-ordered 5 '(1 2 3 4 5 7 8 10))

; ex 2.62
(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-set-ordered (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set-ordered (cdr set1) set2)))
        ((> (car set1) (car set2)) (cons (car set2) (union-set-ordered set1 (cdr set2))))))

(union-set-ordered '(1 2 3 4 5) '(3 4 5 6 7 8))

; ex 2.65
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define some-tree (make-tree 7
                             (make-tree 3
                                        (make-tree 1 '() '())
                                        (make-tree 5 '() '()))
                             (make-tree 9
                                        '()
                                        (make-tree 11 '() '()))))

(define another-tree (make-tree 9
                             (make-tree 3
                                        (make-tree 1 '() '())
                                        (make-tree 5 '() '()))
                             (make-tree 11
                                        (make-tree 10 '() '())
                                        (make-tree 12 '() '()))))

(tree->list-1 some-tree)
(tree->list-2 some-tree)

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))


(define (binary-tree-op ordered-list-operator tree1 tree2)
    (let ((list1 (tree->list-2 tree1))
          (list2 (tree->list-2 tree2)))
      (let ((ordered-list-result (ordered-list-operator list1 list2)))
        (list->tree ordered-list-result))))

(define union-set-binary
  (lambda (tree1 tree2) (binary-tree-op union-set-ordered tree1 tree2)))
  
(tree->list-2 (union-set-binary some-tree another-tree))

(define intersection-set-binary
  (lambda (tree1 tree2) (binary-tree-op intersection-set-ordered tree1 tree2)))

(tree->list-2 (intersection-set-binary some-tree another-tree))

; ex 2.66
(define (lookup key records)
  (cond ((null? records) false)
        ((= key (entry records)) (entry records))
        ((> key (entry records)) (lookup key (right-branch records)))
        ((< key (entry records)) (lookup key (left-branch records)))))