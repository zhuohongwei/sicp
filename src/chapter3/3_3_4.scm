#lang sicp

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (after-delay delay procedure)
  (error "Not Implemented"))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value) (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire signal)
  ((wire 'set-signal!) signal))
(define (add-action! wire proc)
  ((wire 'add-action!) proc))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (if (and (= s1 1) (= s2 1))
      1
      0))

(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 1) (= s2 1)) 1)
        (else (error "Invalid signals" s1 s2))))

(define inverter-delay 1)
(define and-gate-delay 1)
(define or-gate-delay 1)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; ex 3.28

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; ex 3.29

;x || y
;!(!x && !y)

(define (or-gate-alt a1 a2 output)
  (let ((o1 (make-wire)) (o2 (make-wire)) (o3 (make-wire)))
    (inverter a1 o1)
    (inverter a2 o2)
    (and-gate o1 o2 o3)
    (inverter o3 output)
    'ok))

; ex 3.30

(define (ripple-carry-adder c-in A B S)
  (define (ripple-carry-adder-iter c-in A B S)
    (if (and (not (null? A))
             (not (null? B))
             (not (null? S)))
        (let ((c-out (make-wire)))
          (full-adder (car A) (car B) c-in (car S) c-out)
          (ripple-carry-adder-iter c-out (cdr A) (cdr B) (cdr S)))
        c-in))
  (ripple-carry-adder-iter c-in A B S))
             
  
  