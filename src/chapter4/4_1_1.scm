#lang sicp

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; ex 4.1

(define (list-of-values-ltr exps env)
  (if (no-operands? exps)
      '()
      (let ((value (eval (first-operand exps) env)))
        (cons value
              (list-of-values (rest-operands exps) env)))))

(define (list-of-values-rtl exps env)
  (if (no-operands? exps)
      '()
      (let ((values (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              values))))