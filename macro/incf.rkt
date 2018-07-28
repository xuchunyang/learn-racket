#lang racket/base

(provide incf)

(define-syntax incf
  (syntax-rules ()
    [(incf var) (incf var 1)]
    [(incf var x) (set! var (+ x var))]))

(module+ test
  (require rackunit)
  (define x 1)
  (check-eqv? 2 (begin (incf x) x))
  (check-eqv? 102 (begin (incf x 100) x)))
